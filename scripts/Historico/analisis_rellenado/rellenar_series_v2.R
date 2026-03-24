# =============================================================================
# Script: rellenar_series_v2.R
# Rellenado de series brutas — Pasos 1 y 2, metodología homologada.
#
# Paso 1: Cálculo de cumplimiento mensual (referencia; el relleno es a nivel diario).
# Paso 2: RL/IDW según variable; rellena todos los días con NA donde hay modelo/candidatos (no solo meses <70%).
# Paso 3: Rellenar con promedio del mes x del año anterior (y-1) y siguiente (y+1).
# Paso 4: Rellenar con datos del mes x del año anterior (y-1).
# Paso 5: (omitido) CR2Met.
# Paso 6: Rellenar restantes con mediana histórica del día-del-año (+ ruido opcional).
#
# Si dir_salida está definido, se inspecciona si ya existe _rellena_paso1_2.csv y se omite el relleno (omitir_si_ya_rellena = TRUE).
# Uso: rellenar_series_paso1_paso2(archivo_bruta, archivo_metadata, variable, ...)
# =============================================================================

library(readr)
library(lubridate)
library(dplyr)
library(sf)

# -----------------------------------------------------------------------------
# Utilidades: mensajes verbose
# -----------------------------------------------------------------------------
verbose <- function(..., nivel = 1) {
  prefijo <- paste(rep("  ", nivel - 1), collapse = "")
  cat(prefijo, paste(..., collapse = ""), "\n")
}
sep_line <- function(char = "-", n = 60) cat(paste(rep(char, n), collapse = ""), "\n")

# -----------------------------------------------------------------------------
# 1) Cargar serie bruta y metadata; detectar columnas de estaciones
# -----------------------------------------------------------------------------
cargar_serie_y_metadata <- function(archivo_bruta, archivo_metadata) {
  verbose("Leyendo serie bruta:", archivo_bruta, nivel = 1)
  df <- read_csv(archivo_bruta, show_col_types = FALSE)
  df <- as.data.frame(df)
  # Columnas de fecha (aceptar mayúsculas)
  nms <- names(df)
  for (par in list(c("Fecha", "fecha"), c("Year", "year"), c("Month", "month"), c("Day", "day"))) {
    if (par[1] %in% nms && !(par[2] %in% nms)) names(df)[names(df) == par[1]] <- par[2]
  }
  if (!"fecha" %in% names(df)) stop("El CSV debe tener columna 'fecha' o 'Fecha'.")
  df$fecha <- as.Date(df$fecha)
  if (!"year" %in% names(df))  df$year  <- year(df$fecha)
  if (!"month" %in% names(df)) df$month <- month(df$fecha)
  if (!"day" %in% names(df))   df$day   <- day(df$fecha)

  cols_fecha <- c("fecha", "year", "month", "day")
  nms_df <- names(df)
  # Normalizar nombres de columnas de estación: readr puede convertir "320028" -> "X320028"
  # Unificamos a codigo_nacional leyendo primero la metadata
  verbose("Leyendo metadata:", archivo_metadata, nivel = 1)
  meta <- read_csv(archivo_metadata, show_col_types = FALSE)
  meta <- as.data.frame(meta)
  meta$codigo_nacional <- as.character(meta$codigo_nacional)
  if (!"codigo_nacional" %in% names(meta)) stop("Metadata debe tener columna 'codigo_nacional'.")
  # Unificar convención: quitar "X" inicial en codigo_nacional (p. ej. X320028 -> 320028)
  # Así coinciden con columnas que readr convierte de "320028" a "X320028" y luego normalizamos en df
  meta$codigo_nacional <- ifelse(
    substr(meta$codigo_nacional, 1L, 1L) == "X" & nchar(meta$codigo_nacional) > 1L,
    substr(meta$codigo_nacional, 2L, nchar(meta$codigo_nacional)),
    meta$codigo_nacional
  )
  codigos <- meta$codigo_nacional

  # Normalizar nombres de columnas del data frame al mismo convenio (sin "X")
  for (i in seq_along(nms_df)) {
    n <- nms_df[i]
    if (n %in% cols_fecha) next
    if (n %in% codigos) next
    if (substr(n, 1L, 1L) == "X" && substr(n, 2L, nchar(n)) %in% codigos) {
      names(df)[i] <- substr(n, 2L, nchar(n))
      nms_df[i] <- substr(n, 2L, nchar(n))
    }
  }

  estaciones <- setdiff(names(df), cols_fecha)
  # Solo columnas numéricas (datos de estación)
  estaciones <- estaciones[vapply(estaciones, function(c) is.numeric(df[[c]]), logical(1))]
  if (length(estaciones) == 0) stop("No se encontraron columnas numéricas de estaciones.")

  # Restringir a estaciones que están en serie y en metadata (misma convención sin "X")
  estaciones <- intersect(estaciones, codigos)
  if (length(estaciones) == 0) stop("Ninguna columna de la serie coincide con codigo_nacional en metadata.")
  verbose("Estaciones a procesar:", length(estaciones), nivel = 2)
  list(serie = df, meta = meta, estaciones = estaciones)
}

# -----------------------------------------------------------------------------
# 2) Paso 1: Para cada estación y cada mes-año, ¿cumple >= pct_umbral de días con dato?
# -----------------------------------------------------------------------------
calcular_cumplimiento_mensual <- function(df, estaciones, pct_umbral = 0.7) {
  verbose("Paso 1: Criterio de completitud por mes-año (", round(pct_umbral * 100), "% de días con dato)", nivel = 1)
  # Conteo de días con dato por (year, month) para todas las estaciones
  ag <- df %>%
    group_by(year, month) %>%
    summarise(
      n_dias = n(),
      across(all_of(estaciones), ~ sum(!is.na(.))),
      .groups = "drop"
    )
  cumple <- data.frame(
    year = ag$year,
    month = ag$month,
    n_dias = ag$n_dias,
    stringsAsFactors = FALSE
  )
  for (est in estaciones) {
    cumple[[est]] <- ifelse(ag[[est]] / ag$n_dias >= pct_umbral, 1L, NA_integer_)
  }
  cumple$year_month <- paste(cumple$year, cumple$month, sep = "-")
  n_incumple <- sum(is.na(as.matrix(cumple[, estaciones, drop = FALSE])))
  verbose("  Mes-años evaluados:", nrow(cumple), nivel = 2)
  verbose("  Celdas (estación × mes-año) con <", round(pct_umbral * 100), "% datos:", n_incumple, "(se rellenarán en Paso 2)", nivel = 2)
  list(cumple = cumple, n_dias_mes = ag)
}

# -----------------------------------------------------------------------------
# 3) Candidatas cercanas: por distancia (temp = 2 más cercanas; pp = vecindario 100 km)
#    Retorna meta con xcoord, ycoord; para temp: Estacion_1, Estacion_2; para pp: lista de vecinos con distancias
# -----------------------------------------------------------------------------
meta_con_coords <- function(meta) {
  if (!all(c("lon", "lat", "codigo_nacional") %in% names(meta)))
    stop("Metadata debe tener 'lon', 'lat', 'codigo_nacional'.")
  pts <- st_as_sf(meta, coords = c("lon", "lat"), crs = 4326)
  pts_utm <- st_transform(pts, 32719)
  coords <- st_coordinates(pts_utm)
  meta$xcoord <- coords[, "X"]
  meta$ycoord <- coords[, "Y"]
  meta
}

# Temperatura: 2 estaciones más cercanas dentro del umbral (m)
# Orden: por distancia creciente para que Estacion_1 sea la más cercana.
candidatas_por_distancia <- function(meta, umbral_m = 30000, n_candidatas = 2) {
  meta <- meta_con_coords(meta)
  out <- data.frame(
    codigo_nacional = meta$codigo_nacional,
    Estacion_1 = "", Estacion_2 = "",
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(meta))) {
    xc <- meta$xcoord[i]; yc <- meta$ycoord[i]; cod <- meta$codigo_nacional[i]
    d <- sqrt((meta$xcoord - xc)^2 + (meta$ycoord - yc)^2)
    mask <- meta$codigo_nacional != cod & d <= umbral_m
    vecinos <- meta[mask, ]
    dist_vec <- d[mask]
    od <- order(dist_vec)
    vecinos <- vecinos[od, ]
    vecinos <- head(vecinos, n_candidatas)
    if (nrow(vecinos) >= 1) out$Estacion_1[out$codigo_nacional == cod] <- vecinos$codigo_nacional[1]
    if (nrow(vecinos) >= 2) out$Estacion_2[out$codigo_nacional == cod] <- vecinos$codigo_nacional[2]
  }
  out
}

# Precipitación: vecindario radio_m (100 km), todas las estaciones dentro del radio (para IDW; se exige ≥ n_min)
candidatas_pp_vecindario <- function(meta, radio_m = 100000, n_min = 3) {
  meta <- meta_con_coords(meta)
  out <- list(
    meta = meta,
    vecinos = list(),
    distancias = list()
  )
  for (i in seq_len(nrow(meta))) {
    xc <- meta$xcoord[i]; yc <- meta$ycoord[i]; cod <- meta$codigo_nacional[i]
    d <- sqrt((meta$xcoord - xc)^2 + (meta$ycoord - yc)^2)
    vecinos <- meta[meta$codigo_nacional != cod & d <= radio_m & d > 0, ]
    dist_vec <- d[meta$codigo_nacional != cod & d <= radio_m & d > 0]
    od <- order(dist_vec)
    out$vecinos[[cod]] <- vecinos$codigo_nacional[od]
    out$distancias[[cod]] <- dist_vec[od]
  }
  out$n_min <- n_min
  out
}

candidatas_por_cuenca <- function(meta, n_candidatas = 2) {
  if (!all(c("codigo_nacional", "cod_ssubc", "cod_subc", "cod_cuen") %in% names(meta)))
    stop("Metadata de caudal debe tener 'codigo_nacional', 'cod_ssubc', 'cod_subc', 'cod_cuen'.")
  out <- data.frame(
    codigo_nacional = meta$codigo_nacional,
    Estacion_1 = "",
    Estacion_2 = "",
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(meta))) {
    cod <- meta$codigo_nacional[i]
    ssubc <- meta$cod_ssubc[i]; subc <- meta$cod_subc[i]; cuen <- meta$cod_cuen[i]
    otros <- meta[meta$codigo_nacional != cod, ]
    cand <- otros[otros$cod_ssubc == ssubc, ]
    if (nrow(cand) < n_candidatas) {
      en_subc <- otros[otros$cod_subc == subc & !otros$codigo_nacional %in% cand$codigo_nacional, ]
      cand <- rbind(cand, en_subc)
    }
    if (nrow(cand) < n_candidatas) {
      en_cuen <- otros[otros$cod_cuen == cuen & !otros$codigo_nacional %in% cand$codigo_nacional, ]
      cand <- rbind(cand, en_cuen)
    }
    cand <- head(cand, n_candidatas)
    if (nrow(cand) >= 1) out$Estacion_1[out$codigo_nacional == cod] <- cand$codigo_nacional[1]
    if (nrow(cand) >= 2) out$Estacion_2[out$codigo_nacional == cod] <- cand$codigo_nacional[2]
  }
  out
}

# -----------------------------------------------------------------------------
# 4a) Paso 2 Temperatura: correlación por mes; si R² > 0,6 → RL + ruido
#     Rellena a nivel diario: todos los NA donde las candidatas tienen dato (sin umbral 70% por mes).
# -----------------------------------------------------------------------------
rellenar_paso2_temp <- function(df_serie, cumple, estaciones, candidatas, r2_min = 0.6, n_candidatas = 2) {
  verbose("Paso 2 (Temperatura): RL con ruido; R² por mes > ", r2_min, "; relleno diario (todos los NA)", nivel = 1)
  df <- as.data.frame(df_serie)
  rellenados_total <- 0
  set.seed(12345)
  for (est in estaciones) {
    e1 <- candidatas$Estacion_1[candidatas$codigo_nacional == est]
    e2 <- candidatas$Estacion_2[candidatas$codigo_nacional == est]
    cand <- c(e1, e2); cand <- cand[!is.na(cand) & cand != ""]
    cand <- head(unique(cand), n_candidatas)
    if (length(cand) == 0) next
    modelos_mes <- vector("list", 12)
    for (mes in 1:12) {
      filas_mes <- df$month == mes
      y_mes <- df[filas_mes, est]
      x_mes <- as.matrix(df[filas_mes, cand, drop = FALSE])
      ok <- complete.cases(y_mes, x_mes)
      if (sum(ok) < 5) { modelos_mes[[mes]] <- list(r2 = 0); next }
      dat_mes <- data.frame(Y = y_mes[ok], x_mes[ok, , drop = FALSE], check.names = FALSE)
      colnames(dat_mes)[-1] <- cand
      fit <- tryCatch(lm(Y ~ ., data = dat_mes), error = function(e) NULL)
      if (is.null(fit)) { modelos_mes[[mes]] <- list(r2 = 0); next }
      r2 <- suppressWarnings(summary(fit)$r.squared)
      s1 <- sd(y_mes[ok], na.rm = TRUE)
      if (is.na(s1) || s1 <= 0) s1 <- 1
      modelos_mes[[mes]] <- list(fit = fit, r2 = r2, s1 = s1, cand = cand)
    }
    # Rellenar todos los días con NA donde el modelo aplica y las candidatas tienen dato (por mes, sin filtrar por cumple)
    for (mes in 1:12) {
      mod <- modelos_mes[[mes]]
      if (is.null(mod$fit) || mod$r2 < r2_min) next
      filtro <- df$month == mes
      y <- df[filtro, est]
      x_mat <- as.matrix(df[filtro, mod$cand, drop = FALSE])
      na_y <- is.na(y)
      ok_x <- complete.cases(x_mat)
      a_rellenar <- na_y & ok_x
      if (!any(a_rellenar)) next
      newdata <- as.data.frame(x_mat[a_rellenar, , drop = FALSE])
      colnames(newdata) <- mod$cand
      pred_det <- predict(mod$fit, newdata = newdata)
      n_na <- sum(a_rellenar)
      ruido <- sqrt(1 - mod$r2) * rnorm(n_na) * mod$s1
      df[filtro, est][a_rellenar] <- pred_det + ruido
      rellenados_total <- rellenados_total + n_na
    }
  }
  verbose("  Celdas rellenadas (RL + ruido):", rellenados_total, nivel = 2)
  df
}

# -----------------------------------------------------------------------------
# 4b) Paso 2 Precipitación: IDW (p=2) en vecindario; ≥3 estaciones; + ruido
#     P = (Σ d_i^(-p) P_i)/(Σ d_i^(-p)) + √(1-R²_avg)·δ·S (opción aditiva)
#     La precipitación no puede ser negativa: se trunca a >= 0 tras sumar el ruido.
# -----------------------------------------------------------------------------
rellenar_paso2_pp <- function(df_serie, cumple, estaciones, vecindario_pp, p_idw = 2, r2_min = 0.6) {
  verbose("Paso 2 (Precipitación): IDW (p=", p_idw, ") + ruido; relleno diario (todos los NA); valores >= 0", nivel = 1)
  df <- as.data.frame(df_serie)
  meta <- vecindario_pp$meta
  n_min <- vecindario_pp$n_min
  rellenados_total <- 0
  set.seed(12345)
  # R² promedio por (estación, mes) con sus vecinos (correlación mensual histórica)
  r2_por_mes <- list()
  s_por_mes <- list()
  for (est in estaciones) {
    vecinos <- vecindario_pp$vecinos[[est]]
    if (length(vecinos) < n_min) next
    r2_por_mes[[est]] <- numeric(12)
    s_por_mes[[est]] <- numeric(12)
    for (mes in 1:12) {
      filas <- df$month == mes
      y_m <- df[filas, est]
      r2_vec <- numeric(length(vecinos))
      for (iv in seq_along(vecinos)) {
        x_m <- df[filas, vecinos[iv]]
        ok <- complete.cases(y_m, x_m)
        if (sum(ok) < 5) { r2_vec[iv] <- 0; next }
        r2_vec[iv] <- suppressWarnings(summary(lm(y_m[ok] ~ x_m[ok]))$r.squared)
      }
      r2_por_mes[[est]][mes] <- mean(r2_vec, na.rm = TRUE)
      s_por_mes[[est]][mes] <- sd(y_m, na.rm = TRUE)
      if (is.na(s_por_mes[[est]][mes]) || s_por_mes[[est]][mes] <= 0) s_por_mes[[est]][mes] <- 1
    }
  }
  for (est in estaciones) {
    vecinos <- vecindario_pp$vecinos[[est]]
    distancias <- vecindario_pp$distancias[[est]]
    if (length(vecinos) < n_min) next
    r2_por_mes_est <- r2_por_mes[[est]]
    s_por_mes_est <- s_por_mes[[est]]
    # Todas las filas con NA en esta estación (relleno diario)
    filas_na <- which(is.na(df[[est]]))
    for (fila in filas_na) {
      mes <- df$month[fila]
      pesos <- (distancias)^(-p_idw)
      valores <- vapply(vecinos, function(v) df[fila, v], numeric(1))
      ok_val <- !is.na(valores)
      if (sum(ok_val) < n_min) next
      pesos_ok <- pesos[ok_val]
      valores_ok <- valores[ok_val]
      idw <- sum(pesos_ok * valores_ok, na.rm = TRUE) / sum(pesos_ok)
      r2_avg <- r2_por_mes_est[mes]
      S_m <- s_por_mes_est[mes]
      ruido <- sqrt(max(0, 1 - r2_avg)) * rnorm(1) * S_m
      df[fila, est] <- max(0, idw + ruido)
      rellenados_total <- rellenados_total + 1
    }
  }
  verbose("  Celdas rellenadas (IDW + ruido, >= 0):", rellenados_total, nivel = 2)
  df
}

# -----------------------------------------------------------------------------
# 4c) Paso 2 Caudal: misma subcuenca (prioritario); R² por mes > umbral → RL + ruido
#     Rellena a nivel diario: todos los NA donde las candidatas tienen dato (sin umbral 70% por mes).
# -----------------------------------------------------------------------------
rellenar_paso2_caudal <- function(df_serie, cumple, estaciones, candidatas, r2_min = 0.6, n_candidatas = 2) {
  verbose("Paso 2 (Caudal): RL con ruido; R² por mes > ", r2_min, "; relleno diario (todos los NA); valores >= 0", nivel = 1)
  df <- as.data.frame(df_serie)
  rellenados_total <- 0
  set.seed(12345)
  for (est in estaciones) {
    e1 <- candidatas$Estacion_1[candidatas$codigo_nacional == est]
    e2 <- candidatas$Estacion_2[candidatas$codigo_nacional == est]
    cand <- c(e1, e2); cand <- cand[!is.na(cand) & cand != ""]
    cand <- head(unique(cand), n_candidatas)
    if (length(cand) == 0) next
    modelos_mes <- vector("list", 12)
    for (mes in 1:12) {
      filas_mes <- df$month == mes
      y_mes <- df[filas_mes, est]
      x_mes <- as.matrix(df[filas_mes, cand, drop = FALSE])
      ok <- complete.cases(y_mes, x_mes)
      if (sum(ok) < 5) { modelos_mes[[mes]] <- list(r2 = 0); next }
      dat_mes <- data.frame(Y = y_mes[ok], x_mes[ok, , drop = FALSE], check.names = FALSE)
      colnames(dat_mes)[-1] <- cand
      fit <- tryCatch(lm(Y ~ ., data = dat_mes), error = function(e) NULL)
      if (is.null(fit)) { modelos_mes[[mes]] <- list(r2 = 0); next }
      r2 <- suppressWarnings(summary(fit)$r.squared)
      s1 <- sd(y_mes[ok], na.rm = TRUE)
      if (is.na(s1) || s1 <= 0) s1 <- 1
      modelos_mes[[mes]] <- list(fit = fit, r2 = r2, s1 = s1, cand = cand)
    }
    for (mes in 1:12) {
      mod <- modelos_mes[[mes]]
      if (is.null(mod$fit) || mod$r2 < r2_min) next
      filtro <- df$month == mes
      y <- df[filtro, est]
      x_mat <- as.matrix(df[filtro, mod$cand, drop = FALSE])
      na_y <- is.na(y)
      ok_x <- complete.cases(x_mat)
      a_rellenar <- na_y & ok_x
      if (!any(a_rellenar)) next
      newdata <- as.data.frame(x_mat[a_rellenar, , drop = FALSE], check.names = FALSE)
      colnames(newdata) <- mod$cand
      pred_det <- predict(mod$fit, newdata = newdata)
      n_na <- sum(a_rellenar)
      ruido <- sqrt(1 - mod$r2) * rnorm(n_na) * mod$s1
      df[filtro, est][a_rellenar] <- pmax(0, pred_det + ruido)
      rellenados_total <- rellenados_total + n_na
    }
  }
  verbose("  Celdas rellenadas (RL + ruido, >= 0):", rellenados_total, nivel = 2)
  df
}

# -----------------------------------------------------------------------------
# 4d) Auxiliar: cumplimiento actual y pendientes
#     pendientes_si_alguna_na = TRUE: pendientes = mes-años con al menos un NA (relleno diario).
#     pendientes_si_alguna_na = FALSE: pendientes = mes-años con < pct_umbral completitud (solo esos se intentan rellenar).
# -----------------------------------------------------------------------------
actualizar_cumple_y_pendientes <- function(df, estaciones, pct_umbral = 0.7, pendientes_si_alguna_na = FALSE) {
  ag <- df %>%
    group_by(year, month) %>%
    summarise(
      n_dias = n(),
      across(all_of(estaciones), ~ sum(!is.na(.))),
      .groups = "drop"
    )
  cumple <- data.frame(
    year = ag$year,
    month = ag$month,
    n_dias = ag$n_dias,
    stringsAsFactors = FALSE
  )
  for (est in estaciones) {
    cumple[[est]] <- ifelse(ag[[est]] / ag$n_dias >= pct_umbral, 1L, NA_integer_)
  }
  cumple$year_month <- paste(cumple$year, cumple$month, sep = "-")
  pendientes <- list()
  for (est in estaciones) {
    if (pendientes_si_alguna_na) {
      # Mes-años con al menos un día NA (para rellenar todos los huecos en pasos 3 y 4)
      pendientes[[est]] <- cumple$year_month[ag[[est]] < ag$n_dias]
    } else {
      idx_na <- which(is.na(cumple[[est]]))
      pendientes[[est]] <- cumple$year_month[idx_na]
    }
  }
  list(cumple = cumple, pendientes = pendientes)
}

# -----------------------------------------------------------------------------
# 4e) Paso 3: Rellenar con promedio del mes x del año anterior (y-1) y siguiente (y+1)
# -----------------------------------------------------------------------------
rellenar_paso3 <- function(df, estaciones, cumple_actual, pendientes, pct_umbral = 0.7, log_msg = verbose) {
  log_msg("Paso 3: Relleno con promedio (y-1) y (y+1) para el mismo mes", nivel = 1)
  df <- as.data.frame(df)
  anos <- sort(unique(df$year))
  if (length(anos) < 3) {
    log_msg("  Insuficientes años para paso 3; se omite.", nivel = 2)
    return(list(df = df, pendientes = pendientes))
  }
  rellenados <- 0
  pendientes_3 <- list()
  for (est in estaciones) {
    ym_list <- pendientes[[est]]
    pendientes_3[[est]] <- character(0)
    for (ym in ym_list) {
      parts <- as.integer(strsplit(ym, "-")[[1]])
      yr <- parts[1]; mes <- parts[2]
      if (yr <= anos[1] || yr >= anos[length(anos)]) {
        pendientes_3[[est]] <- c(pendientes_3[[est]], ym)
        next
      }
      yr_prev <- yr - 1; yr_next <- yr + 1
      j_prev <- which(cumple_actual$year == yr_prev & cumple_actual$month == mes)[1]
      j_next <- which(cumple_actual$year == yr_next & cumple_actual$month == mes)[1]
      if (is.na(j_prev) || is.na(j_next)) {
        pendientes_3[[est]] <- c(pendientes_3[[est]], ym)
        next
      }
      if (is.na(cumple_actual[[est]][j_prev]) || is.na(cumple_actual[[est]][j_next])) {
        pendientes_3[[est]] <- c(pendientes_3[[est]], ym)
        next
      }
      filtro <- df$year == yr & df$month == mes
      filtro_prev <- df$year == yr_prev & df$month == mes
      filtro_next <- df$year == yr_next & df$month == mes
      n_dias <- sum(filtro)
      if (n_dias == 0) next
      v_prev <- df[filtro_prev, est][seq_len(n_dias)]
      v_next <- df[filtro_next, est][seq_len(n_dias)]
      n_use <- if (mes == 2) min(28, n_dias, length(v_prev), length(v_next)) else min(n_dias, length(v_prev), length(v_next))
      if (n_use < 1) next
      v_prev <- v_prev[1:n_use]; v_next <- v_next[1:n_use]
      media <- (v_prev + v_next) / 2
      na_idx <- which(is.na(df[filtro, est][1:n_use]))
      if (length(na_idx) == 0) next
      ok_media <- !is.na(media[na_idx])
      if (!any(ok_media)) {
        pendientes_3[[est]] <- c(pendientes_3[[est]], ym)
        next
      }
      filas_est <- which(filtro)[na_idx[ok_media]]
      df[filas_est, est] <- media[na_idx][ok_media]
      # Tras rellenar, si el mes-año sigue sin cumplir, se mantiene en pendientes
      n_ok_despues <- sum(!is.na(df[filtro, est]))
      if (n_ok_despues / sum(filtro) < pct_umbral) pendientes_3[[est]] <- c(pendientes_3[[est]], ym)
      rellenados <- rellenados + sum(ok_media)
    }
  }
  log_msg("  Celdas rellenadas (promedio y-1 / y+1):", rellenados, nivel = 2)
  list(df = df, pendientes = pendientes_3)
}

# -----------------------------------------------------------------------------
# 4f) Paso 4: Rellenar con datos del mes x del año anterior (y-1)
# -----------------------------------------------------------------------------
rellenar_paso4 <- function(df, estaciones, cumple_actual, pendientes, pct_umbral = 0.7, log_msg = verbose) {
  log_msg("Paso 4: Relleno con mismo mes del año anterior (y-1)", nivel = 1)
  df <- as.data.frame(df)
  anos <- sort(unique(df$year))
  if (length(anos) < 2) {
    log_msg("  Insuficientes años para paso 4; se omite.", nivel = 2)
    return(list(df = df, pendientes = pendientes))
  }
  rellenados <- 0
  pendientes_4 <- list()
  for (est in estaciones) {
    ym_list <- pendientes[[est]]
    pendientes_4[[est]] <- character(0)
    for (ym in ym_list) {
      parts <- as.integer(strsplit(ym, "-")[[1]])
      yr <- parts[1]; mes <- parts[2]
      if (yr <= anos[1]) {
        pendientes_4[[est]] <- c(pendientes_4[[est]], ym)
        next
      }
      yr_prev <- yr - 1
      j_prev <- which(cumple_actual$year == yr_prev & cumple_actual$month == mes)[1]
      if (is.na(j_prev) || is.na(cumple_actual[[est]][j_prev])) {
        pendientes_4[[est]] <- c(pendientes_4[[est]], ym)
        next
      }
      filtro <- df$year == yr & df$month == mes
      filtro_prev <- df$year == yr_prev & df$month == mes
      n_dias <- sum(filtro)
      if (n_dias == 0) next
      v_prev <- df[filtro_prev, est][seq_len(n_dias)]
      if (mes == 2 && n_dias > 28) v_prev <- v_prev[1:28]
      n_use <- min(n_dias, length(v_prev))
      na_idx <- which(is.na(df[filtro, est][1:n_use]))
      if (length(na_idx) == 0) next
      filas_est <- which(filtro)[na_idx]
      df[filas_est, est] <- v_prev[na_idx]
      rellenados <- rellenados + length(na_idx)
      if (sum(!is.na(df[filtro, est])) / sum(filtro) < pct_umbral) pendientes_4[[est]] <- c(pendientes_4[[est]], ym)
    }
  }
  log_msg("  Celdas rellenadas (y-1):", rellenados, nivel = 2)
  list(df = df, pendientes = pendientes_4)
}

# -----------------------------------------------------------------------------
# 4g) Paso 6: Rellenar restantes con mediana del día-del-año (+ ruido estocástico)
#     T_i,j,m = Median(T_i,m) + sqrt(1-0.5)·δ·S_i,m (δ ~ N(0,1))
#     valor_minimo: cota inferior (ej. 0 para precipitación; -Inf para temp/caudal).
# -----------------------------------------------------------------------------
rellenar_paso6 <- function(df, estaciones, valor_minimo = -Inf, log_msg = verbose) {
  log_msg("Paso 6: Relleno con mediana histórica por día-del-año + ruido estocástico", nivel = 1)
  df <- as.data.frame(df)
  df$doy <- yday(df$fecha)
  rellenados <- 0
  set.seed(54321)
  for (est in estaciones) {
    na_idx <- which(is.na(df[[est]]))
    if (length(na_idx) == 0) next
    # Mediana por día del año; si un doy no tiene datos, usar mediana global de la estación
    agg_doy <- df %>% filter(!is.na(.data[[est]])) %>% group_by(doy) %>% summarise(med = median(.data[[est]], na.rm = TRUE), .groups = "drop")
    med_global <- median(df[[est]], na.rm = TRUE)
    if (is.na(med_global)) med_global <- 0
    sd_mes <- df %>% filter(!is.na(.data[[est]])) %>% group_by(month) %>% summarise(sd_m = sd(.data[[est]], na.rm = TRUE), .groups = "drop")
    for (i in na_idx) {
      doy <- df$doy[i]
      mes <- df$month[i]
      med_row <- agg_doy[agg_doy$doy == doy, ]
      med_val <- if (nrow(med_row) > 0) med_row$med[1] else med_global
      sd_row <- sd_mes[sd_mes$month == mes, ]
      s_val <- if (nrow(sd_row) > 0 && !is.na(sd_row$sd_m[1]) && sd_row$sd_m[1] > 0) sd_row$sd_m[1] else 1
      ruido <- sqrt(1 - 0.5) * rnorm(1) * s_val
      # Respetar cota inferior (ej. precipitación >= 0)
      df[i, est] <- max(valor_minimo, med_val + ruido)
      rellenados <- rellenados + 1
    }
  }
  df$doy <- NULL
  log_msg("  Celdas rellenadas (mediana + ruido):", rellenados, nivel = 2)
  df
}

# -----------------------------------------------------------------------------
# 5) Función principal: Pasos 1, 2, 3, 4 y 6 (se omite paso 5 CR2Met)
# -----------------------------------------------------------------------------
# variable: "pp", "temp" o "q" (para elegir candidatas por distancia o cuenca)
# pct_umbral: umbral de completitud (ej. 0.7 = 70%)
# pasos: vector con los pasos a ejecutar, ej. c(1, 2), c(1, 2, 3, 4, 6). Si se incluye 2, el paso 1 se ejecuta siempre (es dependencia).
#
rellenar_series_paso1_paso2 <- function(archivo_bruta,
                                       archivo_metadata,
                                       variable = c("pp", "temp", "q"),
                                       dir_salida = NULL,
                                       pct_umbral = 0.7,
                                       umbral_distancia_m = 30000,
                                       n_candidatas_rl = 2,
                                       r2_min_caudal = 0.6,
                                       pasos = c(1, 2, 3, 4, 6),
                                       omitir_si_ya_rellena = TRUE,
                                       mostrar_mensajes = TRUE) {
  log_msg <- if (mostrar_mensajes) verbose else function(..., nivel = 1) invisible(NULL)
  variable <- match.arg(variable)
  # Pasos permitidos: 1, 2, 3, 4, 6 (el 5 CR2Met no está implementado)
  pasos <- unique(as.integer(pasos))
  pasos <- pasos[pasos %in% c(1L, 2L, 3L, 4L, 6L)]
  if (length(pasos) == 0) stop("pasos debe contener al menos uno de: 1, 2, 3, 4, 6.")
  # Paso 2 requiere paso 1 (cumple); se agrega 1 si no está
  if (2L %in% pasos && !1L %in% pasos) pasos <- sort(c(1L, pasos))

  # Inspección previa: si ya existe la serie rellena en dir_salida, omitir rellenado
  if (omitir_si_ya_rellena && !is.null(dir_salida) && dir_salida != "") {
    nombre_rellena <- gsub("_bruta\\.csv$", "_rellena_paso1_2.csv", basename(archivo_bruta))
    archivo_rellena <- file.path(dir_salida, nombre_rellena)
    if (file.exists(archivo_rellena)) {
      sep_line("=", 60)
      log_msg("RELLENADO DE SERIES — Pasos 1, 2, 3, 4 y 6", nivel = 1)
      log_msg("  Serie:", basename(archivo_bruta), nivel = 1)
      log_msg("  Variable:", variable, nivel = 1)
      log_msg("  [OMITIDO] Ya existe serie rellenada:", nombre_rellena, nivel = 1)
      sep_line("=", 60)
      return(invisible(list(omitido = TRUE, archivo_rellena = archivo_rellena)))
    }
  }

  sep_line("=", 60)
  log_msg("RELLENADO DE SERIES — Pasos: ", paste(pasos, collapse = ", "), nivel = 1)
  log_msg("  Serie:", basename(archivo_bruta), nivel = 1)
  log_msg("  Variable:", variable, " | Umbral completitud:", round(pct_umbral * 100), "%", nivel = 1)
  sep_line("=", 60)

  # Cargar
  dat <- cargar_serie_y_metadata(archivo_bruta, archivo_metadata)
  df <- dat$serie
  meta <- dat$meta
  estaciones <- dat$estaciones
  log_msg("", nivel = 1)

  # Conteo explícito de missing values ANTES del relleno
  na_antes <- sum(is.na(df[, estaciones]))
  total_celdas <- nrow(df) * length(estaciones)
  log_msg("--- Missing values ANTES del relleno ---", nivel = 1)
  log_msg("  Total de celdas (días × estaciones):", format(total_celdas, big.mark = ","), nivel = 2)
  log_msg("  Valores faltantes (NA):", format(na_antes, big.mark = ","), nivel = 2)
  log_msg("", nivel = 1)

  # Paso 1 (si está en pasos)
  cumple <- NULL
  if (1L %in% pasos) {
    paso1 <- calcular_cumplimiento_mensual(df, estaciones, pct_umbral = pct_umbral)
    cumple <- paso1$cumple
    log_msg("", nivel = 1)
  }

  # Paso 2 (requiere paso 1; candidatas según variable)
  df_relleno <- df
  candidatas <- NULL
  na_despues_p2 <- na_antes
  if (2L %in% pasos) {
    if (variable == "q") {
      log_msg("Candidatas por cuenca (misma subcuenca prioritario > subc > cuenca); R² por mes > ", r2_min_caudal, nivel = 1)
      candidatas <- candidatas_por_cuenca(meta, n_candidatas = n_candidatas_rl)
      log_msg("  Estaciones con 2 candidatas para RL:", sum(candidatas$Estacion_1 != "" & candidatas$Estacion_2 != ""), nivel = 2)
      log_msg("", nivel = 1)
      df_relleno <- rellenar_paso2_caudal(df_relleno, cumple, estaciones, candidatas, r2_min = r2_min_caudal, n_candidatas = n_candidatas_rl)
    } else if (variable == "temp") {
      log_msg("Candidatas por distancia (umbral ", umbral_distancia_m / 1000, " km); máx. 2 rellenadoras; R² por mes > 0,7", nivel = 1)
      candidatas <- candidatas_por_distancia(meta, umbral_m = umbral_distancia_m, n_candidatas = 2)
      log_msg("  Estaciones con 2 candidatas:", sum(candidatas$Estacion_1 != "" & candidatas$Estacion_2 != ""), nivel = 2)
      log_msg("", nivel = 1)
      df_relleno <- rellenar_paso2_temp(df_relleno, cumple, estaciones, candidatas, r2_min = 0.6, n_candidatas = 2)
    } else {
      radio_pp_m <- 100000
      n_min_pp <- 3
      log_msg("Precipitación: vecindario ", radio_pp_m / 1000, " km; mínimo ", n_min_pp, " estaciones; IDW (p=2) + ruido", nivel = 1)
      vecindario_pp <- candidatas_pp_vecindario(meta, radio_m = radio_pp_m, n_min = n_min_pp)
      n_con_vec <- sum(vapply(vecindario_pp$vecinos, function(v) length(v) >= n_min_pp, logical(1)))
      log_msg("  Estaciones con ≥", n_min_pp, " vecinos:", n_con_vec, nivel = 2)
      log_msg("", nivel = 1)
      df_relleno <- rellenar_paso2_pp(df_relleno, cumple, estaciones, vecindario_pp, p_idw = 2)
      candidatas <- vecindario_pp
    }
    na_despues_p2 <- sum(is.na(df_relleno[, estaciones]))
    log_msg("", nivel = 1)
    log_msg("--- Resumen tras Paso 2 ---", nivel = 1)
    log_msg("  Datos rellenados (Paso 2):", format(na_antes - na_despues_p2, big.mark = ","), nivel = 2)
    log_msg("  Faltantes que quedan:", format(na_despues_p2, big.mark = ","), nivel = 2)
    log_msg("", nivel = 1)
  }

  na_despues_p3 <- na_despues_p2
  na_despues_p4 <- na_despues_p2
  if (3L %in% pasos || 4L %in% pasos || 6L %in% pasos) {
    aux <- actualizar_cumple_y_pendientes(df_relleno, estaciones, pct_umbral = pct_umbral, pendientes_si_alguna_na = TRUE)
    cumple_actual <- aux$cumple
    pendientes <- aux$pendientes
  }

  if (3L %in% pasos) {
    log_msg("", nivel = 1)
    paso3 <- rellenar_paso3(df_relleno, estaciones, cumple_actual, pendientes, pct_umbral = pct_umbral, log_msg = log_msg)
    df_relleno <- paso3$df
    na_despues_p3 <- sum(is.na(df_relleno[, estaciones]))
    aux <- actualizar_cumple_y_pendientes(df_relleno, estaciones, pct_umbral = pct_umbral, pendientes_si_alguna_na = TRUE)
    cumple_actual <- aux$cumple
    pendientes <- aux$pendientes
  }

  if (4L %in% pasos) {
    log_msg("", nivel = 1)
    paso4 <- rellenar_paso4(df_relleno, estaciones, cumple_actual, pendientes, pct_umbral = pct_umbral, log_msg = log_msg)
    df_relleno <- paso4$df
    na_despues_p4 <- sum(is.na(df_relleno[, estaciones]))
  }

  if (6L %in% pasos) {
    log_msg("", nivel = 1)
    # Precipitación y caudal no pueden ser negativos; temperatura sí puede
    valor_min <- if (variable == "pp" || variable == "q") 0 else -Inf
    df_relleno <- rellenar_paso6(df_relleno, estaciones, valor_minimo = valor_min, log_msg = log_msg)
  }

  na_despues <- sum(is.na(df_relleno[, estaciones]))
  rellenados_total <- na_antes - na_despues
  log_msg("", nivel = 1)
  log_msg("--- Resumen final de relleno ---", nivel = 1)
  log_msg("  Datos rellenados en total:", format(rellenados_total, big.mark = ","), nivel = 2)
  log_msg("  Faltantes que aún quedan (NA):", format(na_despues, big.mark = ","), nivel = 2)
  log_msg("", nivel = 1)

  # Guardar si se indica directorio
  if (!is.null(dir_salida) && dir_salida != "") {
    if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)
    archivo_salida <- file.path(dir_salida, gsub("_bruta\\.csv$", "_rellena_paso1_2.csv", basename(archivo_bruta)))
    write_csv(df_relleno, archivo_salida)
    log_msg("Guardado:", archivo_salida, nivel = 1)
  }

  resumen_pasos <- c(Paso_1 = na_antes, Paso_2 = na_despues_p2, Paso_3 = na_despues_p3, Paso_4 = na_despues_p4, Paso_6 = na_despues)
  sep_line("=", 60)
  invisible(list(omitido = FALSE, serie_rellena = df_relleno, cumple = cumple, candidatas = candidatas, resumen_pasos = resumen_pasos))
}

# -----------------------------------------------------------------------------
# 6) Ejecutar rellenado para todas las variables y divisiones (llamada desde MAIN)
# -----------------------------------------------------------------------------
# calidad_por_variable: vector con nombres c("pp", "temp", "q") y valores de umbral de calidad
# divisiones: "el_teniente", "el_salvador" (o vector con ambos)
# pasos: vector de pasos a ejecutar, ej. c(1, 2) solo RL/IDW; c(1, 2, 3, 4, 6) todos (por defecto)
#
ejecutar_rellenado_series <- function(dir_series_brutas   = "output/output_seleccion_estaciones/series/brutas",
                                     dir_series_rellenas = "output/output_seleccion_estaciones/series/rellenas",
                                     dir_estaciones      = "output/output_seleccion_estaciones/estaciones",
                                     calidad_por_variable = c(pp = 60, temp = 70, q = 50),
                                     ano_inicio,
                                     ano_fin,
                                     divisiones          = c("el_teniente", "el_salvador"),
                                     pasos               = c(1, 2, 3, 4, 6),
                                     omitir_si_ya_rellena = TRUE,
                                     mostrar_mensajes    = TRUE) {
  log_msg <- if (mostrar_mensajes) verbose else function(..., nivel = 1) invisible(NULL)
  # Tablas resumen: filas = variable (pp, temp, caudal), columnas = Paso 1, 2, 3, 4, 6 (faltantes)
  vars_resumen <- c("pp", "temp", "caudal")
  pasos_resumen <- c("Paso 1", "Paso 2", "Paso 3", "Paso 4", "Paso 6")
  resumen_teniente <- matrix(NA_integer_, nrow = 3, ncol = 5, dimnames = list(vars_resumen, pasos_resumen))
  resumen_salvador <- matrix(NA_integer_, nrow = 3, ncol = 5, dimnames = list(vars_resumen, pasos_resumen))

  for (var in c("pp", "temp", "q")) {
    cal <- calidad_por_variable[var]
    if (is.na(cal)) next
    for (div in divisiones) {
      div_archivo <- if (div == "el_teniente") "teniente" else "salvador"
      fila_var <- switch(var, pp = "pp", temp = "temp", q = "caudal")
      if (var == "pp") {
        archivo_bruta    <- file.path(dir_series_brutas, paste0("series_pp_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        archivo_metadata <- file.path(dir_estaciones,    paste0("estaciones_pp_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, ".csv"))
        if (file.exists(archivo_bruta) && file.exists(archivo_metadata)) {
          res <- rellenar_series_paso1_paso2(archivo_bruta, archivo_metadata, variable = "pp", dir_salida = dir_series_rellenas,
                                             pasos = pasos, omitir_si_ya_rellena = omitir_si_ya_rellena, mostrar_mensajes = mostrar_mensajes)
          if (!is.null(res) && !identical(res$omitido, TRUE) && !is.null(res$resumen_pasos)) {
            if (div == "el_teniente") resumen_teniente[fila_var, ] <- as.integer(res$resumen_pasos)
            else resumen_salvador[fila_var, ] <- as.integer(res$resumen_pasos)
          }
        }
      } else if (var == "temp") {
        archivo_metadata <- file.path(dir_estaciones, paste0("estaciones_temp_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, ".csv"))
        for (serie in c("t_max", "t_min")) {
          archivo_bruta <- file.path(dir_series_brutas, paste0("series_", serie, "_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
          if (file.exists(archivo_bruta) && file.exists(archivo_metadata)) {
            res <- rellenar_series_paso1_paso2(archivo_bruta, archivo_metadata, variable = "temp", dir_salida = dir_series_rellenas,
                                               pasos = pasos, omitir_si_ya_rellena = omitir_si_ya_rellena, mostrar_mensajes = mostrar_mensajes)
            if (!is.null(res) && !identical(res$omitido, TRUE) && !is.null(res$resumen_pasos)) {
              if (div == "el_teniente") {
                if (any(is.na(resumen_teniente[fila_var, ]))) resumen_teniente[fila_var, ] <- as.integer(res$resumen_pasos)
                else resumen_teniente[fila_var, ] <- resumen_teniente[fila_var, ] + as.integer(res$resumen_pasos)
              } else {
                if (any(is.na(resumen_salvador[fila_var, ]))) resumen_salvador[fila_var, ] <- as.integer(res$resumen_pasos)
                else resumen_salvador[fila_var, ] <- resumen_salvador[fila_var, ] + as.integer(res$resumen_pasos)
              }
            }
          }
        }
      } else {
        archivo_bruta    <- file.path(dir_series_brutas, paste0("series_q_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        archivo_metadata <- file.path(dir_estaciones,    paste0("estaciones_caudal_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, ".csv"))
        if (file.exists(archivo_bruta) && file.exists(archivo_metadata)) {
          res <- rellenar_series_paso1_paso2(archivo_bruta, archivo_metadata, variable = "q", dir_salida = dir_series_rellenas,
                                             pasos = pasos, omitir_si_ya_rellena = omitir_si_ya_rellena, mostrar_mensajes = mostrar_mensajes)
          if (!is.null(res) && !identical(res$omitido, TRUE) && !is.null(res$resumen_pasos)) {
            if (div == "el_teniente") resumen_teniente[fila_var, ] <- as.integer(res$resumen_pasos)
            else resumen_salvador[fila_var, ] <- as.integer(res$resumen_pasos)
          }
        }
      }
    }
  }

  # Emitir tablas finales de resumen (celdas = cantidad de datos faltantes)
  log_msg("", nivel = 1)
  sep_line("=", 60)
  log_msg("RESUMEN FINAL — Datos faltantes por variable y paso (El Teniente)", nivel = 1)
  sep_line("-", 60)
  tbl_t <- as.data.frame(resumen_teniente)
  tbl_t[] <- lapply(tbl_t, function(x) if (is.integer(x)) format(x, big.mark = ",") else x)
  print(tbl_t)
  log_msg("", nivel = 1)
  log_msg("RESUMEN FINAL — Datos faltantes por variable y paso (El Salvador)", nivel = 1)
  sep_line("-", 60)
  tbl_s <- as.data.frame(resumen_salvador)
  tbl_s[] <- lapply(tbl_s, function(x) if (is.integer(x)) format(x, big.mark = ",") else x)
  print(tbl_s)
  sep_line("=", 60)

  invisible(list(resumen_el_teniente = resumen_teniente, resumen_el_salvador = resumen_salvador))
}

# -----------------------------------------------------------------------------
# Ejemplo de uso (desde MAIN o consola, con setwd en caracterizacion_historica)
# -----------------------------------------------------------------------------
#
# source("scripts/analisis_rellenado/rellenar_series_v2.R")
#
# # Desde MAIN (una sola llamada, todos los pasos):
# ejecutar_rellenado_series(
#   calidad_por_variable = c(pp = 60, temp = 70, q = 50),
#   ano_inicio = 1990,
#   ano_fin    = 2024
# )
#
# # Solo pasos 1 y 2 (RL/IDW, sin 3, 4, 6):
# ejecutar_rellenado_series(
#   calidad_por_variable = c(pp = 60, temp = 70, q = 50),
#   ano_inicio = 1990,
#   ano_fin    = 2024,
#   pasos      = c(1, 2)
# )
#
# # Una serie suelta (pp):
# rellenar_series_paso1_paso2(
#   archivo_bruta   = file.path("output/.../brutas", "series_pp_el_salvador_60_1990_2024_bruta.csv"),
#   archivo_metadata = file.path("output/.../estaciones", "estaciones_pp_el_salvador_60_1990_2024.csv"),
#   variable = "pp", dir_salida = "output/.../series/rellenas"
# )
