# =============================================================================
# Script: 3.depurado_datos.R
# Identifica outliers en las series brutas y los reemplaza por NA antes del
# rellenado. Lee desde series/brutas y escribe en series/depurado.
#
# Criterios:
#   (1) Límites físicos: PP [0, pp_limite_superior_mm], temp [-50, 55] °C, Q >= 0.
#   (2) IQR: fuera de [Q1 - k*IQR, Q3 + k*IQR]. Si IQR=0 (zonas áridas), fallback a percentil 99.5.
#   (3) Umbral estadístico (solo PP): media muy baja → >100 mm; si no, media + factor_sd*SD.
# Reporte: por archivo/estación/criterio y CSV resumen_outliers_depurado.csv.
# =============================================================================

library(readr)

dir_series_default <- "output/output_seleccion_estaciones/series"

# -----------------------------------------------------------------------------
# Inferir tipo de variable desde el nombre del archivo
# -----------------------------------------------------------------------------
inferir_variable_archivo <- function(nombre_archivo) {
  bn <- basename(nombre_archivo)
  if (grepl("^series_pp_", bn)) return("pp")
  if (grepl("^series_t_max_", bn)) return("t_max")
  if (grepl("^series_t_min_", bn)) return("t_min")
  if (grepl("^series_q_", bn)) return("q")
  NA_character_
}

# -----------------------------------------------------------------------------
# Inferir división hidrográfica desde el nombre del archivo (el_salvador / el_teniente)
# -----------------------------------------------------------------------------
inferir_division_archivo <- function(nombre_archivo) {
  bn <- basename(nombre_archivo)
  if (grepl("salvador", bn, ignore.case = TRUE)) return("el_salvador")
  if (grepl("teniente", bn, ignore.case = TRUE)) return("el_teniente")
  NULL
}

# Parámetros por división (El Salvador: árido, detección más estricta; El Teniente: húmedo, más permisivo)
parametros_division <- list(
  el_salvador = list(k_iqr = 1.5, pp_limite_superior_mm = 150, umbral_pp_media_baja_mm = 100, factor_sd = 10),
  el_teniente = list(k_iqr = 3.0, pp_limite_superior_mm = 300, umbral_pp_media_baja_mm = 200, factor_sd = 15)
)

# -----------------------------------------------------------------------------
# Cargar serie bruta (mismo formato que en 3.rellenar_series_missForest)
# -----------------------------------------------------------------------------
cargar_serie_bruta <- function(archivo) {
  if (!file.exists(archivo)) stop("No existe el archivo: ", archivo)
  df <- read_csv(archivo, show_col_types = FALSE)
  df <- as.data.frame(df)
  nms <- names(df)
  for (par in list(c("Fecha", "fecha"), c("Year", "year"), c("Month", "month"), c("Day", "day"))) {
    if (par[1] %in% nms && !(par[2] %in% nms)) names(df)[names(df) == par[1]] <- par[2]
  }
  if (!"fecha" %in% names(df)) stop("El archivo debe tener columna 'fecha' o 'Fecha': ", archivo)
  df$fecha <- as.Date(df$fecha)
  if (!"year" %in% names(df))  df$year  <- as.integer(format(df$fecha, "%Y"))
  if (!"month" %in% names(df)) df$month <- as.integer(format(df$fecha, "%m"))
  if (!"day" %in% names(df))   df$day   <- as.integer(format(df$fecha, "%d"))
  cols_fecha <- c("fecha", "year", "month", "day")
  estaciones <- setdiff(names(df), cols_fecha)
  estaciones <- estaciones[vapply(estaciones, function(c) is.numeric(df[[c]]), logical(1))]
  list(df = df, estaciones = estaciones)
}

# -----------------------------------------------------------------------------
# Aplicar límites físicos por variable (valores fuera de rango -> NA).
# PP: [0, pp_limite_superior_mm] mm/día (en zonas áridas >150 mm/día es sospechoso).
# Retorna: list(df = df, n_por_estacion = vector con conteo por columna)
# -----------------------------------------------------------------------------
aplicar_limites_fisicos <- function(df, estaciones, variable, pp_limite_superior_mm = 150) {
  n_por_estacion <- setNames(integer(length(estaciones)), estaciones)
  for (col in estaciones) {
    x <- df[[col]]
    na_antes <- is.na(x)
    if (variable == "pp") {
      x[x < 0 | x > pp_limite_superior_mm] <- NA
    } else if (variable == "q") {
      x[x < 0] <- NA
    } else if (variable %in% c("t_max", "t_min")) {
      x[x < -50 | x > 55] <- NA
    }
    df[[col]] <- x
    n_por_estacion[col] <- sum(!na_antes & is.na(x))
  }
  list(df = df, n_por_estacion = n_por_estacion)
}

# -----------------------------------------------------------------------------
# Marcar como NA los valores fuera de [Q1 - k*IQR, Q3 + k*IQR] por columna.
# Cuando IQR=0 (común en PP en zonas áridas con mediana 0): fallback a percentil 99.5
# como límite superior para detectar valores extremos.
# Retorna: list(df = df, n_por_estacion = vector con conteo por columna)
# -----------------------------------------------------------------------------
aplicar_iqr <- function(df, estaciones, k_iqr = 1.5) {
  n_por_estacion <- setNames(integer(length(estaciones)), estaciones)
  for (col in estaciones) {
    x <- df[[col]]
    na_antes <- is.na(x)
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    if (iqr != 0) {
      lim_inf <- q1 - k_iqr * iqr
      lim_sup <- q3 + k_iqr * iqr
      x[x < lim_inf | x > lim_sup] <- NA
    } else {
      # Método alternativo cuando IQR=0: marcar como outlier valores > percentil 99.5
      p995 <- quantile(x, 0.995, na.rm = TRUE)
      if (!is.na(p995) && p995 > 0) {
        x[x > p995] <- NA
      }
    }
    df[[col]] <- x
    n_por_estacion[col] <- sum(!na_antes & is.na(x))
  }
  list(df = df, n_por_estacion = n_por_estacion)
}

# -----------------------------------------------------------------------------
# Umbral estadístico: detectar valores extremos respecto a la media/SD de la estación.
# Para PP en zonas áridas (media < 2 mm): umbral absoluto 100 mm/día.
# Si no, umbral = media + factor_sd * SD. Solo aplicado a precipitación.
# Retorna: list(df = df, n_por_estacion = vector con conteo por columna)
# -----------------------------------------------------------------------------
aplicar_umbral_estadistico <- function(df, estaciones, variable,
                                      umbral_pp_media_baja_mm = 100,
                                      factor_sd = 10) {
  n_por_estacion <- setNames(integer(length(estaciones)), estaciones)
  for (col in estaciones) {
    x <- df[[col]]
    na_antes <- is.na(x)
    if (variable != "pp") {
      df[[col]] <- x
      next
    }
    media <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    if (is.na(media) || length(x[!is.na(x)]) == 0) {
      df[[col]] <- x
      next
    }
    if (media < 2 && media > 0) {
      x[x > umbral_pp_media_baja_mm] <- NA
    } else if (!is.na(sd_x) && sd_x > 0) {
      umbral <- media + factor_sd * sd_x
      x[x > umbral] <- NA
    }
    df[[col]] <- x
    n_por_estacion[col] <- sum(!na_antes & is.na(x))
  }
  list(df = df, n_por_estacion = n_por_estacion)
}

# -----------------------------------------------------------------------------
# Depurar una serie: límites físicos + IQR (con fallback si IQR=0) + opcional
# umbral estadístico. Parámetros se ajustan por división si division está definida.
# -----------------------------------------------------------------------------
depurar_una_serie <- function(archivo_bruta,
                             dir_depurado = NULL,
                             variable = NULL,
                             division = NULL,
                             usar_limites_fisicos = TRUE,
                             pp_limite_superior_mm = 150,
                             usar_iqr = TRUE,
                             k_iqr = 1.5,
                             usar_umbral_estadistico = TRUE,
                             umbral_pp_media_baja_mm = 100,
                             factor_sd = 10,
                             verbose = TRUE,
                             reportar_por_estacion = TRUE) {
  if (is.null(dir_depurado)) dir_depurado <- file.path(dirname(dirname(archivo_bruta)), "depurado")
  if (!dir.exists(dir_depurado)) dir.create(dir_depurado, recursive = TRUE)

  if (is.null(variable)) variable <- inferir_variable_archivo(archivo_bruta)
  if (is.na(variable)) stop("No se pudo inferir la variable del archivo: ", archivo_bruta)

  nombre_archivo <- basename(archivo_bruta)
  if (is.null(division)) division <- inferir_division_archivo(archivo_bruta)
  if (!is.null(division) && division %in% names(parametros_division)) {
    pars <- parametros_division[[division]]
    k_iqr <- pars$k_iqr
    pp_limite_superior_mm <- pars$pp_limite_superior_mm
    umbral_pp_media_baja_mm <- pars$umbral_pp_media_baja_mm
    factor_sd <- pars$factor_sd
    if (verbose) {
      msg_division <- if (division == "el_salvador") "División El Salvador (zona árida)" else "División El Teniente (zona húmeda)"
      message("  Aplicando parámetros para ", msg_division, " (k_iqr=", k_iqr, ", PP_max=", pp_limite_superior_mm, " mm/día)")
    }
  }

  dat <- cargar_serie_bruta(archivo_bruta)
  df <- dat$df
  estaciones <- dat$estaciones
  if (length(estaciones) == 0) stop("No se encontraron columnas numéricas de estaciones en ", archivo_bruta)

  total_celdas <- nrow(df) * length(estaciones)
  na_antes     <- sum(is.na(df[, estaciones, drop = FALSE]))
  pct_na_antes <- if (total_celdas > 0) 100 * na_antes / total_celdas else NA_real_

  n_fisicos_por_est    <- setNames(integer(length(estaciones)), estaciones)
  n_iqr_por_est        <- setNames(integer(length(estaciones)), estaciones)
  n_estadistico_por_est <- setNames(integer(length(estaciones)), estaciones)

  if (usar_limites_fisicos) {
    res_f <- aplicar_limites_fisicos(df, estaciones, variable, pp_limite_superior_mm = pp_limite_superior_mm)
    df <- res_f$df
    n_fisicos_por_est <- res_f$n_por_estacion
  }
  if (usar_iqr) {
    res_i <- aplicar_iqr(df, estaciones, k_iqr = k_iqr)
    df <- res_i$df
    n_iqr_por_est <- res_i$n_por_estacion
  }
  if (usar_umbral_estadistico) {
    res_e <- aplicar_umbral_estadistico(df, estaciones, variable,
                                        umbral_pp_media_baja_mm = umbral_pp_media_baja_mm,
                                        factor_sd = factor_sd)
    df <- res_e$df
    n_estadistico_por_est <- res_e$n_por_estacion
  }

  n_fisicos    <- sum(n_fisicos_por_est)
  n_iqr        <- sum(n_iqr_por_est)
  n_estadistico <- sum(n_estadistico_por_est)
  n_total      <- n_fisicos + n_iqr + n_estadistico

  na_despues   <- sum(is.na(df[, estaciones, drop = FALSE]))
  pct_na_despues <- if (total_celdas > 0) 100 * na_despues / total_celdas else NA_real_

  nombre_salida <- basename(archivo_bruta)
  archivo_salida <- file.path(dir_depurado, nombre_salida)
  write_csv(df, archivo_salida)

  resumen <- data.frame(
    archivo = nombre_archivo,
    variable = variable,
    division = if (!is.null(division)) division else NA_character_,
    estacion = estaciones,
    n_outliers_limites_fisicos = as.integer(n_fisicos_por_est),
    n_outliers_iqr = as.integer(n_iqr_por_est),
    n_outliers_umbral_estadistico = as.integer(n_estadistico_por_est),
    n_outliers_total = as.integer(n_fisicos_por_est + n_iqr_por_est + n_estadistico_por_est),
    pct_na_antes = round(pct_na_antes, 2),
    pct_na_despues = round(pct_na_despues, 2),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message("")
    message("  --- ", nombre_archivo, " ---")
    message("  % NA antes del depurado:   ", round(pct_na_antes, 2), "%  (", na_antes, " / ", total_celdas, " celdas)")
    message("  % NA después del depurado: ", round(pct_na_despues, 2), "%  (", na_despues, " / ", total_celdas, " celdas)")
    message("  Total de valores marcados como NA (outliers): ", n_total)
    if (usar_limites_fisicos) message("    - Por límites físicos: ", n_fisicos, if (variable == "pp") paste0(" (PP > ", pp_limite_superior_mm, " mm/día o < 0)") else "")
    if (usar_iqr) message("    - Por IQR (k = ", k_iqr, ") o percentil 99.5 si IQR=0: ", n_iqr)
    if (usar_umbral_estadistico) message("    - Por umbral estadístico (PP): ", n_estadistico)
    if (reportar_por_estacion && n_total > 0) {
      detalle <- resumen[resumen$n_outliers_total > 0, ]
      if (nrow(detalle) > 0) {
        message("  Por estación (solo estaciones con al menos 1 outlier):")
        for (i in seq_len(nrow(detalle))) {
          partes <- character(0)
          if (usar_limites_fisicos && detalle$n_outliers_limites_fisicos[i] > 0)
            partes <- c(partes, paste0("físicos: ", detalle$n_outliers_limites_fisicos[i]))
          if (usar_iqr && detalle$n_outliers_iqr[i] > 0)
            partes <- c(partes, paste0("IQR: ", detalle$n_outliers_iqr[i]))
          if (usar_umbral_estadistico && detalle$n_outliers_umbral_estadistico[i] > 0)
            partes <- c(partes, paste0("umbral_est: ", detalle$n_outliers_umbral_estadistico[i]))
          detalle_crit <- if (length(partes) > 0) paste0(" (", paste(partes, collapse = ", "), ")") else ""
          message("    ", detalle$estacion[i], ": ", detalle$n_outliers_total[i], " total", detalle_crit)
        }
      }
    }
  }

  invisible(list(df = df, n_outliers = n_total, archivo_salida = archivo_salida, resumen = resumen))
}

# -----------------------------------------------------------------------------
# Ejecutar depuración para todas las series (pp, temp, q × divisiones).
# Misma convención de nombres que 2.obtener_series_brutas y 3.rellenar_series_missForest.
# Si guardar_reporte = TRUE, escribe resumen_outliers_depurado.csv en dir_depurado.
# -----------------------------------------------------------------------------
ejecutar_depurado_datos <- function(dir_brutas   = file.path(dir_series_default, "brutas"),
                                   dir_depurado = file.path(dir_series_default, "depurado"),
                                   calidad_por_variable, # Definido en MAIN.R — no redefinir aquí
                                   ano_inicio,
                                   ano_fin,
                                   divisiones = c("el_teniente", "el_salvador"),
                                   usar_limites_fisicos = TRUE,
                                   pp_limite_superior_mm = 150,
                                   usar_iqr = TRUE,
                                   k_iqr = 1.5,
                                   usar_umbral_estadistico = TRUE,
                                   umbral_pp_media_baja_mm = 100,
                                   factor_sd = 10,
                                   verbose = TRUE,
                                   reportar_por_estacion = TRUE,
                                   guardar_reporte = TRUE) {
  if (!dir.exists(dir_brutas)) stop("No existe la carpeta de series brutas: ", dir_brutas)
  if (!dir.exists(dir_depurado)) dir.create(dir_depurado, recursive = TRUE)

  if (verbose) {
    message("")
    message("========== DEPURACIÓN DE OUTLIERS (series brutas -> depurado) ==========")
    message("Parámetros adaptados por división: El Salvador (árido, estricto) | El Teniente (húmedo, permisivo).")
    criterios <- character(0)
    if (usar_limites_fisicos) criterios <- c(criterios, "límites físicos (PP; temp; Q>=0)")
    if (usar_iqr) criterios <- c(criterios, "IQR o P99.5 si IQR=0")
    if (usar_umbral_estadistico) criterios <- c(criterios, "umbral estadístico (solo PP)")
    message("Criterios: ", paste(criterios, collapse = "; "))
    message("")
  }

  listas_resumen <- list()
  n_archivos <- 0L

  for (var in c("pp", "temp", "q")) {
    cal <- calidad_por_variable[var]
    if (is.na(cal)) next
    for (div in divisiones) {
      div_archivo <- if (div == "el_teniente") "teniente" else "salvador"
      if (var == "pp") {
        archivo_bruta <- file.path(dir_brutas, paste0("series_pp_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        if (file.exists(archivo_bruta)) {
          out <- depurar_una_serie(archivo_bruta, dir_depurado, variable = "pp", division = div,
                                   usar_limites_fisicos = usar_limites_fisicos,
                                   pp_limite_superior_mm = pp_limite_superior_mm,
                                   usar_iqr = usar_iqr, k_iqr = k_iqr,
                                   usar_umbral_estadistico = usar_umbral_estadistico,
                                   umbral_pp_media_baja_mm = umbral_pp_media_baja_mm, factor_sd = factor_sd,
                                   verbose = verbose, reportar_por_estacion = reportar_por_estacion)
          listas_resumen[[length(listas_resumen) + 1L]] <- out$resumen
          n_archivos <- n_archivos + 1L
        }
      } else if (var == "temp") {
        for (serie in c("t_max", "t_min")) {
          archivo_bruta <- file.path(dir_brutas, paste0("series_", serie, "_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
          if (file.exists(archivo_bruta)) {
            out <- depurar_una_serie(archivo_bruta, dir_depurado, variable = serie, division = div,
                                     usar_limites_fisicos = usar_limites_fisicos,
                                     pp_limite_superior_mm = pp_limite_superior_mm,
                                     usar_iqr = usar_iqr, k_iqr = k_iqr,
                                     usar_umbral_estadistico = usar_umbral_estadistico,
                                     umbral_pp_media_baja_mm = umbral_pp_media_baja_mm, factor_sd = factor_sd,
                                     verbose = verbose, reportar_por_estacion = reportar_por_estacion)
            listas_resumen[[length(listas_resumen) + 1L]] <- out$resumen
            n_archivos <- n_archivos + 1L
          }
        }
      } else {
        archivo_bruta <- file.path(dir_brutas, paste0("series_q_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        if (file.exists(archivo_bruta)) {
          out <- depurar_una_serie(archivo_bruta, dir_depurado, variable = "q", division = div,
                                   usar_limites_fisicos = usar_limites_fisicos,
                                   pp_limite_superior_mm = pp_limite_superior_mm,
                                   usar_iqr = usar_iqr, k_iqr = k_iqr,
                                   usar_umbral_estadistico = usar_umbral_estadistico,
                                   umbral_pp_media_baja_mm = umbral_pp_media_baja_mm, factor_sd = factor_sd,
                                   verbose = verbose, reportar_por_estacion = reportar_por_estacion)
          listas_resumen[[length(listas_resumen) + 1L]] <- out$resumen
          n_archivos <- n_archivos + 1L
        }
      }
    }
  }

  # Unir todos los resúmenes y guardar CSV indicativo
  if (guardar_reporte && length(listas_resumen) > 0) {
    resumen_global <- do.call(rbind, listas_resumen)
    archivo_reporte <- file.path(dir_depurado, "resumen_outliers_depurado.csv")
    write.csv(resumen_global, archivo_reporte, row.names = FALSE)
    if (verbose) {
      total_outliers <- sum(resumen_global$n_outliers_total, na.rm = TRUE)
      message("")
      message("---------- Resumen global ----------")
      message("  Archivos procesados: ", n_archivos)
      message("  Total de valores eliminados (marcados como NA): ", total_outliers)
      message("  Detalle por archivo y estación guardado en: ", archivo_reporte)
      message("==========================================")
    }
  }

  if (verbose) message("Series depuradas guardadas en: ", dir_depurado)
  invisible(dir_depurado)
}

# -----------------------------------------------------------------------------
# Uso desde MAIN.R (después del paso 2, antes del paso 3):
#   source("scripts/MAIN/3.depurado_datos.R")
#   ejecutar_depurado_datos(
#     calidad_por_variable = calidad_por_variable,
#     ano_inicio = ano_inicio, ano_fin = ano_fin,
#     pp_limite_superior_mm = 150,   # opcional: límite superior PP (mm/día)
#     k_iqr = 1.5                    # opcional: más conservador con k_iqr = 3
#   )
#   ejecutar_rellenado_missForest(
#     dir_series_brutas = "output/output_seleccion_estaciones/series/depurado", ...
#   )
# -----------------------------------------------------------------------------
