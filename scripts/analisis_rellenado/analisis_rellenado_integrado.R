# =============================================================================
# Script: analisis_rellenado_integrado.R
# Análisis unificado de rellenado: caudal, temperatura y precipitación.
# Combina la lógica de analisis_rellenado.R (temp/pp con periodo continuo) y
# analisis_rellenado_caudal.R (caudal sobre serie completa o rango de años).
# Genera una sola tabla final para comparar resultados de los tres métodos
# (paso1_2, imputeTS, missForest) en todas las variables.
#
# Requiere: rellenar_series_v2.R, rellenar_series_imputeTS.R, rellenar_series_missForest.R
#           comparar_series_bruta_vs_rellena.R (para gráficos de comparación)
# Uso: evaluar_rellenado_integrado(archivos_brutas, dir_estaciones, ...)
#      generar_graficos_comparacion_metodos_rellenado(...)
# =============================================================================

library(readr)
library(lubridate)
library(dplyr)

# Cargar script de comparación bruta vs rellena (función comparar_serie_estacion)
source("scripts/comparar_series_bruta_vs_rellena.R")

# -----------------------------------------------------------------------------
# Inferir ruta de metadata a partir del nombre del archivo bruto
# series_t_max_*, series_pp_*, series_q_* -> estaciones_temp_*, estaciones_pp_*, estaciones_caudal_*
# -----------------------------------------------------------------------------
inferir_metadata_path <- function(archivo_bruta, dir_estaciones) {
  bn <- basename(archivo_bruta)
  if (!grepl("_bruta\\.csv$", bn)) stop("Archivo bruta debe terminar en _bruta.csv: ", archivo_bruta)
  base <- gsub("_bruta\\.csv$", "", bn)
  if (grepl("^series_pp_", base)) {
    nombre_meta <- gsub("^series_pp_", "estaciones_pp_", base)
  } else if (grepl("^series_t_max_|^series_t_min_", base)) {
    nombre_meta <- gsub("^series_t_(max|min)_", "estaciones_temp_", base)
  } else if (grepl("^series_q_", base)) {
    nombre_meta <- gsub("^series_q_", "estaciones_caudal_", base)
  } else {
    stop("No se pudo inferir tipo de variable desde: ", bn)
  }
  file.path(dir_estaciones, paste0(nombre_meta, ".csv"))
}

# -----------------------------------------------------------------------------
# Inferir variable para rellenar_series_paso1_paso2: "pp", "temp", "q"
# -----------------------------------------------------------------------------
inferir_variable <- function(archivo_bruta) {
  bn <- basename(archivo_bruta)
  if (grepl("^series_pp_", bn)) return("pp")
  if (grepl("^series_t_max_|^series_t_min_", bn)) return("temp")
  if (grepl("^series_q_", bn)) return("q")
  stop("No se pudo inferir variable desde: ", bn)
}

# -----------------------------------------------------------------------------
# Cargar serie bruta (común para temp, pp y caudal): fecha + columnas de estaciones
# -----------------------------------------------------------------------------
cargar_serie <- function(archivo) {
  if (!file.exists(archivo)) stop("No existe: ", archivo)
  df <- read_csv(archivo, show_col_types = FALSE)
  df <- as.data.frame(df)
  nms <- names(df)
  for (par in list(c("Fecha", "fecha"), c("Year", "year"), c("Month", "month"), c("Day", "day"))) {
    if (par[1] %in% nms && !(par[2] %in% nms)) names(df)[names(df) == par[1]] <- par[2]
  }
  if (!"fecha" %in% names(df)) stop("Se requiere columna 'fecha' o 'Fecha': ", archivo)
  df$fecha <- as.Date(df$fecha)
  if (!"year" %in% names(df))  df$year  <- as.integer(year(df$fecha))
  if (!"month" %in% names(df)) df$month <- as.integer(month(df$fecha))
  if (!"day" %in% names(df))   df$day   <- as.integer(day(df$fecha))
  cols_fecha <- c("fecha", "year", "month", "day")
  cols_fecha <- intersect(cols_fecha, names(df))
  estaciones <- setdiff(names(df), cols_fecha)
  estaciones <- estaciones[vapply(estaciones, function(c) is.numeric(df[[c]]), logical(1))]
  list(df = df, estaciones = estaciones)
}

# -----------------------------------------------------------------------------
# Encontrar el periodo más largo con datos continuos (para temp y pp)
# -----------------------------------------------------------------------------
encontrar_periodo_completo <- function(df, estaciones, pct_completo = 1, min_dias = 365) {
  if (length(estaciones) == 0) stop("Sin columnas de estaciones")
  df <- df[order(df$fecha), ]
  n_est <- length(estaciones)
  if (pct_completo >= 1) {
    fila_ok <- rowSums(is.na(df[, estaciones, drop = FALSE])) == 0
  } else {
    fila_ok <- rowSums(!is.na(df[, estaciones, drop = FALSE])) / n_est >= pct_completo
  }
  runs <- rle(fila_ok)
  if (!any(runs$values)) {
    if (pct_completo >= 1 && min_dias > 0) {
      for (pct_relajado in c(0.99, 0.95, 0.9, 0.8, 0.7)) {
        fila_ok_r <- rowSums(!is.na(df[, estaciones, drop = FALSE])) / n_est >= pct_relajado
        runs_r <- rle(fila_ok_r)
        if (!any(runs_r$values)) next
        run_lengths_r <- runs_r$lengths[runs_r$values]
        run_starts_r <- cumsum(c(1L, runs_r$lengths))[which(runs_r$values)]
        idx_r <- which.max(run_lengths_r)[1]
        n_dias_r <- run_lengths_r[idx_r]
        if (n_dias_r >= min_dias) {
          inicio <- run_starts_r[idx_r]
          fin <- inicio + n_dias_r - 1L
          df_sub <- df[inicio:fin, ]
          return(list(
            df_subset = df_sub,
            fecha_inicio = df_sub$fecha[1],
            fecha_fin = df_sub$fecha[nrow(df_sub)],
            n_dias = n_dias_r,
            completo_estricto = FALSE
          ))
        }
      }
      for (pct_relajado in c(0.99, 0.95, 0.9, 0.8, 0.7)) {
        fila_ok_r <- rowSums(!is.na(df[, estaciones, drop = FALSE])) / n_est >= pct_relajado
        runs_r <- rle(fila_ok_r)
        if (!any(runs_r$values)) next
        run_lengths_r <- runs_r$lengths[runs_r$values]
        run_starts_r <- cumsum(c(1L, runs_r$lengths))[which(runs_r$values)]
        idx_r <- which.max(run_lengths_r)[1]
        n_dias_r <- run_lengths_r[idx_r]
        if (n_dias_r >= 30) {
          inicio <- run_starts_r[idx_r]
          fin <- inicio + n_dias_r - 1L
          df_sub <- df[inicio:fin, ]
          return(list(
            df_subset = df_sub,
            fecha_inicio = df_sub$fecha[1],
            fecha_fin = df_sub$fecha[nrow(df_sub)],
            n_dias = n_dias_r,
            completo_estricto = FALSE
          ))
        }
      }
    }
    return(list(df_subset = df[0, ], fecha_inicio = NA, fecha_fin = NA, n_dias = 0L, completo_estricto = FALSE))
  }
  run_lengths <- runs$lengths[runs$values]
  run_starts <- cumsum(c(1L, runs$lengths))[which(runs$values)]
  idx_mejor <- which.max(run_lengths)[1]
  inicio <- run_starts[idx_mejor]
  fin <- inicio + run_lengths[idx_mejor] - 1L
  n_dias <- run_lengths[idx_mejor]
  completo_estricto <- (pct_completo >= 1)
  if (pct_completo >= 1 && n_dias < min_dias) {
    for (pct_relajado in c(0.99, 0.95, 0.9, 0.8)) {
      fila_ok_r <- rowSums(!is.na(df[, estaciones, drop = FALSE])) / n_est >= pct_relajado
      runs_r <- rle(fila_ok_r)
      if (!any(runs_r$values)) next
      run_lengths_r <- runs_r$lengths[runs_r$values]
      run_starts_r <- cumsum(c(1L, runs_r$lengths))[which(runs_r$values)]
      idx_r <- which.max(run_lengths_r)[1]
      if (run_lengths_r[idx_r] >= min_dias) {
        inicio <- run_starts_r[idx_r]
        fin <- inicio + run_lengths_r[idx_r] - 1L
        n_dias <- run_lengths_r[idx_r]
        completo_estricto <- FALSE
        break
      }
    }
  }
  df_sub <- df[inicio:fin, ]
  list(
    df_subset = df_sub,
    fecha_inicio = df_sub$fecha[1],
    fecha_fin = df_sub$fecha[nrow(df_sub)],
    n_dias = n_dias,
    completo_estricto = completo_estricto
  )
}

# -----------------------------------------------------------------------------
# Introducir NA artificial en pct_eliminar de las celdas con dato
# -----------------------------------------------------------------------------
introducir_na_artificial <- function(df, estaciones, pct_eliminar = 0.1, semilla = 42) {
  set.seed(semilla)
  df_mask <- as.data.frame(df)
  n_filas <- nrow(df_mask)
  n_cols <- length(estaciones)
  posiciones <- which(!is.na(as.matrix(df_mask[, estaciones])))
  n_disponibles <- length(posiciones)
  if (n_disponibles == 0) stop("No hay celdas con dato para eliminar")
  n_eliminar <- max(1, min(round(n_disponibles * pct_eliminar), n_disponibles))
  elegidos <- sample(posiciones, size = n_eliminar, replace = FALSE)
  mascara_mat <- matrix(FALSE, nrow = n_filas, ncol = n_cols)
  mascara_mat[elegidos] <- TRUE
  for (j in seq_along(estaciones)) {
    df_mask[[estaciones[j]]][mascara_mat[, j]] <- NA
  }
  list(df_mask = df_mask, mascara = mascara_mat, n_eliminados = n_eliminar)
}

# -----------------------------------------------------------------------------
# Métricas: comparar imputado vs verdadero en celdas enmascaradas (común)
# -----------------------------------------------------------------------------
metricas_imputacion <- function(ref_df, filled_df, estaciones, mascara) {
  cols_filled <- names(filled_df)
  use_est <- character(length(estaciones))
  for (k in seq_along(estaciones)) {
    e <- estaciones[k]
    if (e %in% cols_filled) {
      use_est[k] <- e
    } else if (paste0("X", e) %in% cols_filled) {
      use_est[k] <- paste0("X", e)
    } else {
      use_est[k] <- NA_character_
    }
  }
  valid <- !is.na(use_est)
  if (sum(valid) == 0) return(list(RMSE = NA_real_, MAE = NA_real_, cor = NA_real_, sesgo = NA_real_, n = 0))
  est_use <- use_est[valid]
  mascara_use <- mascara[, valid, drop = FALSE]
  verdadero <- as.matrix(ref_df[, estaciones[valid]])[mascara_use]
  imputado  <- as.matrix(filled_df[, est_use])[mascara_use]
  ok <- is.finite(verdadero) & is.finite(imputado)
  if (sum(ok) < 2) return(list(RMSE = NA_real_, MAE = NA_real_, cor = NA_real_, sesgo = NA_real_, n = sum(ok)))
  v <- verdadero[ok]
  i <- imputado[ok]
  n <- length(v)
  list(
    RMSE = sqrt(mean((i - v)^2)),
    MAE = mean(abs(i - v)),
    cor = cor(i, v, use = "complete.obs"),
    sesgo = mean(i - v),
    n = n
  )
}

# -----------------------------------------------------------------------------
# Evaluar una serie de TEMP o PP: periodo continuo + enmascarar + 3 métodos
# -----------------------------------------------------------------------------
evaluar_una_serie_temp_pp <- function(archivo_bruta,
                                      dir_estaciones,
                                      dir_trabajo,
                                      pct_eliminar,
                                      semilla,
                                      metodo_imputeTS,
                                      pct_completo_min,
                                      min_dias_periodo,
                                      verbose) {
  if (verbose) message("=== [temp/pp] Serie: ", basename(archivo_bruta), " ===")
  dat <- cargar_serie(archivo_bruta)
  df_full <- dat$df
  estaciones <- dat$estaciones
  if (length(estaciones) == 0) stop("Sin columnas de estaciones en ", archivo_bruta)

  periodo <- encontrar_periodo_completo(df_full, estaciones, pct_completo = pct_completo_min, min_dias = min_dias_periodo)
  ref_df <- periodo$df_subset
  n_dias <- periodo$n_dias

  if (n_dias == 0) {
    warning("Serie ", basename(archivo_bruta), ": no se encontró periodo con datos continuos. Se omite.")
    return(list(
      archivo = archivo_bruta,
      variable = inferir_variable(archivo_bruta),
      n_dias = 0L,
      n_eliminados = 0L,
      metricas = list(paso1_2 = list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0),
                      imputeTS = list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0),
                      missForest = list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)),
      omitida = TRUE
    ))
  }

  if (verbose) message("  Periodo continuo: ", periodo$fecha_inicio, " a ", periodo$fecha_fin, " (", n_dias, " días)")
  art <- introducir_na_artificial(ref_df, estaciones, pct_eliminar = pct_eliminar, semilla = semilla)
  df_mask <- art$df_mask
  mascara <- art$mascara
  if (verbose) message("  Celdas eliminadas (artificial): ", art$n_eliminados)

  dir_brutas <- file.path(dir_trabajo, "brutas")
  dir_paso12 <- file.path(dir_trabajo, "rellenas_paso1_2")
  dir_imputeTS <- file.path(dir_trabajo, "rellenas_imputeTS")
  dir_missForest <- file.path(dir_trabajo, "rellenas_missForest")
  for (d in c(dir_brutas, dir_paso12, dir_imputeTS, dir_missForest)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  nombre_base <- gsub("_bruta\\.csv$", "", basename(archivo_bruta))
  archivo_mask <- file.path(dir_brutas, paste0(nombre_base, "_bruta.csv"))
  write_csv(df_mask, archivo_mask)

  variable <- inferir_variable(archivo_bruta)
  archivo_metadata <- inferir_metadata_path(archivo_bruta, dir_estaciones)
  if (!file.exists(archivo_metadata)) stop("Metadata no encontrado: ", archivo_metadata)

  resultados <- ejecutar_metodos_rellenado(
    archivo_mask = archivo_mask,
    archivo_metadata = archivo_metadata,
    variable = variable,
    ref_df = ref_df,
    estaciones = estaciones,
    mascara = mascara,
    nombre_base = nombre_base,
    dir_paso12 = dir_paso12,
    dir_imputeTS = dir_imputeTS,
    dir_missForest = dir_missForest,
    metodo_imputeTS = metodo_imputeTS,
    verbose = verbose
  )

  if (verbose) {
    for (met in names(resultados)) {
      r <- resultados[[met]]
      message("  ", met, ": RMSE = ", round(r$RMSE, 4), ", MAE = ", round(r$MAE, 4), ", cor = ", round(r$cor, 4), ", n = ", r$n)
    }
  }

  list(
    archivo = archivo_bruta,
    variable = variable,
    n_dias = n_dias,
    n_eliminados = art$n_eliminados,
    metricas = resultados,
    omitida = FALSE
  )
}

# -----------------------------------------------------------------------------
# Evaluar una serie de CAUDAL: serie completa o rango de años (sin periodo continuo)
# -----------------------------------------------------------------------------
evaluar_una_serie_caudal <- function(archivo_bruta,
                                     dir_estaciones,
                                     dir_trabajo,
                                     pct_eliminar,
                                     semilla,
                                     ano_inicio,
                                     ano_fin,
                                     metodo_imputeTS,
                                     verbose) {
  if (verbose) message("=== [caudal] Serie: ", basename(archivo_bruta), " ===")
  dat <- cargar_serie(archivo_bruta)
  df_full <- dat$df
  estaciones <- dat$estaciones
  if (length(estaciones) == 0) stop("Sin columnas de estaciones en ", archivo_bruta)

  if (!is.null(ano_inicio) && !is.null(ano_fin)) {
    df_full <- df_full[df_full$year >= ano_inicio & df_full$year <= ano_fin, ]
    if (nrow(df_full) == 0) stop("No hay filas en el rango ", ano_inicio, "-", ano_fin)
    if (verbose) message("  Subconjunto: ", ano_inicio, "-", ano_fin, " (", nrow(df_full), " días)")
  }

  ref_df <- df_full
  n_dias <- nrow(ref_df)
  art <- introducir_na_artificial(ref_df, estaciones, pct_eliminar = pct_eliminar, semilla = semilla)
  df_mask <- art$df_mask
  mascara <- art$mascara
  if (verbose) message("  Celdas eliminadas (artificial): ", art$n_eliminados)

  dir_brutas <- file.path(dir_trabajo, "brutas")
  dir_paso12 <- file.path(dir_trabajo, "rellenas_paso1_2")
  dir_imputeTS <- file.path(dir_trabajo, "rellenas_imputeTS")
  dir_missForest <- file.path(dir_trabajo, "rellenas_missForest")
  for (d in c(dir_brutas, dir_paso12, dir_imputeTS, dir_missForest)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  nombre_base <- gsub("_bruta\\.csv$", "", basename(archivo_bruta))
  archivo_mask <- file.path(dir_brutas, paste0(nombre_base, "_bruta.csv"))
  write_csv(df_mask, archivo_mask)

  archivo_metadata <- inferir_metadata_path(archivo_bruta, dir_estaciones)
  if (!file.exists(archivo_metadata)) stop("Metadata no encontrado: ", archivo_metadata)

  resultados <- ejecutar_metodos_rellenado(
    archivo_mask = archivo_mask,
    archivo_metadata = archivo_metadata,
    variable = "q",
    ref_df = ref_df,
    estaciones = estaciones,
    mascara = mascara,
    nombre_base = nombre_base,
    dir_paso12 = dir_paso12,
    dir_imputeTS = dir_imputeTS,
    dir_missForest = dir_missForest,
    metodo_imputeTS = metodo_imputeTS,
    verbose = verbose
  )

  if (verbose) {
    for (met in names(resultados)) {
      r <- resultados[[met]]
      message("  ", met, ": RMSE = ", round(r$RMSE, 4), ", MAE = ", round(r$MAE, 4), ", cor = ", round(r$cor, 4), ", n = ", r$n)
    }
  }

  list(
    archivo = archivo_bruta,
    variable = "q",
    n_dias = n_dias,
    n_eliminados = art$n_eliminados,
    metricas = resultados,
    omitida = FALSE
  )
}

# -----------------------------------------------------------------------------
# Ejecutar los 3 métodos de rellenado y calcular métricas (evita duplicar código)
# -----------------------------------------------------------------------------
ejecutar_metodos_rellenado <- function(archivo_mask,
                                       archivo_metadata,
                                       variable,
                                       ref_df,
                                       estaciones,
                                       mascara,
                                       nombre_base,
                                       dir_paso12,
                                       dir_imputeTS,
                                       dir_missForest,
                                       metodo_imputeTS,
                                       verbose) {
  resultados <- list()

  # Paso 1_2
  if (exists("rellenar_series_paso1_paso2", mode = "function")) {
    if (verbose) message("  Ejecutando paso1_2...")
    res_v2 <- tryCatch({
      rellenar_series_paso1_paso2(
        archivo_bruta = archivo_mask,
        archivo_metadata = archivo_metadata,
        variable = variable,
        dir_salida = dir_paso12,
        pasos = c(1, 2, 3, 4, 6),
        omitir_si_ya_rellena = FALSE,
        mostrar_mensajes = FALSE
      )
    }, error = function(e) {
      if (verbose) message("  [paso1_2] Error: ", conditionMessage(e))
      list(serie_rellena = NULL)
    })
    archivo_rellena <- file.path(dir_paso12, paste0(nombre_base, "_rellena_paso1_2.csv"))
    filled <- NULL
    if (!is.null(res_v2$serie_rellena)) filled <- res_v2$serie_rellena
    else if (file.exists(archivo_rellena)) filled <- as.data.frame(read_csv(archivo_rellena, show_col_types = FALSE))
    if (!is.null(filled)) {
      filled$fecha <- as.Date(filled$fecha)
      filled <- filled[filled$fecha %in% ref_df$fecha, ]
      filled <- filled[match(ref_df$fecha, filled$fecha), ]
      resultados[["paso1_2"]] <- metricas_imputacion(ref_df, filled, estaciones, mascara)
    } else {
      resultados[["paso1_2"]] <- list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)
    }
  }

  # imputeTS
  if (exists("rellenar_una_serie_imputeTS", mode = "function")) {
    if (verbose) message("  Ejecutando imputeTS (", metodo_imputeTS, ")...")
    res_its <- tryCatch({
      rellenar_una_serie_imputeTS(
        archivo_bruta = archivo_mask,
        dir_salida = dir_imputeTS,
        metodo = metodo_imputeTS,
        sufijo = "rellena_imputeTS.csv"
      )
    }, error = function(e) {
      if (verbose) message("  [imputeTS] Error: ", conditionMessage(e))
      list(serie_rellena = NULL)
    })
    if (!is.null(res_its$serie_rellena)) {
      filled <- res_its$serie_rellena
      filled$fecha <- as.Date(filled$fecha)
      filled <- filled[filled$fecha %in% ref_df$fecha, ]
      filled <- filled[match(ref_df$fecha, filled$fecha), ]
      resultados[["imputeTS"]] <- metricas_imputacion(ref_df, filled, estaciones, mascara)
    } else {
      resultados[["imputeTS"]] <- list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)
    }
  }

  # missForest
  if (exists("rellenar_una_serie_missForest", mode = "function")) {
    if (verbose) message("  Ejecutando missForest...")
    res_mf <- tryCatch({
      rellenar_una_serie_missForest(
        archivo_bruta = archivo_mask,
        dir_salida = dir_missForest,
        verbose = FALSE,
        sufijo = "rellena_missForest.csv"
      )
    }, error = function(e) {
      if (verbose) message("  [missForest] Error: ", conditionMessage(e))
      list(serie_rellena = NULL)
    })
    if (!is.null(res_mf$serie_rellena)) {
      filled <- res_mf$serie_rellena
      filled$fecha <- as.Date(filled$fecha)
      filled <- filled[filled$fecha %in% ref_df$fecha, ]
      filled <- filled[match(ref_df$fecha, filled$fecha), ]
      resultados[["missForest"]] <- metricas_imputacion(ref_df, filled, estaciones, mascara)
    } else {
      resultados[["missForest"]] <- list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)
    }
  }

  resultados
}

# -----------------------------------------------------------------------------
# Evaluación integrada: caudal + temperatura + precipitación → una sola tabla
# archivos_brutas: list(temp = "path/series_t_max_..._bruta.csv",
#                       pp   = "path/series_pp_..._bruta.csv",
#                       q    = "path/series_q_..._bruta.csv")
# -----------------------------------------------------------------------------
evaluar_rellenado_integrado <- function(archivos_brutas,
                                        dir_estaciones = "output/output_seleccion_estaciones/estaciones",
                                        dir_trabajo = "output/analisis_rellenado_integrado",
                                        pct_eliminar = 0.1,
                                        semilla = 42,
                                        metodo_imputeTS = "seadec",
                                        pct_completo_min = 1,
                                        min_dias_periodo = 365,
                                        ano_inicio_caudal = NULL,
                                        ano_fin_caudal = NULL,
                                        verbose = TRUE) {
  if (!is.list(archivos_brutas)) {
    archivos_brutas <- list(
      temp = archivos_brutas[1],
      pp   = if (length(archivos_brutas) >= 2) archivos_brutas[2] else NULL,
      q    = if (length(archivos_brutas) >= 3) archivos_brutas[3] else NULL
    )
  }

  todo <- list()

  # Temperatura
  if (!is.null(archivos_brutas$temp) && file.exists(archivos_brutas$temp)) {
    todo[["temp"]] <- evaluar_una_serie_temp_pp(
      archivo_bruta = archivos_brutas$temp,
      dir_estaciones = dir_estaciones,
      dir_trabajo = dir_trabajo,
      pct_eliminar = pct_eliminar,
      semilla = semilla,
      metodo_imputeTS = metodo_imputeTS,
      pct_completo_min = pct_completo_min,
      min_dias_periodo = min_dias_periodo,
      verbose = verbose
    )
  } else if (verbose && !is.null(archivos_brutas$temp)) {
    message("Omitiendo temp: archivo no existe.")
  }

  # Precipitación
  if (!is.null(archivos_brutas$pp) && file.exists(archivos_brutas$pp)) {
    todo[["pp"]] <- evaluar_una_serie_temp_pp(
      archivo_bruta = archivos_brutas$pp,
      dir_estaciones = dir_estaciones,
      dir_trabajo = dir_trabajo,
      pct_eliminar = pct_eliminar,
      semilla = semilla,
      metodo_imputeTS = metodo_imputeTS,
      pct_completo_min = pct_completo_min,
      min_dias_periodo = min_dias_periodo,
      verbose = verbose
    )
  } else if (verbose && !is.null(archivos_brutas$pp)) {
    message("Omitiendo pp: archivo no existe.")
  }

  # Caudal
  if (!is.null(archivos_brutas$q) && file.exists(archivos_brutas$q)) {
    todo[["q"]] <- evaluar_una_serie_caudal(
      archivo_bruta = archivos_brutas$q,
      dir_estaciones = dir_estaciones,
      dir_trabajo = dir_trabajo,
      pct_eliminar = pct_eliminar,
      semilla = semilla,
      ano_inicio = ano_inicio_caudal,
      ano_fin = ano_fin_caudal,
      metodo_imputeTS = metodo_imputeTS,
      verbose = verbose
    )
  } else if (verbose && !is.null(archivos_brutas$q)) {
    message("Omitiendo caudal: archivo no existe.")
  }

  # Tabla final única: variable | archivo | metodo | RMSE | MAE | cor | sesgo | n_celdas | n_dias
  resumen <- data.frame(
    variable = character(),
    archivo = character(),
    metodo = character(),
    RMSE = numeric(),
    MAE = numeric(),
    cor = numeric(),
    sesgo = numeric(),
    n_celdas = integer(),
    n_dias = integer(),
    stringsAsFactors = FALSE
  )

  for (nom in names(todo)) {
    r <- todo[[nom]]
    if (isTRUE(r$omitida)) next
    for (met in names(r$metricas)) {
      m <- r$metricas[[met]]
      resumen <- rbind(resumen, data.frame(
        variable = r$variable,
        archivo = basename(r$archivo),
        metodo = met,
        RMSE = m$RMSE,
        MAE = m$MAE,
        cor = m$cor,
        sesgo = m$sesgo,
        n_celdas = m$n,
        n_dias = r$n_dias,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (verbose) {
    message("")
    message("=== Tabla final: comparación caudal + temperatura + precipitación ===")
    print(resumen)
  }

  if (!dir.exists(dir_trabajo)) dir.create(dir_trabajo, recursive = TRUE)
  archivo_resumen <- file.path(dir_trabajo, "resumen_evaluacion_rellenado_integrado.csv")
  write_csv(resumen, archivo_resumen)
  if (verbose) message("Resumen guardado: ", archivo_resumen)

  invisible(list(resultados_detalle = todo, resumen = resumen))
}

# -----------------------------------------------------------------------------
# Gráficos de comparación: serie bruta vs rellena por estación y método
# Genera los mismos ploteos que estaban en MAIN.R (comparación temp, pp, q
# para estaciones ejemplo con paso1_2, imputeTS y missForest).
# Los guarda en dir_graficos (por defecto output/analisis_rellenado_integrado/graficos_comparacion).
# Si imprimir = TRUE además los muestra con print().
# -----------------------------------------------------------------------------
generar_graficos_comparacion_metodos_rellenado <- function(dir_series = "output/output_seleccion_estaciones/series",
                                                           ano_inicio = 1990,
                                                           ano_fin = 2024,
                                                           calidad_por_variable = c(pp = 60, temp = 70, q = 50),
                                                           dir_graficos = "output/analisis_rellenado_integrado/graficos_comparacion",
                                                           imprimir = FALSE) {
  if (!exists("comparar_serie_estacion", mode = "function")) {
    stop("Se requiere comparar_serie_estacion. Cargue: source('scripts/comparar_series_bruta_vs_rellena.R')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Se requiere el paquete ggplot2 para guardar los gráficos.")
  if (!dir.exists(dir_graficos)) dir.create(dir_graficos, recursive = TRUE)
  graficos <- list()

  # Temperatura, El Teniente, estación 330088 (paso1_2, imputeTS, missForest)
  p <- comparar_serie_estacion("330088", variable = "temp", division = "el_teniente", ano_inicio, ano_fin,
                               calidad_por_variable["temp"], tipo_temp = "t_max", dir_series = dir_series)
  graficos[["temp_330088_paso1_2"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("330088", variable = "temp", division = "el_teniente", ano_inicio, ano_fin,
                               calidad_por_variable["temp"], tipo_temp = "t_max", dir_series = dir_series,
                               tipo_rellena = "imputeTS")
  graficos[["temp_330088_imputeTS"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("330088", variable = "temp", division = "el_teniente", ano_inicio, ano_fin,
                               calidad_por_variable["temp"], tipo_temp = "t_max", dir_series = dir_series,
                               tipo_rellena = "missForest")
  graficos[["temp_330088_missForest"]] <- p
  if (imprimir) print(p)

  # Caudal, El Salvador, estación 02103014-7
  p <- comparar_serie_estacion("02103014-7", variable = "q", division = "el_salvador",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["q"],
                               dir_series = dir_series)
  graficos[["q_02103014-7_paso1_2"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("02103014-7", variable = "q", division = "el_salvador",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["q"],
                               tipo_rellena = "imputeTS", dir_series = dir_series)
  graficos[["q_02103014-7_imputeTS"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("02103014-7", variable = "q", division = "el_salvador",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["q"],
                               tipo_rellena = "missForest", dir_series = dir_series)
  graficos[["q_02103014-7_missForest"]] <- p
  if (imprimir) print(p)

  # Precipitación, El Teniente, estación 05110002-6
  p <- comparar_serie_estacion("05110002-6", variable = "pp", division = "el_teniente",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["pp"],
                               dir_series = dir_series)
  graficos[["pp_05110002-6_paso1_2"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("05110002-6", variable = "pp", division = "el_teniente",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["pp"],
                               tipo_rellena = "imputeTS", dir_series = dir_series)
  graficos[["pp_05110002-6_imputeTS"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("05110002-6", variable = "pp", division = "el_teniente",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["pp"],
                               tipo_rellena = "missForest", dir_series = dir_series)
  graficos[["pp_05110002-6_missForest"]] <- p
  if (imprimir) print(p)

  # Precipitación, El Teniente, estación 340035
  p <- comparar_serie_estacion("340035", variable = "pp", division = "el_teniente",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["pp"],
                               dir_series = dir_series)
  graficos[["pp_340035_paso1_2"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("340035", variable = "pp", division = "el_teniente",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["pp"],
                               tipo_rellena = "imputeTS", dir_series = dir_series)
  graficos[["pp_340035_imputeTS"]] <- p
  if (imprimir) print(p)

  p <- comparar_serie_estacion("340035", variable = "pp", division = "el_teniente",
                               ano_inicio = ano_inicio, ano_fin = ano_fin, calidad = calidad_por_variable["pp"],
                               tipo_rellena = "missForest", dir_series = dir_series)
  graficos[["pp_340035_missForest"]] <- p
  if (imprimir) print(p)

  # Guardar cada gráfico en dir_graficos
  for (nom in names(graficos)) {
    archivo <- file.path(dir_graficos, paste0(nom, ".png"))
    ggplot2::ggsave(archivo, plot = graficos[[nom]], width = 12, height = 5, dpi = 150, bg = "white")
  }
  message("Gráficos guardados en: ", dir_graficos)

  invisible(graficos)
}

# -----------------------------------------------------------------------------
# Ejemplo de uso (setwd en caracterizacion_historica)
# -----------------------------------------------------------------------------
#
# source("scripts/analisis_rellenado/rellenar_series_v2.R")
# source("scripts/analisis_rellenado/rellenar_series_imputeTS.R")
# source("scripts/MAIN/3.rellenar_series_missForest.R")
#
# dir_brutas    <- "output/output_seleccion_estaciones/series/brutas"
# dir_estaciones <- "output/output_seleccion_estaciones/estaciones"
#
# evaluar_rellenado_integrado(
   archivos_brutas = list(
     temp = file.path(dir_brutas, "series_t_max_el_salvador_70_1990_2024_bruta.csv"),
     pp   = file.path(dir_brutas, "series_pp_el_salvador_60_1990_2024_bruta.csv"),
     q    = file.path(dir_brutas, "series_q_el_salvador_50_1990_2024_bruta.csv")
   ),
   dir_estaciones = dir_estaciones,
   pct_eliminar = 0.1,
   pct_completo_min = 0.99,
   min_dias_periodo = 365,
   ano_inicio_caudal = 2015,
   ano_fin_caudal = 2024
# )
#
# generar_graficos_comparacion_metodos_rellenado(
#   dir_series = "output/output_seleccion_estaciones/series",
#   ano_inicio = 1990,
#   ano_fin = 2024,
#   calidad_por_variable = c(pp = 60, temp = 70, q = 50)
# )