# =============================================================================
# Script: analisis_rellenado.R
# Evalúa algoritmos de rellenado (paso1_2, imputeTS, missForest) usando series
# con datos artificialmente eliminados y comparando con el valor verdadero.
#
# Flujo:
#   1. Lee CSV de temperatura, precipitación y caudal (series brutas).
#   2. En cada serie, encuentra el periodo más largo con datos continuos
#      (sin faltantes en ninguna celda, o con umbral de completitud opcional).
#   3. Introduce NA de forma artificial en un porcentaje de celdas (pct_eliminar).
#   4. Ejecuta los tres métodos de rellenado sobre la serie con huecos.
#   5. Compara valores imputados vs verdaderos: RMSE, MAE, correlación, sesgo.
#
# Requiere: rellenar_series_v2.R, rellenar_series_imputeTS.R, rellenar_series_missForest.R
# Uso: evaluar_algoritmos_rellenado(archivos_brutas, dir_estaciones, ...)
# =============================================================================

library(readr)
library(lubridate)
library(dplyr)

# Cargar scripts de rellenado (asumir que se hace desde caracterizacion_historica o vía source en MAIN)
# source("scripts/analisis_rellenado/rellenar_series_v2.R")
# source("scripts/analisis_rellenado/rellenar_series_imputeTS.R")
# source("scripts/MAIN/3.rellenar_series_missForest.R")

# -----------------------------------------------------------------------------
# Inferir ruta de metadata a partir del nombre del archivo bruto
# series_t_max_el_salvador_70_1990_2024_bruta.csv -> estaciones_temp_el_salvador_70_1990_2024.csv
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
# Cargar serie bruta y detectar columnas de fecha y estaciones
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
# Encontrar el periodo más largo con datos continuos (o con completitud >= pct)
# df: data frame con fecha y columnas de estaciones
# pct_completo: 1 = sin ningún NA; <1 = fracción mínima de celdas con dato por fila
# min_dias: si el periodo 100% completo es más corto, se usa pct_completo hasta alcanzar min_dias
# Devuelve: list(df_subset, fecha_inicio, fecha_fin, n_dias, completo_estricto)
# -----------------------------------------------------------------------------
encontrar_periodo_completo <- function(df, estaciones, pct_completo = 1, min_dias = 365) {
  if (length(estaciones) == 0) stop("Sin columnas de estaciones")
  df <- df[order(df$fecha), ]
  n_est <- length(estaciones)
  # Por fila: sin ningún NA (pct_completo >= 1) o fracción de celdas con dato >= pct_completo
  if (pct_completo >= 1) {
    fila_ok <- rowSums(is.na(df[, estaciones, drop = FALSE])) == 0
  } else {
    fila_ok <- rowSums(!is.na(df[, estaciones, drop = FALSE])) / n_est >= pct_completo
  }
  runs <- rle(fila_ok)
  if (!any(runs$values)) {
    # Sin periodo al umbral actual: probar umbrales más bajos (p. ej. caudal sin tramo 100% completo)
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
      # Aceptar el tramo más largo aunque sea < min_dias (para que caudal entre en la evaluación)
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
      # Último recurso: tramo donde cada día tenga al menos 1 estación con dato (p. ej. caudal con muchas estaciones y muchos NA)
      fila_ok_1 <- rowSums(!is.na(df[, estaciones, drop = FALSE])) >= 1
      runs_1 <- rle(fila_ok_1)
      if (any(runs_1$values)) {
        run_lengths_1 <- runs_1$lengths[runs_1$values]
        run_starts_1 <- cumsum(c(1L, runs_1$lengths))[which(runs_1$values)]
        idx_1 <- which.max(run_lengths_1)[1]
        n_dias_1 <- run_lengths_1[idx_1]
        if (n_dias_1 >= 30) {
          inicio <- run_starts_1[idx_1]
          fin <- inicio + n_dias_1 - 1L
          df_sub <- df[inicio:fin, ]
          return(list(
            df_subset = df_sub,
            fecha_inicio = df_sub$fecha[1],
            fecha_fin = df_sub$fecha[nrow(df_sub)],
            n_dias = n_dias_1,
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
  # Si 100% completo es muy corto, buscar periodo con 99% de completitud
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
# Introducir NA artificial en un porcentaje de celdas (solo en columnas estación)
# Devuelve: list(df_mask = data frame con NA introducidos, mascara = matriz lógica TRUE = fue eliminado)
# -----------------------------------------------------------------------------
introducir_na_artificial <- function(df, estaciones, pct_eliminar = 0.1, semilla = 42) {
  set.seed(semilla)
  df_mask <- as.data.frame(df)
  n_filas <- nrow(df_mask)
  n_cols <- length(estaciones)
  total_celdas <- n_filas * n_cols
  # Solo considerar celdas que actualmente tienen valor (no NA)
  posiciones <- which(!is.na(as.matrix(df_mask[, estaciones])))
  n_disponibles <- length(posiciones)
  if (n_disponibles == 0) stop("No hay celdas con dato para eliminar")
  n_eliminar <- max(1, round(n_disponibles * pct_eliminar))
  n_eliminar <- min(n_eliminar, n_disponibles)
  elegidos <- sample(posiciones, size = n_eliminar, replace = FALSE)
  # Convertir índice lineal a (fila, columna dentro de estaciones)
  mascara_mat <- matrix(FALSE, nrow = n_filas, ncol = n_cols)
  mascara_mat[elegidos] <- TRUE
  for (j in seq_along(estaciones)) {
    df_mask[[estaciones[j]]][mascara_mat[, j]] <- NA
  }
  list(df_mask = df_mask, mascara = mascara_mat, n_eliminados = n_eliminar)
}

# -----------------------------------------------------------------------------
# Métricas de evaluación: comparar valores imputados vs verdaderos solo en celdas enmascaradas
# -----------------------------------------------------------------------------
evaluar_imputacion <- function(ref_df, filled_df, estaciones, mascara) {
  # Alinear columnas: readr puede convertir "320028" a "X320028" en filled_df
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
  if (sum(ok) < 2) {
    return(list(RMSE = NA_real_, MAE = NA_real_, cor = NA_real_, sesgo = NA_real_, n = sum(ok)))
  }
  v <- verdadero[ok]
  i <- imputado[ok]
  n <- length(v)
  rmse <- sqrt(mean((i - v)^2))
  mae <- mean(abs(i - v))
  cor_val <- cor(i, v, use = "complete.obs")
  if (is.na(cor_val)) cor_val <- NA_real_
  sesgo <- mean(i - v)
  list(RMSE = rmse, MAE = mae, cor = cor_val, sesgo = sesgo, n = n)
}

# -----------------------------------------------------------------------------
# Ejecutar evaluación para una serie (un archivo bruto)
# archivo_bruta: ruta al CSV bruto
# dir_estaciones: carpeta con estaciones_*.csv
# dir_trabajo: carpeta temporal para escribir serie con NA y resultados (brutas/rellenas por método)
# pct_eliminar: fracción de celdas a eliminar (ej. 0.1 = 10%)
# metodo_imputeTS: "interpolation", "kalman", "seadec", etc.
# -----------------------------------------------------------------------------
evaluar_una_serie <- function(archivo_bruta,
                              dir_estaciones,
                              dir_trabajo = "output/analisis_rellenado",
                              pct_eliminar = 0.1,
                              semilla = 42,
                              metodo_imputeTS = "seadec",
                              pct_completo_min = 1,
                              min_dias_periodo = 365,
                              verbose = TRUE) {
  if (verbose) message("=== Serie: ", basename(archivo_bruta), " ===")
  dat <- cargar_serie(archivo_bruta)
  df_full <- dat$df
  estaciones <- dat$estaciones
  if (length(estaciones) == 0) stop("Sin columnas de estaciones en ", archivo_bruta)

  # Periodo más largo con datos continuos
  periodo <- encontrar_periodo_completo(df_full, estaciones, pct_completo = pct_completo_min, min_dias = min_dias_periodo)
  ref_df <- periodo$df_subset
  n_dias <- periodo$n_dias

  if (n_dias == 0) {
    warning("Serie ", basename(archivo_bruta), ": no se encontró periodo con datos continuos (0 días). Se omite esta serie. Considere bajar pct_completo_min (ej. 0.95) o min_dias_periodo.")
    return(list(
      archivo = archivo_bruta,
      variable = inferir_variable(archivo_bruta),
      periodo = periodo,
      n_eliminados = 0L,
      metricas = list(paso1_2 = list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0),
                      imputeTS = list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0),
                      missForest = list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)),
      omitida = TRUE
    ))
  }

  if (n_dias < 30) {
    warning("Periodo completo muy corto (", n_dias, " días). Considerar bajar pct_completo_min o min_dias_periodo.")
  }
  if (verbose) message("  Periodo continuo: ", periodo$fecha_inicio, " a ", periodo$fecha_fin, " (", n_dias, " días)")

  # Introducir NA artificial
  art <- introducir_na_artificial(ref_df, estaciones, pct_eliminar = pct_eliminar, semilla = semilla)
  df_mask <- art$df_mask
  mascara <- art$mascara
  if (verbose) message("  Celdas eliminadas artificialmente: ", art$n_eliminados)

  # Directorios de trabajo
  dir_brutas <- file.path(dir_trabajo, "brutas")
  dir_paso12 <- file.path(dir_trabajo, "rellenas_paso1_2")
  dir_imputeTS <- file.path(dir_trabajo, "rellenas_imputeTS")
  dir_missForest <- file.path(dir_trabajo, "rellenas_missForest")
  for (d in c(dir_brutas, dir_paso12, dir_imputeTS, dir_missForest)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # Escribir serie con huecos (mismo nombre base para que metadata coincida)
  nombre_base <- gsub("_bruta\\.csv$", "", basename(archivo_bruta))
  archivo_mask <- file.path(dir_brutas, paste0(nombre_base, "_bruta.csv"))
  write_csv(df_mask, archivo_mask)

  variable <- inferir_variable(archivo_bruta)
  archivo_metadata <- inferir_metadata_path(archivo_bruta, dir_estaciones)
  if (!file.exists(archivo_metadata)) {
    stop("Metadata no encontrado: ", archivo_metadata)
  }

  resultados <- list()

  # 1) Paso 1_2 (rellenar_series_v2)
  if (exists("rellenar_series_paso1_paso2", mode = "function")) {
    if (verbose) message("  Ejecutando rellenar_series_paso1_paso2...")
    # Pasos 1,2,3,4,6 para que rellene TODOS los NA (paso 6 rellena restantes); si solo usamos 1,2
    # muchos huecos artificiales no se rellenan porque el mes sigue con >70% completitud
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
    archivo_rellena_paso12 <- file.path(dir_paso12, paste0(nombre_base, "_rellena_paso1_2.csv"))
    if (!is.null(res_v2$serie_rellena)) {
      filled <- res_v2$serie_rellena
    } else if (file.exists(archivo_rellena_paso12)) {
      filled <- as.data.frame(read_csv(archivo_rellena_paso12, show_col_types = FALSE))
    } else {
      filled <- NULL
    }
    if (!is.null(filled)) {
      # Mismo orden y mismas filas que ref_df (por fecha)
      filled$fecha <- as.Date(filled$fecha)
      filled <- filled[filled$fecha %in% ref_df$fecha, ]
      filled <- filled[match(ref_df$fecha, filled$fecha), ]
      resultados[["paso1_2"]] <- evaluar_imputacion(ref_df, filled, estaciones, mascara)
    } else {
      resultados[["paso1_2"]] <- list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)
    }
  }

  # 2) imputeTS
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
      resultados[["imputeTS"]] <- evaluar_imputacion(ref_df, filled, estaciones, mascara)
    } else {
      resultados[["imputeTS"]] <- list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)
    }
  }

  # 3) missForest
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
      resultados[["missForest"]] <- evaluar_imputacion(ref_df, filled, estaciones, mascara)
    } else {
      resultados[["missForest"]] <- list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0)
    }
  }

  if (verbose) {
    for (met in names(resultados)) {
      r <- resultados[[met]]
      message("  ", met, ": RMSE = ", round(r$RMSE, 4), ", MAE = ", round(r$MAE, 4), ", cor = ", round(r$cor, 4), ", n = ", r$n)
    }
  }
  list(
    archivo = archivo_bruta,
    variable = variable,
    periodo = periodo,
    n_eliminados = art$n_eliminados,
    metricas = resultados
  )
}

# -----------------------------------------------------------------------------
# Evaluar algoritmos para varias series (temperatura, precipitación, caudal)
# archivos_brutas: lista con nombres "temp", "pp", "q" y rutas a CSV brutas
#   ej. list(temp = "path/series_t_max_el_salvador_70_1990_2024_bruta.csv", pp = "...", q = "...")
# -----------------------------------------------------------------------------
evaluar_algoritmos_rellenado <- function(archivos_brutas,
                                        dir_estaciones = "output/output_seleccion_estaciones/estaciones",
                                        dir_trabajo = "output/analisis_rellenado",
                                        pct_eliminar = 0.1,
                                        semilla = 42,
                                        metodo_imputeTS = "seadec",
                                        pct_completo_min = 1,
                                        min_dias_periodo = 365,
                                        verbose = TRUE) {
  if (!is.list(archivos_brutas)) archivos_brutas <- list(temp = archivos_brutas[1], pp = archivos_brutas[2], q = archivos_brutas[3])
  todo <- list()
  for (nom in names(archivos_brutas)) {
    path <- archivos_brutas[[nom]]
    if (is.null(path) || !file.exists(path)) {
      if (verbose) message("Omitiendo ", nom, ": archivo no indicado o no existe.")
      next
    }
    todo[[nom]] <- evaluar_una_serie(
      archivo_bruta = path,
      dir_estaciones = dir_estaciones,
      dir_trabajo = dir_trabajo,
      pct_eliminar = pct_eliminar,
      semilla = semilla,
      metodo_imputeTS = metodo_imputeTS,
      pct_completo_min = pct_completo_min,
      min_dias_periodo = min_dias_periodo,
      verbose = verbose
    )
  }

  # Tabla resumen: variable x método x métrica
  resumen <- data.frame(
    variable = character(),
    metodo = character(),
    RMSE = numeric(),
    MAE = numeric(),
    cor = numeric(),
    sesgo = numeric(),
    n_celdas = integer(),
    stringsAsFactors = FALSE
  )
  for (nom in names(todo)) {
    r <- todo[[nom]]
    if (isTRUE(r$omitida)) next
    for (met in names(r$metricas)) {
      m <- r$metricas[[met]]
      resumen <- rbind(resumen, data.frame(
        variable = r$variable,
        metodo = met,
        RMSE = m$RMSE,
        MAE = m$MAE,
        cor = m$cor,
        sesgo = m$sesgo,
        n_celdas = m$n,
        stringsAsFactors = FALSE
      ))
    }
  }
  if (verbose) {
    message("")
    message("=== Resumen de puntajes ===")
    print(resumen)
  }
  # Guardar resumen en CSV
  archivo_resumen <- file.path(dir_trabajo, "resumen_evaluacion_rellenado.csv")
  if (!dir.exists(dir_trabajo)) dir.create(dir_trabajo, recursive = TRUE)
  write_csv(resumen, archivo_resumen)
  if (verbose) message("Resumen guardado en: ", archivo_resumen)
  invisible(list(resultados_detalle = todo, resumen = resumen))
}

# -----------------------------------------------------------------------------
# Ejemplo de uso (con setwd en caracterizacion_historica)
# -----------------------------------------------------------------------------
#
# source("scripts/analisis_rellenado/rellenar_series_v2.R")
# source("scripts/analisis_rellenado/rellenar_series_imputeTS.R")
# source("scripts/MAIN/3.rellenar_series_missForest.R")

#dir_brutas <- "output/output_seleccion_estaciones/series/brutas"
#dir_estaciones <- "output/output_seleccion_estaciones/estaciones"
#
#evaluar_algoritmos_rellenado(
#archivos_brutas = list(
#     temp = file.path(dir_brutas, "series_t_max_el_salvador_70_1990_2024_bruta.csv"),
#     pp   = file.path(dir_brutas, "series_pp_el_salvador_60_1990_2024_bruta.csv"),
#     q    = file.path(dir_brutas, "series_q_el_salvador_50_1990_2024_bruta.csv")
#   ),
#   dir_estaciones = dir_estaciones,
#   pct_eliminar   = 0.1,
#   pct_completo_min = 0.99,
#   min_dias_periodo = 365
# )
