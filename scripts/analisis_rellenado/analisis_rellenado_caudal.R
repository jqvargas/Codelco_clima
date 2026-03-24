# =============================================================================
# Script: analisis_rellenado_caudal.R
# Evalúa algoritmos de rellenado solo sobre series brutas de CAUDAL.
# No usa "periodo continuo": trabaja sobre toda la serie (o un rango de años)
# y enmascara un % de celdas con dato para comparar imputado vs verdadero.
#
# Requiere: rellenar_series_v2.R, rellenar_series_imputeTS.R, rellenar_series_missForest.R
# Uso: evaluar_rellenado_caudal(archivos_bruta, dir_estaciones, ...)
# =============================================================================

library(readr)
library(lubridate)

# -----------------------------------------------------------------------------
# Cargar serie bruta (caudal): fecha + columnas de estaciones
# -----------------------------------------------------------------------------
cargar_serie_caudal <- function(archivo) {
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
  estaciones <- setdiff(names(df), cols_fecha)
  estaciones <- estaciones[vapply(estaciones, function(c) is.numeric(df[[c]]), logical(1))]
  list(df = df, estaciones = estaciones)
}

# -----------------------------------------------------------------------------
# Metadata de caudal: series_q_el_salvador_50_1990_2024_bruta.csv -> estaciones_caudal_el_salvador_50_1990_2024.csv
# -----------------------------------------------------------------------------
metadata_caudal_path <- function(archivo_bruta, dir_estaciones) {
  bn <- basename(archivo_bruta)
  if (!grepl("^series_q_.*_bruta\\.csv$", bn)) stop("Archivo debe ser serie de caudal: series_q_el_..._bruta.csv")
  base <- gsub("_bruta\\.csv$", "", bn)
  nombre_meta <- gsub("^series_q_", "estaciones_caudal_", base)
  file.path(dir_estaciones, paste0(nombre_meta, ".csv"))
}

# -----------------------------------------------------------------------------
# Introducir NA artificial en pct_eliminar de las celdas con dato
# -----------------------------------------------------------------------------
introducir_na_caudal <- function(df, estaciones, pct_eliminar = 0.1, semilla = 42) {
  set.seed(semilla)
  df_mask <- as.data.frame(df)
  n_filas <- nrow(df_mask)
  n_cols <- length(estaciones)
  posiciones <- which(!is.na(as.matrix(df_mask[, estaciones])))
  n_disponibles <- length(posiciones)
  if (n_disponibles == 0) stop("No hay celdas con dato en la serie de caudal.")
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
# Métricas: comparar imputado vs verdadero en celdas enmascaradas
# -----------------------------------------------------------------------------
metricas_caudal <- function(ref_df, filled_df, estaciones, mascara) {
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
  if (sum(valid) == 0) return(list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = 0))
  est_use <- use_est[valid]
  mascara_use <- mascara[, valid, drop = FALSE]
  verdadero <- as.matrix(ref_df[, estaciones[valid]])[mascara_use]
  imputado  <- as.matrix(filled_df[, est_use])[mascara_use]
  ok <- is.finite(verdadero) & is.finite(imputado)
  if (sum(ok) < 2) return(list(RMSE = NA, MAE = NA, cor = NA, sesgo = NA, n = sum(ok)))
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
# Evaluar una serie de caudal: enmascarar, rellenar con los 3 métodos, comparar
# archivo_bruta: ruta al CSV bruta de caudal (series_q_el_..._bruta.csv)
# ano_inicio, ano_fin: opcional; si se indican, se usa solo ese rango para reducir tiempo
# -----------------------------------------------------------------------------
evaluar_una_serie_caudal <- function(archivo_bruta,
                                    dir_estaciones,
                                    dir_trabajo = "output/analisis_rellenado_caudal",
                                    pct_eliminar = 0.1,
                                    semilla = 42,
                                    ano_inicio = NULL,
                                    ano_fin = NULL,
                                    metodo_imputeTS = "seadec",
                                    verbose = TRUE) {
  if (verbose) message("=== Caudal: ", basename(archivo_bruta), " ===")

  dat <- cargar_serie_caudal(archivo_bruta)
  df_full <- dat$df
  estaciones <- dat$estaciones
  if (length(estaciones) == 0) stop("Sin columnas de estaciones en ", archivo_bruta)

  # Opcional: subconjunto por años para acelerar (p. ej. últimos 10 años)
  if (!is.null(ano_inicio) && !is.null(ano_fin)) {
    df_full <- df_full[df_full$year >= ano_inicio & df_full$year <= ano_fin, ]
    if (nrow(df_full) == 0) stop("No hay filas en el rango ", ano_inicio, "-", ano_fin)
    if (verbose) message("  Subconjunto: ", ano_inicio, "-", ano_fin, " (", nrow(df_full), " días)")
  }

  ref_df <- df_full
  n_dias <- nrow(ref_df)
  n_celdas_con_dato <- sum(!is.na(as.matrix(ref_df[, estaciones])))
  if (verbose) message("  Días: ", n_dias, " | Celdas con dato: ", n_celdas_con_dato)

  art <- introducir_na_caudal(ref_df, estaciones, pct_eliminar = pct_eliminar, semilla = semilla)
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

  archivo_metadata <- metadata_caudal_path(archivo_bruta, dir_estaciones)
  if (!file.exists(archivo_metadata)) {
    stop("Metadata no encontrado: ", archivo_metadata)
  }

  resultados <- list()

  # Paso 1_2 (v2)
  if (exists("rellenar_series_paso1_paso2", mode = "function")) {
    if (verbose) message("  Ejecutando paso1_2...")
    res_v2 <- tryCatch({
      rellenar_series_paso1_paso2(
        archivo_bruta = archivo_mask,
        archivo_metadata = archivo_metadata,
        variable = "q",
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
      resultados[["paso1_2"]] <- metricas_caudal(ref_df, filled, estaciones, mascara)
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
      resultados[["imputeTS"]] <- metricas_caudal(ref_df, filled, estaciones, mascara)
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
      resultados[["missForest"]] <- metricas_caudal(ref_df, filled, estaciones, mascara)
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
    n_dias = n_dias,
    n_eliminados = art$n_eliminados,
    metricas = resultados
  )
}

# -----------------------------------------------------------------------------
# Evaluar una o varias series brutas de caudal
# archivos_bruta: vector de rutas a CSV (series_q_el_..._bruta.csv) o una sola ruta
# -----------------------------------------------------------------------------
evaluar_rellenado_caudal <- function(archivos_bruta,
                                    dir_estaciones = "output/output_seleccion_estaciones/estaciones",
                                    dir_trabajo = "output/analisis_rellenado_caudal",
                                    pct_eliminar = 0.1,
                                    semilla = 42,
                                    ano_inicio = NULL,
                                    ano_fin = NULL,
                                    metodo_imputeTS = "seadec",
                                    verbose = TRUE) {
  if (is.character(archivos_bruta) && length(archivos_bruta) == 1) {
    archivos_bruta <- list(archivos_bruta)
  } else if (is.character(archivos_bruta)) {
    archivos_bruta <- as.list(archivos_bruta)
  }
  todo <- list()
  for (k in seq_along(archivos_bruta)) {
    path <- archivos_bruta[[k]]
    if (is.null(path) || !file.exists(path)) {
      if (verbose) message("Omitiendo: archivo no existe - ", path)
      next
    }
    nom <- basename(path)
    todo[[nom]] <- evaluar_una_serie_caudal(
      archivo_bruta = path,
      dir_estaciones = dir_estaciones,
      dir_trabajo = dir_trabajo,
      pct_eliminar = pct_eliminar,
      semilla = semilla,
      ano_inicio = ano_inicio,
      ano_fin = ano_fin,
      metodo_imputeTS = metodo_imputeTS,
      verbose = verbose
    )
  }

  # Tabla resumen
  resumen <- data.frame(
    archivo = character(),
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
    for (met in names(r$metricas)) {
      m <- r$metricas[[met]]
      resumen <- rbind(resumen, data.frame(
        archivo = basename(r$archivo),
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
    message("=== Resumen evaluación caudal ===")
    print(resumen)
  }
  archivo_resumen <- file.path(dir_trabajo, "resumen_evaluacion_caudal.csv")
  if (!dir.exists(dir_trabajo)) dir.create(dir_trabajo, recursive = TRUE)
  write_csv(resumen, archivo_resumen)
  if (verbose) message("Resumen guardado: ", archivo_resumen)

  invisible(list(resultados_detalle = todo, resumen = resumen))
}

# -----------------------------------------------------------------------------
# Ejemplo de uso (setwd en caracterizacion_historica)
# -----------------------------------------------------------------------------
#
# source("scripts/analisis_rellenado/rellenar_series_v2.R")
# source("scripts/analisis_rellenado/rellenar_series_imputeTS.R")
# source("scripts/MAIN/3.rellenar_series_missForest.R")
# source("scripts/analisis_rellenado/analisis_rellenado_caudal.R")
#
#dir_brutas   <- "output/output_seleccion_estaciones/series/brutas"
#dir_estaciones <- "output/output_seleccion_estaciones/estaciones"

 # Una serie (El Salvador)
 #evaluar_rellenado_caudal(
#   archivos_bruta = file.path(dir_brutas, "series_q_el_salvador_50_1990_2024_bruta.csv"),
#   dir_estaciones = dir_estaciones,
#   pct_eliminar = 0.1
# )
#
# # Reducir tiempo: solo años 2015-2024
# evaluar_rellenado_caudal(
 #  archivos_bruta = file.path(dir_brutas, "series_q_el_salvador_50_1990_2024_bruta.csv"),
#  dir_estaciones = dir_estaciones,
#   ano_inicio = 2015,
#   ano_fin = 2024,
#   pct_eliminar = 0.1
 )
#
# # Ambas divisiones
# evaluar_rellenado_caudal(
#   archivos_bruta = c(
#     file.path(dir_brutas, "series_q_el_salvador_50_1990_2024_bruta.csv"),
#     file.path(dir_brutas, "series_q_el_teniente_50_1990_2024_bruta.csv")
#   ),
#   dir_estaciones = dir_estaciones
# )
