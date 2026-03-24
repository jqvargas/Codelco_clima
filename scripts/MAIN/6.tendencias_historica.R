# =============================================================================
# Script: 5.tendencias_historica.R
# Calcula tendencias decadales e indicadores extremos a partir de las series
# diarias rellenas (por defecto output/.../series/rellenas_filtradas_envolvente tras paso 4.5).
# Usa 5.1.trends_preproceso.r y 5.2.analisis_extremos_original.R.
# Salida: output/tendencias_historica/ (por división: teniente, salvador).
# =============================================================================

library(readr)
library(dplyr)

# Cargar scripts de preproceso y análisis de extremos (rutas relativas a setwd = caracterizacion_historica)
if (!exists("process_climate_data", mode = "function") || !exists("process_metadata", mode = "function"))
  source("scripts/MAIN/6.1.trends_preproceso.r")
if (!exists("analisis_extremos", mode = "function"))
  source("scripts/MAIN/6.2.analisis_extremos_original.R")

# -----------------------------------------------------------------------------
# Ejecutar tendencias e indicadores extremos por división
# Series de entrada: formato fecha, year, month, day + columnas por estación (codigo_nacional).
# Estaciones: CSV con codigo_nacional, nombre o nombre_estacion, lat, lon, calidad_*.
# -----------------------------------------------------------------------------
ejecutar_tendencias_historica <- function(
  dir_series_rellenas = "output/output_seleccion_estaciones/series/rellenas_filtradas_envolvente",
  dir_estaciones = "output/output_seleccion_estaciones/estaciones",
  calidad_por_variable, # Vector nombrado completo c(pp=, temp=, q=) como en MAIN.R
  ano_inicio,           # Definido en MAIN.R — no redefinir aquí
  ano_fin,              # Definido en MAIN.R — no redefinir aquí
  dir_output = "output/tendencias_historica",
  divisiones = c("el_teniente", "el_salvador"),
  verbose = TRUE
) {
  nms <- names(calidad_por_variable)
  if (is.null(nms) || !all(c("pp", "temp") %in% nms)) {
    stop(
      "calidad_por_variable debe ser el vector completo con nombres pp, temp y q ",
      "(p. ej. c(pp = 70, temp = 60, q = 70)), no un solo elemento."
    )
  }

  if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

  div_archivo <- c(el_teniente = "teniente", el_salvador = "salvador")
  name_division <- c(el_teniente = "teniente", el_salvador = "salvador")

  for (division in divisiones) {
    div <- div_archivo[division]
    name <- name_division[division]
    cal_pp   <- as.numeric(calidad_por_variable["pp"])
    cal_temp <- as.numeric(calidad_por_variable["temp"])
    cal_q    <- as.numeric(calidad_por_variable["q"])
    if (is.na(cal_pp)) cal_pp <- 60
    if (is.na(cal_temp)) cal_temp <- 70
    if (is.na(cal_q)) cal_q <- 50

    # Rutas de series rellenas (convención del proyecto)
    archivo_pp    <- file.path(dir_series_rellenas, paste0("series_pp_el_", div, "_", cal_pp, "_", ano_inicio, "_", ano_fin, "_rellena_missForest.csv"))
    archivo_t_max <- file.path(dir_series_rellenas, paste0("series_t_max_el_", div, "_", cal_temp, "_", ano_inicio, "_", ano_fin, "_rellena_missForest.csv"))
    archivo_t_min <- file.path(dir_series_rellenas, paste0("series_t_min_el_", div, "_", cal_temp, "_", ano_inicio, "_", ano_fin, "_rellena_missForest.csv"))
    archivo_q    <- file.path(dir_series_rellenas, paste0("series_q_el_", div, "_", cal_q, "_", ano_inicio, "_", ano_fin, "_rellena_missForest.csv"))

    # Rutas de metadata de estaciones
    archivo_est_pp    <- file.path(dir_estaciones, paste0("estaciones_pp_el_", div, "_", cal_pp, "_", ano_inicio, "_", ano_fin, ".csv"))
    archivo_est_temp  <- file.path(dir_estaciones, paste0("estaciones_temp_el_", div, "_", cal_temp, "_", ano_inicio, "_", ano_fin, ".csv"))
    archivo_est_caudal <- file.path(dir_estaciones, paste0("estaciones_caudal_el_", div, "_", cal_q, "_", ano_inicio, "_", ano_fin, ".csv"))

    # Cargar solo si existen (pp, t_max, t_min obligatorios para extremos; q opcional)
    if (!file.exists(archivo_pp) || !file.exists(archivo_t_max) || !file.exists(archivo_t_min)) {
      if (verbose) message("Tendencias [", name, "]: omitido (faltan pp, t_max o t_min rellenos).")
      next
    }

    if (verbose) message("Tendencias [", name, "]: cargando series y estaciones...")

    pp <- as.data.frame(read_csv(archivo_pp, show_col_types = FALSE))
    t_max <- as.data.frame(read_csv(archivo_t_max, show_col_types = FALSE))
    t_min <- as.data.frame(read_csv(archivo_t_min, show_col_types = FALSE))
    q <- NULL
    if (file.exists(archivo_q)) q <- as.data.frame(read_csv(archivo_q, show_col_types = FALSE))

    estaciones_pp <- estaciones_temp <- estaciones_q <- NULL
    if (file.exists(archivo_est_pp))   estaciones_pp   <- read.csv(archivo_est_pp)
    if (file.exists(archivo_est_temp)) estaciones_temp  <- read.csv(archivo_est_temp)
    if (file.exists(archivo_est_caudal)) estaciones_q    <- read.csv(archivo_est_caudal)

    if (is.null(estaciones_pp) || is.null(estaciones_temp)) {
      if (verbose) message("Tendencias [", name, "]: omitido (faltan estaciones pp o temp).")
      next
    }
    if (is.null(estaciones_q)) estaciones_q <- data.frame(codigo_nacional = character(), nombre_estacion = character(), lat = numeric(), lon = numeric())

    estaciones_trends_all <- process_metadata(estaciones_pp, estaciones_temp, estaciones_q)
    cols_fecha <- c("fecha", "year", "month", "day")
    codigos_en_series <- unique(c(
      names(pp)[!names(pp) %in% cols_fecha],
      names(t_max)[!names(t_max) %in% cols_fecha],
      names(t_min)[!names(t_min) %in% cols_fecha],
      if (!is.null(q)) names(q)[!names(q) %in% cols_fecha] else character(0)
    ))
    for (i in seq_len(nrow(estaciones_trends_all))) {
      c0 <- as.character(estaciones_trends_all$Codigo[i])
      if (c0 %in% codigos_en_series) next
      c1 <- paste0("X", gsub("-", ".", c0))
      if (c1 %in% codigos_en_series) estaciones_trends_all$Codigo[i] <- c1
    }
    estaciones_trends_all <- estaciones_trends_all %>% filter(Codigo %in% codigos_en_series)

    pp_trends    <- process_climate_data(pp)
    t_min_trends <- process_climate_data(t_min)
    t_max_trends <- process_climate_data(t_max)
    q_trends     <- if (!is.null(q)) process_climate_data(q) else NULL

    if (verbose) message("Tendencias [", name, "]: calculando tendencias e indicadores extremos...")
    analisis_extremos(pp_trends, t_min_trends, t_max_trends, q_trends, estaciones_trends_all, dir_output, name)
  }

  if (verbose) message("Salida guardada en: ", dir_output)
  invisible(dir_output)
}

# -----------------------------------------------------------------------------
# Uso desde MAIN.R (con setwd en caracterizacion_historica):
#   source("scripts/MAIN/5.tendencias_historica.R")
#   ejecutar_tendencias_historica(
#     calidad_por_variable = c(pp = 70, temp = 60, q = 70),
#     ano_inicio = 1990,
#     ano_fin    = 2024
#   )
# Salida: output/tendencias_historica/teniente_*.csv y salvador_*.csv
# -----------------------------------------------------------------------------
