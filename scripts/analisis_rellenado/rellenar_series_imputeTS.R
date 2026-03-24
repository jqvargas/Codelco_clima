# =============================================================================
# Script: rellenar_series_imputeTS.R
# Rellena series brutas usando la librería imputeTS (validada para series temporales).
# Aplica un método de imputación por columna (estación): interpolación, Kalman,
# descomposición estacional, etc. Adecuado para temperatura, precipitación y caudal.
#
# Requiere: install.packages("imputeTS")
# Uso: rellenar_una_serie_imputeTS(archivo_bruta, dir_salida, metodo = "interpolation")
#      ejecutar_rellenado_imputeTS(calidad_por_variable, ano_inicio, ano_fin, ...)
#
# --- Validez metodológica (imputeTS, R Journal 2017) ---
# imputeTS está pensada para series temporales UNIVARIADAS (una variable en el tiempo).
# Aquí se aplica por columna: cada estación es una serie univariada, así que el uso es correcto.
# Utiliza dependencias TEMPORALES (valores anteriores/posteriores en el tiempo), no correlación
# entre estaciones. Es metodológicamente válida para:
#   - Temperatura: muy adecuada (suave, estacional); Kalman o interpolación suelen ir bien.
#   - Caudal: adecuada (suave, estacional); Kalman, seadec o interpolación.
#   - Precipitación: aplicable (muchos ceros y picos); interpolación lineal o LOCF son habituales
#     en hidrología para rellenar huecos; para series muy irregulares el resultado es más aproximado.
# Métodos que usa (según opción): interpolación lineal, Kalman (modelo estructural/ARIMA),
# descomposición estacional (seadec), LOCF, media móvil. No hay un "mejor" universal; depende
# de la serie (estacionalidad, tendencia). El artículo recomienda probar kalman, interpolation o seadec.
# =============================================================================

library(readr)
library(lubridate)

# Comprobar e cargar imputeTS
if (!requireNamespace("imputeTS", quietly = TRUE)) {
  stop("Se necesita el paquete 'imputeTS'. Instale con: install.packages(\"imputeTS\")")
}
library(imputeTS)

# -----------------------------------------------------------------------------
# Rutas por defecto
# -----------------------------------------------------------------------------
dir_series_default <- "output/output_seleccion_estaciones/series"

# -----------------------------------------------------------------------------
# Métodos imputeTS (validados para series temporales univariadas)
# -----------------------------------------------------------------------------
# interpolation: interpolación lineal entre observaciones (recomendado general).
# kalman:       suavizado Kalman / modelos de espacio de estados (temp, caudal).
# seadec:       descomposición estacional + interpolación (series con estacionalidad).
# locf:         última observación llevada adelante (conservador, pp con muchos ceros).
# ma:           media móvil ponderada (suavizado).
#
metodos_imputeTS <- list(
  interpolation = function(x) imputeTS::na_interpolation(x, option = "linear", maxgap = Inf),
  kalman        = function(x) imputeTS::na_kalman(x, model = "auto"),
  seadec        = function(x) imputeTS::na_seadec(x, algorithm = "interpolation"),
  locf          = function(x) imputeTS::na_locf(x, option = "locf", na_remaining = "rev"),
  ma            = function(x) imputeTS::na_ma(x, k = 4, weighting = "exponential")
)

# -----------------------------------------------------------------------------
# Cargar serie bruta y detectar columnas de fecha y de estaciones
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
  cols_fecha <- c("fecha", "year", "month", "day")
  cols_fecha <- intersect(cols_fecha, names(df))
  estaciones <- setdiff(names(df), cols_fecha)
  estaciones <- estaciones[vapply(estaciones, function(c) is.numeric(df[[c]]), logical(1))]
  list(df = df, cols_fecha = cols_fecha, estaciones = estaciones)
}

# -----------------------------------------------------------------------------
# Rellenar una serie bruta con imputeTS y guardar
# archivo_bruta: ruta al CSV bruto
# dir_salida:   carpeta donde guardar (ej. .../series/rellenas)
# metodo:       "interpolation", "kalman", "seadec", "locf", "ma"
# sufijo:       sufijo del archivo de salida (default "rellena_imputeTS.csv")
# -----------------------------------------------------------------------------
rellenar_una_serie_imputeTS <- function(archivo_bruta,
                                        dir_salida = NULL,
                                        metodo = c("interpolation", "kalman", "seadec", "locf", "ma"),
                                        sufijo = "rellena_imputeTS.csv") {
  metodo <- match.arg(metodo)
  if (is.null(dir_salida)) dir_salida <- dirname(dirname(archivo_bruta))

  dat <- cargar_serie_bruta(archivo_bruta)
  df <- dat$df
  estaciones <- dat$estaciones
  if (length(estaciones) == 0) stop("No se encontraron columnas numéricas de estaciones en ", archivo_bruta)

  fun_impute <- metodos_imputeTS[[metodo]]
  for (col in estaciones) {
    x <- df[[col]]
    if (!any(is.na(x))) next
    if (all(is.na(x))) next  # columna sin datos: se deja igual
    df[[col]] <- tryCatch(fun_impute(x), error = function(e) x)
  }

  if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)
  nombre_base <- gsub("_bruta\\.csv$", "", basename(archivo_bruta))
  archivo_salida <- file.path(dir_salida, paste0(nombre_base, "_", sufijo))
  write_csv(df, archivo_salida)
  message("Guardado: ", archivo_salida, " (método: ", metodo, ")")
  invisible(list(serie_rellena = df, archivo_salida = archivo_salida))
}

# -----------------------------------------------------------------------------
# Ejecutar rellenado con imputeTS para todas las series (pp, temp, q × divisiones)
# Usa la misma convención de nombres que el proyecto (division, variable, calidad, años).
# Salida: archivos *_rellena_imputeTS.csv en dir_series/rellenas/
# -----------------------------------------------------------------------------
ejecutar_rellenado_imputeTS <- function(dir_series_brutas   = file.path(dir_series_default, "brutas"),
                                       dir_series_rellenas = file.path(dir_series_default, "rellenas"),
                                       calidad_por_variable = c(pp = 60, temp = 70, q = 50),
                                       ano_inicio,
                                       ano_fin,
                                       divisiones = c("el_teniente", "el_salvador"),
                                       metodo = c("interpolation", "kalman", "seadec", "locf", "ma"),
                                       sufijo = "rellena_imputeTS.csv") {
  metodo <- match.arg(metodo)
  for (var in c("pp", "temp", "q")) {
    cal <- calidad_por_variable[var]
    if (is.na(cal)) next
    for (div in divisiones) {
      div_archivo <- if (div == "el_teniente") "teniente" else "salvador"
      if (var == "pp") {
        archivo_bruta <- file.path(dir_series_brutas, paste0("series_pp_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        if (file.exists(archivo_bruta)) {
          rellenar_una_serie_imputeTS(archivo_bruta, dir_salida = dir_series_rellenas, metodo = metodo, sufijo = sufijo)
        }
      } else if (var == "temp") {
        for (serie in c("t_max", "t_min")) {
          archivo_bruta <- file.path(dir_series_brutas, paste0("series_", serie, "_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
          if (file.exists(archivo_bruta)) {
            rellenar_una_serie_imputeTS(archivo_bruta, dir_salida = dir_series_rellenas, metodo = metodo, sufijo = sufijo)
          }
        }
      } else {
        archivo_bruta <- file.path(dir_series_brutas, paste0("series_q_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        if (file.exists(archivo_bruta)) {
          rellenar_una_serie_imputeTS(archivo_bruta, dir_salida = dir_series_rellenas, metodo = metodo, sufijo = sufijo)
        }
      }
    }
  }
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Ejemplo de uso (con setwd en caracterizacion_historica)
# -----------------------------------------------------------------------------
#
# # Instalar dependencia una vez:
# # install.packages("imputeTS")
#
# source("scripts/analisis_rellenado/rellenar_series_imputeTS.R")
#
# # Una sola serie (interpolación lineal)
# rellenar_una_serie_imputeTS(
#   "output/output_seleccion_estaciones/series/brutas/series_pp_el_salvador_60_1990_2024_bruta.csv",
#   dir_salida = "output/output_seleccion_estaciones/series/rellenas",
#   metodo = "interpolation"
# )
#
# # Todas las series (mismo período y calidad que MAIN)
# ejecutar_rellenado_imputeTS(
#   calidad_por_variable = c(pp = 60, temp = 70, q = 50),
#   ano_inicio = 1990,
#   ano_fin    = 2024,
#   metodo     = "interpolation"   # o "kalman", "seadec", "locf", "ma"
# )
#
# # Archivos generados: *_rellena_imputeTS.csv (no pisan *_rellena_paso1_2.csv)
#
