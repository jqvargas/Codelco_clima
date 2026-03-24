# =============================================================================
# Script: comparar_series_bruta_vs_rellena.R
# Compara series brutas vs rellenas por estación de monitoreo.
# Match por nombre de archivo: división, variable, calidad, año inicio y fin.
# Uso: comparar_serie_estacion(codigo_nacional, variable, division, ano_inicio, ano_fin, calidad, ...)
#       Devuelve un ggplot con la serie bruta vs rellena para esa estación.
# =============================================================================

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# Rutas por defecto (carpeta series: brutas/ y rellenas/)
# -----------------------------------------------------------------------------
dir_series_default <- "output/output_seleccion_estaciones/series"

# -----------------------------------------------------------------------------
# Normalizar nombre de columna de estación en un data frame
# (readr puede leer "320028" como "X320028")
# -----------------------------------------------------------------------------
nombre_columna_estacion <- function(df, codigo_nacional) {
  cod <- as.character(codigo_nacional)
  nms <- names(df)
  if (cod %in% nms) return(cod)
  xcod <- paste0("X", cod)
  if (xcod %in% nms) return(xcod)
  NULL
}

# -----------------------------------------------------------------------------
# Cargar un CSV de serie y normalizar fecha; devolver data.frame con fecha + columnas numéricas
# -----------------------------------------------------------------------------
cargar_serie_csv <- function(archivo) {
  if (!file.exists(archivo)) stop("No existe el archivo: ", archivo)
  df <- read_csv(archivo, show_col_types = FALSE)
  df <- as.data.frame(df)
  nms <- names(df)
  for (par in list(c("Fecha", "fecha"), c("Year", "year"), c("Month", "month"), c("Day", "day"))) {
    if (par[1] %in% nms && !(par[2] %in% nms)) names(df)[names(df) == par[1]] <- par[2]
  }
  if (!"fecha" %in% names(df)) stop("El archivo debe tener columna 'fecha' o 'Fecha': ", archivo)
  df$fecha <- as.Date(df$fecha)
  df
}

# -----------------------------------------------------------------------------
# Construir rutas de archivo bruta y rellena según convención del proyecto
# variable: "pp", "temp", "q"
# division: "el_teniente" o "el_salvador"
# tipo_temp: "t_max" o "t_min" (solo cuando variable == "temp")
# tipo_rellena: "paso1_2", "imputeTS" o "missForest"
# -----------------------------------------------------------------------------
rutas_serie <- function(division, variable, calidad, ano_inicio, ano_fin, tipo_temp = "t_max", tipo_rellena = "paso1_2", dir_series = dir_series_default) {
  div_archivo <- if (division == "el_teniente") "teniente" else "salvador"
  if (variable == "pp") {
    base <- paste0("series_pp_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin)
  } else if (variable == "temp") {
    base <- paste0("series_", tipo_temp, "_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin)
  } else if (variable == "q") {
    base <- paste0("series_q_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin)
  } else {
    stop('variable debe ser "pp", "temp" o "q".')
  }
  sufijo_rellena <- switch(tipo_rellena,
    paso1_2 = "rellena_paso1_2.csv",
    imputeTS = "rellena_imputeTS.csv",
    missForest = "rellena_missForest.csv",
    paste0("rellena_", tipo_rellena, ".csv"))
  list(
    bruta   = file.path(dir_series, "brutas",   paste0(base, "_bruta.csv")),
    rellena = file.path(dir_series, "rellenas", paste0(base, "_", sufijo_rellena))
  )
}

# -----------------------------------------------------------------------------
# Comparar serie bruta vs rellena para una estación
# Devuelve un ggplot con ambas series (bruta y rellena) para el codigo_nacional dado.
#
# codigo_nacional: código de la estación (ej. "320028", "330021")
# variable: "pp", "temp" o "q"
# division: "el_teniente" o "el_salvador"
# ano_inicio, ano_fin: rango de años
# calidad: umbral de calidad usado (ej. 60 para pp, 70 para temp, 50 para q)
# tipo_temp: "t_max" o "t_min" (solo si variable == "temp")
# tipo_rellena: "paso1_2", "imputeTS" o "missForest"
# dir_series: carpeta base que contiene subcarpetas brutas/ y rellenas/
# -----------------------------------------------------------------------------
comparar_serie_estacion <- function(codigo_nacional,
                                    variable = c("pp", "temp", "q"),
                                    division = c("el_teniente", "el_salvador"),
                                    ano_inicio,
                                    ano_fin,
                                    calidad,
                                    tipo_temp = c("t_max", "t_min"),
                                    tipo_rellena = c("paso1_2", "imputeTS", "missForest"),
                                    dir_series = dir_series_default) {
  variable     <- match.arg(variable)
  division     <- match.arg(division)
  tipo_temp    <- match.arg(tipo_temp)
  tipo_rellena <- match.arg(tipo_rellena)

  rutas <- rutas_serie(division, variable, calidad, ano_inicio, ano_fin, tipo_temp = tipo_temp, tipo_rellena = tipo_rellena, dir_series = dir_series)
  if (!file.exists(rutas$bruta))   stop("No existe serie bruta: ", rutas$bruta)
  if (!file.exists(rutas$rellena)) stop("No existe serie rellena: ", rutas$rellena)

  df_bruta   <- cargar_serie_csv(rutas$bruta)
  df_rellena <- cargar_serie_csv(rutas$rellena)

  col_est <- nombre_columna_estacion(df_bruta, codigo_nacional)
  if (is.null(col_est)) {
    stop("La estación ", codigo_nacional, " no está presente en la serie bruta. ",
         "Columnas de estación: ", paste(setdiff(names(df_bruta), c("fecha", "year", "month", "day")), collapse = ", "))
  }
  col_rellena <- nombre_columna_estacion(df_rellena, codigo_nacional)
  if (is.null(col_rellena)) col_rellena <- col_est

  d_bruta   <- df_bruta[, c("fecha", col_est)]
  d_rellena <- df_rellena[, c("fecha", col_rellena)]
  names(d_bruta)[2]   <- "valor"
  names(d_rellena)[2] <- "valor"
  etiqueta_rellena <- switch(tipo_rellena,
  paso1_2 = "Rellena (paso1_2)",
  imputeTS = "Rellena (imputeTS)",
  missForest = "Rellena (missForest)",
  paste0("Rellena (", tipo_rellena, ")"))
  d_bruta$serie   <- "Bruta"
  d_rellena$serie <- etiqueta_rellena
  d <- bind_rows(d_rellena, d_bruta)  # Rellena primero para que Bruta se dibuje encima

  titulo_var <- switch(variable,
    pp = "Precipitación",
    temp = if (tipo_temp == "t_max") "Temperatura máxima" else "Temperatura mínima",
    q = "Caudal"
  )
  titulo <- paste0(titulo_var, " — Estación ", codigo_nacional, " (", division, ", ", ano_inicio, "-", ano_fin, ")\nBruta vs ", etiqueta_rellena)

  # Orden de capas: Rellena (línea fina, debajo) y Bruta (línea gruesa, encima) para que se distingan
  col_bruta <- "#1f77b4"
  col_rellena <- "#d62728"
  vals_color <- setNames(c(col_bruta, col_rellena), c("Bruta", etiqueta_rellena))
  p <- ggplot(d, aes(x = fecha, y = valor, color = serie)) +
    geom_line(data = filter(d, serie == etiqueta_rellena), linewidth = 0.6, alpha = 0.7) +
    geom_line(data = filter(d, serie == "Bruta"),   linewidth = 1.0, alpha = 1) +
    scale_color_manual(values = vals_color, limits = c("Bruta", etiqueta_rellena)) +
    labs(title = titulo, x = "Fecha", y = titulo_var, color = "Serie") +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(size = 11, hjust = 0.5)
    )

  p
}

# -----------------------------------------------------------------------------
# Ejemplo de uso (con setwd en caracterizacion_historica)
# -----------------------------------------------------------------------------
#
# source("scripts/comparar_series_bruta_vs_rellena.R")
#
# # Precipitación, El Salvador, estación 320015, calidad 60, 1990-2024
# p <- comparar_serie_estacion("320015", variable = "pp", division = "el_salvador",
#                              ano_inicio = 1990, ano_fin = 2024, calidad = 60)
# print(p)
#
# # Temperatura máxima, El Teniente, estación 330021, calidad 70
# p <- comparar_serie_estacion("330021", variable = "temp", division = "el_teniente",
#                              ano_inicio = 1990, ano_fin = 2024, calidad = 70, tipo_temp = "t_max")
# print(p)
#
# # Caudal, El Salvador
# p <- comparar_serie_estacion("06016004-K", variable = "q", division = "el_salvador",
#                              ano_inicio = 1990, ano_fin = 2024, calidad = 50)
# print(p)
#
# # Bruta vs rellena imputeTS (en lugar de paso1_2)
# p <- comparar_serie_estacion("320015", variable = "pp", division = "el_salvador",
#                              ano_inicio = 1990, ano_fin = 2024, calidad = 60, tipo_rellena = "imputeTS")
# print(p)
