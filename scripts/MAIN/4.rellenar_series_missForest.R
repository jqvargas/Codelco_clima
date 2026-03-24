# =============================================================================
# Script: 3.rellenar_series_missForest.R
# Rellena series brutas usando la librería missForest (imputación por random forest).
# Aprovecha correlaciones entre estaciones y, opcionalmente, variables de tiempo
# (año, mes, día) para imputar valores faltantes. Multivariado: cada columna
# se predice usando el resto de columnas y covariables.
#
# Requiere: install.packages("missForest")
# Uso: rellenar_una_serie_missForest(archivo_bruta, dir_salida, ...)
#      ejecutar_rellenado_missForest(calidad_por_variable, ano_inicio, ano_fin, ...)
#
# --- Validez metodológica ---
# missForest (Stekhoven & Bühlmann, 2012) imputa con random forests de forma
# no paramétrica, capturando relaciones no lineales entre variables. Aquí las
# variables son las estaciones (columnas) y opcionalmente año/mes/día. Es adecuada
# para temperatura, precipitación y caudal cuando hay varias estaciones con
# datos correlacionados. Puede ser más lenta que imputeTS en series largas.
# Salida: archivos *_rellena_missForest.csv en dir_series/rellenas/
# =============================================================================

library(readr)
library(lubridate)

# Comprobar y cargar missForest
if (!requireNamespace("missForest", quietly = TRUE)) {
  stop("Se necesita el paquete 'missForest'. Instale con: install.packages(\"missForest\")")
}
library(missForest)

# -----------------------------------------------------------------------------
# Rutas por defecto
# -----------------------------------------------------------------------------
dir_series_default <- "output/output_seleccion_estaciones/series"

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
  if (!"year" %in% names(df))  df$year  <- as.integer(year(df$fecha))
  if (!"month" %in% names(df)) df$month <- as.integer(month(df$fecha))
  if (!"day" %in% names(df))   df$day   <- as.integer(day(df$fecha))
  cols_fecha <- c("fecha", "year", "month", "day")
  cols_fecha <- intersect(cols_fecha, names(df))
  estaciones <- setdiff(names(df), cols_fecha)
  estaciones <- estaciones[vapply(estaciones, function(c) is.numeric(df[[c]]), logical(1))]
  list(df = df, cols_fecha = cols_fecha, estaciones = estaciones)
}

# -----------------------------------------------------------------------------
# Rellenar una serie bruta con missForest y guardar
# -----------------------------------------------------------------------------
rellenar_una_serie_missForest <- function(archivo_bruta,
                                          dir_salida = NULL,
                                          ntree = 100,
                                          maxiter = 10,
                                          usar_tiempo = TRUE,
                                          verbose = TRUE,
                                          sufijo = "rellena_missForest.csv") {
  if (is.null(dir_salida)) dir_salida <- dirname(dirname(archivo_bruta))
  if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

  dat <- cargar_serie_bruta(archivo_bruta)
  df <- dat$df
  estaciones <- dat$estaciones
  if (length(estaciones) == 0) stop("No se encontraron columnas numéricas de estaciones en ", archivo_bruta)

  # Matriz para missForest: estaciones + opcionalmente año/mes/día (como predictores)
  if (usar_tiempo && all(c("year", "month", "day") %in% names(df))) {
    xtab <- df[, c("year", "month", "day", estaciones)]
  } else {
    xtab <- df[, estaciones, drop = FALSE]
  }

  if (!any(is.na(xtab))) {
    message("Sin valores faltantes en ", basename(archivo_bruta), "; se guarda copia con sufijo indicado.")
    nombre_base <- gsub("_bruta\\.csv$", "", basename(archivo_bruta))
    archivo_salida <- file.path(dir_salida, paste0(nombre_base, "_", sufijo))
    write_csv(df, archivo_salida)
    return(invisible(list(serie_rellena = df, archivo_salida = archivo_salida)))
  }

  # Imputación con missForest (solo columnas numéricas; year/month/day son continuos)
  res <- tryCatch(
    missForest::missForest(
      xtab,
      maxiter = maxiter,
      ntree = ntree,
      verbose = verbose,
      variablewise = FALSE
    ),
    error = function(e) {
      stop("missForest falló en ", archivo_bruta, ": ", conditionMessage(e))
    }
  )

  # Pegar imputaciones de vuelta al data frame original (solo columnas estación)
  if (usar_tiempo && all(c("year", "month", "day") %in% names(df))) {
    df[, estaciones] <- res$ximp[, estaciones, drop = FALSE]
  } else {
    df[, estaciones] <- res$ximp
  }

  if (verbose && !is.null(res$OOBerror)) {
    if (is.vector(res$OOBerror)) {
      message("  OOB NRMSE (missForest): ", round(res$OOBerror, 4))
    } else {
      message("  OOB error (missForest): ", paste(round(res$OOBerror, 4), collapse = ", "))
    }
  }

  nombre_base <- gsub("_bruta\\.csv$", "", basename(archivo_bruta))
  archivo_salida <- file.path(dir_salida, paste0(nombre_base, "_", sufijo))
  write_csv(df, archivo_salida)
  message("Guardado: ", archivo_salida)
  invisible(list(serie_rellena = df, archivo_salida = archivo_salida, OOBerror = res$OOBerror))
}

# -----------------------------------------------------------------------------
# Ejecutar rellenado con missForest para todas las series (pp, temp, q × divisiones)
# -----------------------------------------------------------------------------
ejecutar_rellenado_missForest <- function(dir_series_brutas   = file.path(dir_series_default, "brutas"),
                                          dir_series_rellenas = file.path(dir_series_default, "rellenas"),
                                          calidad_por_variable, # Definido en MAIN.R — no redefinir aquí
                                          ano_inicio,
                                          ano_fin,
                                          divisiones = c("el_teniente", "el_salvador"),
                                          ntree = 100,
                                          maxiter = 10,
                                          usar_tiempo = TRUE,
                                          verbose = TRUE,
                                          sufijo = "rellena_missForest.csv") {
  for (var in c("pp", "temp", "q")) {
    cal <- calidad_por_variable[var]
    if (is.na(cal)) next
    for (div in divisiones) {
      div_archivo <- if (div == "el_teniente") "teniente" else "salvador"
      if (var == "pp") {
        archivo_bruta <- file.path(dir_series_brutas, paste0("series_pp_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        if (file.exists(archivo_bruta)) {
          rellenar_una_serie_missForest(archivo_bruta, dir_salida = dir_series_rellenas,
                                       ntree = ntree, maxiter = maxiter, usar_tiempo = usar_tiempo,
                                       verbose = verbose, sufijo = sufijo)
        }
      } else if (var == "temp") {
        for (serie in c("t_max", "t_min")) {
          archivo_bruta <- file.path(dir_series_brutas, paste0("series_", serie, "_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
          if (file.exists(archivo_bruta)) {
            rellenar_una_serie_missForest(archivo_bruta, dir_salida = dir_series_rellenas,
                                         ntree = ntree, maxiter = maxiter, usar_tiempo = usar_tiempo,
                                         verbose = verbose, sufijo = sufijo)
          }
        }
      } else {
        archivo_bruta <- file.path(dir_series_brutas, paste0("series_q_el_", div_archivo, "_", cal, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
        if (file.exists(archivo_bruta)) {
          rellenar_una_serie_missForest(archivo_bruta, dir_salida = dir_series_rellenas,
                                       ntree = ntree, maxiter = maxiter, usar_tiempo = usar_tiempo,
                                       verbose = verbose, sufijo = sufijo)
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
# source("scripts/MAIN/3.rellenar_series_missForest.R")
#
# ejecutar_rellenado_missForest(
#   calidad_por_variable = c(pp = 60, temp = 70, q = 50),
#   ano_inicio = 1990,
#   ano_fin    = 2024,
#   ntree      = 100,
#   usar_tiempo = TRUE
# )
