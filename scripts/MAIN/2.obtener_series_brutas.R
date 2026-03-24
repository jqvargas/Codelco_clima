source("backend/connection.R")
source("backend/funciones_utilidad.R")
source("backend/librerias.R")
source("backend/queries.R")

# =============================================================================
# Obtener series brutas para una variable, división y calidad.
# Archivos de estaciones: estaciones_{variable}_el_{division}_{calidad}_{ano_inicio}_{ano_fin}.csv
# Ejemplo: estaciones_caudal_el_salvador_70_1990_2024.csv
# =============================================================================
#
# variable:   "pp" (precipitación), "temp" (temperatura), "q" (caudal)
# division:   "el_teniente", "el_salvador", "teniente", "salvador", "el teniente", "el salvador"
# calidad:    umbral de calidad usado en filtrar_estaciones (ej. 0, 70)
# ano_inicio, ano_fin: rango de años
# dir_estaciones: carpeta donde están los CSV de estaciones
# dir_series: carpeta donde se guardan las series brutas (se crea si no existe)
#
obtener_series_brutas <- function(connection,
                                 variable = c("pp", "temp", "q"),
                                 division,
                                 calidad,
                                 ano_inicio,
                                 ano_fin,
                                 dir_estaciones = "output/output_seleccion_estaciones/estaciones",
                                 dir_series = "output/output_seleccion_estaciones/series/brutas") {

  variable <- match.arg(variable)

  # Normalizar division a "teniente" o "salvador" para el nombre de archivo
  div <- tolower(trimws(as.character(division)))
  div_archivo <- if (div %in% c("el_teniente", "teniente", "el teniente")) {
    "teniente"
  } else if (div %in% c("el_salvador", "salvador", "el salvador")) {
    "salvador"
  } else {
    stop('division debe ser "el_teniente", "el_salvador", "teniente", "salvador", "el teniente" o "el salvador"')
  }

  # Nombre en archivo: pp y temp coinciden con variable; caudal se guarda como "caudal" (no "q")
  variable_nombre_archivo <- if (variable == "q") "caudal" else variable
  archivo_estaciones <- file.path(
    dir_estaciones,
    paste0("estaciones_", variable_nombre_archivo, "_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin, ".csv")
  )

  if (!file.exists(archivo_estaciones)) {
    stop("No se encontró el archivo de estaciones: ", archivo_estaciones)
  }

  if (!dir.exists(dir_series)) {
    dir.create(dir_series, recursive = TRUE)
  }

  estaciones <- read.csv(archivo_estaciones)
  codigos <- estaciones$codigo_nacional

  if (variable == "pp") {
    series <- obtener_observaciones_por_variable_codigo(connection, "pp", "observacion_final", ano_inicio, ano_fin, codigos)
    series_long <- preparar_datos_ancho(series, "pp")
    archivo_salida <- file.path(dir_series, paste0("series_pp_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
    write.csv(series_long, archivo_salida, row.names = FALSE)
    return(invisible(list(pp = series_long)))
  }

  if (variable == "temp") {
    series_t_max <- obtener_observaciones_por_variable_codigo(connection, "t_max", "observacion_final", ano_inicio, ano_fin, codigos)
    series_t_min <- obtener_observaciones_por_variable_codigo(connection, "t_min", "observacion_final", ano_inicio, ano_fin, codigos)
    series_t_max_long <- preparar_datos_ancho(series_t_max, "t_max")
    series_t_min_long <- preparar_datos_ancho(series_t_min, "t_min")
    archivo_t_max <- file.path(dir_series, paste0("series_t_max_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
    archivo_t_min <- file.path(dir_series, paste0("series_t_min_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
    write.csv(series_t_max_long, archivo_t_max, row.names = FALSE)
    write.csv(series_t_min_long, archivo_t_min, row.names = FALSE)
    return(invisible(list(t_max = series_t_max_long, t_min = series_t_min_long)))
  }

  if (variable == "q") {
    series <- obtener_observaciones_por_variable_codigo(connection, "q", "observacion_final", ano_inicio, ano_fin, codigos)
    series_long <- preparar_datos_ancho(series, "q")
    archivo_salida <- file.path(dir_series, paste0("series_q_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin, "_bruta.csv"))
    write.csv(series_long, archivo_salida, row.names = FALSE)
    return(invisible(list(q = series_long)))
  }

  invisible(NULL)
}

# =============================================================================
# Ejemplo de uso (desde MAIN o con setwd en caracterizacion_historica):
# =============================================================================
#
# connection <- connect_to_db("backend/ATT15580.env")
#
# # Una combinación
# obtener_series_brutas(connection, variable = "q", division = "el_salvador", calidad = 70, ano_inicio = 1990, ano_fin = 2024)
#
# # Varias (por ejemplo todas las combinaciones)
# for (var in c("pp", "temp", "q")) {
#   for (div in c("el_teniente", "el_salvador")) {
#     obtener_series_brutas(connection, variable = var, division = div, calidad = 70, ano_inicio = 1990, ano_fin = 2024)
#   }
# }
