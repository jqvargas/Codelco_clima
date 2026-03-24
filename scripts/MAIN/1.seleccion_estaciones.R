# Cargar funciones y librerías
source("backend/connection.R")
source("backend/funciones_utilidad.R")
source("backend/librerias.R")
source("backend/queries.R")

# variable: "pp" (precipitación), "temp" (temperatura), "q" (caudal) o "all" (las tres; por defecto)
filtrar_estaciones <- function(connection, calidad_minima, ano_inicio, ano_fin, variable = c("all", "pp", "temp", "q")) {

  variable <- match.arg(variable)

  # Directorio de salida: crear si no existe
  dir_estaciones <- "output/output_seleccion_estaciones/estaciones"
  if (!dir.exists(dir_estaciones)) dir.create(dir_estaciones, recursive = TRUE)

  # Sufijo para nombres de salida según calidad_minima
  sufijo_cal <- paste0("_", as.character(calidad_minima))

  # Regiones: El Teniente (6, 13, 5), Salvador (3, 2)
  cod_el_teniente <- c(6, 13, 5)
  cod_el_salvador <- c(3, 2)

  if (variable == "pp" || variable == "all") {
    # 1. Estaciones de precipitación
    estaciones_pp <- calcular_metricas_calidad_multi(connection, "pp", ano_inicio, ano_fin)
    estaciones_pp <- estaciones_pp %>%
      arrange(-calidad_pp) %>%
      filter(calidad_pp >= calidad_minima)

    estaciones_pp_el_teniente <- estaciones_pp %>% filter(cod_reg %in% cod_el_teniente)
    estaciones_pp_el_salvador <- estaciones_pp %>% filter(cod_reg %in% cod_el_salvador)

    write.csv(estaciones_pp_el_teniente, file.path(dir_estaciones, paste0("estaciones_pp_el_teniente", sufijo_cal, "_", ano_inicio, "_", ano_fin, ".csv")), row.names = FALSE)
    write.csv(estaciones_pp_el_salvador, file.path(dir_estaciones, paste0("estaciones_pp_el_salvador", sufijo_cal, "_", ano_inicio, "_", ano_fin, ".csv")), row.names = FALSE)

    if (variable == "pp") {
      return(setNames(
        list(estaciones_pp_el_teniente, estaciones_pp_el_salvador),
        paste0(c("estaciones_pp_el_teniente", "estaciones_pp_el_salvador"), sufijo_cal)
      ))
    }
  }

  if (variable == "temp" || variable == "all") {
    # 2. Estaciones de temperatura
    estaciones_temp <- calcular_metricas_calidad_multi(connection, c("t_min", "t_max"), ano_inicio, ano_fin)
    estaciones_temp <- estaciones_temp %>%
      arrange(-calidad_t_max) %>%
      filter(calidad_t_max > calidad_minima & calidad_t_min > calidad_minima)

    estaciones_temp_el_teniente <- estaciones_temp %>% filter(cod_reg %in% cod_el_teniente)
    estaciones_temp_el_salvador <- estaciones_temp %>% filter(cod_reg %in% cod_el_salvador)

    write.csv(estaciones_temp_el_teniente, file.path(dir_estaciones, paste0("estaciones_temp_el_teniente", sufijo_cal, "_", ano_inicio, "_", ano_fin, ".csv")), row.names = FALSE)
    write.csv(estaciones_temp_el_salvador, file.path(dir_estaciones, paste0("estaciones_temp_el_salvador", sufijo_cal, "_", ano_inicio, "_", ano_fin, ".csv")), row.names = FALSE)

    if (variable == "temp") {
      return(setNames(
        list(estaciones_temp_el_teniente, estaciones_temp_el_salvador),
        paste0(c("estaciones_temp_el_teniente", "estaciones_temp_el_salvador"), sufijo_cal)
      ))
    }
  }

  if (variable == "q" || variable == "all") {
    # 3. Estaciones de caudal
    estaciones_caudal <- calcular_metricas_calidad_multi(connection, "q", ano_inicio, ano_fin)
    estaciones_caudal <- estaciones_caudal %>%
      arrange(-calidad_q) %>%
      filter(calidad_q >= calidad_minima)

    estaciones_caudal_el_teniente <- estaciones_caudal %>% filter(cod_reg %in% cod_el_teniente)
    estaciones_caudal_el_salvador <- estaciones_caudal %>% filter(cod_reg %in% cod_el_salvador)

    write.csv(estaciones_caudal_el_teniente, file.path(dir_estaciones, paste0("estaciones_caudal_el_teniente", sufijo_cal, "_", ano_inicio, "_", ano_fin, ".csv")), row.names = FALSE)
    write.csv(estaciones_caudal_el_salvador, file.path(dir_estaciones, paste0("estaciones_caudal_el_salvador", sufijo_cal, "_", ano_inicio, "_", ano_fin, ".csv")), row.names = FALSE)

    if (variable == "q") {
      return(setNames(
        list(estaciones_caudal_el_teniente, estaciones_caudal_el_salvador),
        paste0(c("estaciones_caudal_el_teniente", "estaciones_caudal_el_salvador"), sufijo_cal)
      ))
    }
  }

  if (variable == "all") {
    return(setNames(
      list(
        estaciones_pp_el_teniente, estaciones_pp_el_salvador,
        estaciones_temp_el_teniente, estaciones_temp_el_salvador,
        estaciones_caudal_el_teniente, estaciones_caudal_el_salvador
      ),
      paste0(c(
        "estaciones_pp_el_teniente", "estaciones_pp_el_salvador",
        "estaciones_temp_el_teniente", "estaciones_temp_el_salvador",
        "estaciones_caudal_el_teniente", "estaciones_caudal_el_salvador"
      ), sufijo_cal)
    ))
  }
  invisible(NULL)
}

# Ejecución desde línea de comandos (ejecutar desde caracterizacion_historica):
#   Rscript scripts/MAIN/1.seleccion_estaciones.R pp 70 1992 2021
# Argumentos: variable, calidad_minima, ano_inicio, ano_fin
#
# NOTA (entorno aislado): cuando se llama via Rscript el entorno global de MAIN.R
# no está disponible, por lo que ano_inicio y ano_fin se leen desde los argumentos
# de la línea de comandos. Estas asignaciones son LOCALES a este bloque y no
# afectan al entorno global cuando el script se ejecuta desde MAIN.R (modo interactivo).
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 4) {
    variable    <- args[1]
    calidad     <- as.numeric(args[2])
    ano_inicio  <- as.numeric(args[3])  # local CLI — valor canónico definido en MAIN.R
    ano_fin     <- as.numeric(args[4])  # local CLI — valor canónico definido en MAIN.R
    conn        <- connect_to_db("backend/ATT15580.env")
    filtrar_estaciones(conn, calidad, ano_inicio, ano_fin, variable = variable)
  } else {
    message("Uso: Rscript scripts/MAIN/1.seleccion_estaciones.R <variable> <calidad_minima> <ano_inicio> <ano_fin>")
    message('  variable: pp | temp | q | all')
  }
}
# Estaciones exportadas a SIG y elegir por localización.

