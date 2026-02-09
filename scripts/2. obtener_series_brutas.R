source("backend/connection.R")
source("backend/funciones_utilidad.R")
source("backend/librerias.R")
source("backend/queries.R")

#conectar a la base de datos CCG-UC
connection <- connect_to_db("backend/ATT15580.env")

#0. definir rango años para elegir estaciones
ano_inicio <- 1990
ano_fin <- 2024

#luego de haber elegido las estaciones, las cargamos de nuevo y obtenemos las series. 

#1. Precipitación - El Teniente
estaciones_pp_el_teniente <- read.csv(paste0("output/output_seleccion_estaciones/estaciones/estaciones_pp_el_teniente_cal_", ano_inicio, "_", ano_fin, ".csv"))
codigos_pp_el_teniente <- estaciones_pp_el_teniente$codigo_nacional
series_pp_el_teniente <- obtener_observaciones_por_variable_codigo(connection, "pp", "observacion_final",ano_inicio, ano_fin, codigos_pp_el_teniente)
series_pp_el_teniente_long <- preparar_datos_ancho(series_pp_el_teniente, "pp")
write.csv(series_pp_el_teniente_long, paste0("output/output_seleccion_estaciones/series/brutas/series_pp_el_teniente_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)

#2. Precipitación - El Salvador
estaciones_pp_el_salvador <- read.csv(paste0("output/output_seleccion_estaciones/estaciones/estaciones_pp_el_salvador_cal_", ano_inicio, "_", ano_fin, ".csv"))
codigos_pp_el_salvador <- estaciones_pp_el_salvador$codigo_nacional
series_pp_el_salvador <- obtener_observaciones_por_variable_codigo(connection, "pp", "observacion_final",ano_inicio, ano_fin, codigos_pp_el_salvador)
series_pp_el_salvador_long <- preparar_datos_ancho(series_pp_el_salvador, "pp")
write.csv(series_pp_el_salvador_long, paste0("output/output_seleccion_estaciones/series/brutas/series_pp_el_salvador_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)

#3. Temperatura - El Teniente
estaciones_temp_el_teniente <- read.csv(paste0("output/output_seleccion_estaciones/estaciones/estaciones_temp_el_teniente_cal_", ano_inicio, "_", ano_fin, ".csv"))
codigos_temp_el_teniente <- estaciones_temp_el_teniente$codigo_nacional
series_t_max_el_teniente <- obtener_observaciones_por_variable_codigo(connection, "t_max", "observacion_final",ano_inicio, ano_fin, codigos_temp_el_teniente)
series_t_min_el_teniente <- obtener_observaciones_por_variable_codigo(connection, "t_min", "observacion_final",ano_inicio, ano_fin, codigos_temp_el_teniente)
series_t_max_el_teniente_long <- preparar_datos_ancho(series_t_max_el_teniente, "t_max")
series_t_min_el_teniente_long <- preparar_datos_ancho(series_t_min_el_teniente, "t_min")
write.csv(series_t_max_el_teniente_long, paste0("output/output_seleccion_estaciones/series/brutas/series_t_max_el_teniente_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)
write.csv(series_t_min_el_teniente_long, paste0("output/output_seleccion_estaciones/series/brutas/series_t_min_el_teniente_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)

#4. Temperatura - El Salvador
estaciones_temp_el_salvador <- read.csv(paste0("output/output_seleccion_estaciones/estaciones/estaciones_temp_el_salvador_cal_", ano_inicio, "_", ano_fin, ".csv"))
codigos_temp_el_salvador <- estaciones_temp_el_salvador$codigo_nacional
series_t_max_el_salvador <- obtener_observaciones_por_variable_codigo(connection, "t_max", "observacion_final",ano_inicio, ano_fin, codigos_temp_el_salvador)
series_t_min_el_salvador <- obtener_observaciones_por_variable_codigo(connection, "t_min", "observacion_final",ano_inicio, ano_fin, codigos_temp_el_salvador)
series_t_max_el_salvador_long <- preparar_datos_ancho(series_t_max_el_salvador, "t_max")
series_t_min_el_salvador_long <- preparar_datos_ancho(series_t_min_el_salvador, "t_min")
write.csv(series_t_max_el_salvador_long, paste0("output/output_seleccion_estaciones/series/brutas/series_t_max_el_salvador_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)
write.csv(series_t_min_el_salvador_long, paste0("output/output_seleccion_estaciones/series/brutas/series_t_min_el_salvador_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)

#5. Caudal - El Teniente
estaciones_caudal_el_teniente <- read.csv(paste0("output/output_seleccion_estaciones/estaciones/estaciones_caudal_el_teniente_cal_", ano_inicio, "_", ano_fin, ".csv"))
codigos_caudal_el_teniente <- estaciones_caudal_el_teniente$codigo_nacional
series_caudal_el_teniente <- obtener_observaciones_por_variable_codigo(connection, "q", "observacion_final",ano_inicio, ano_fin, codigos_caudal_el_teniente)
series_caudal_el_teniente_long <- preparar_datos_ancho(series_caudal_el_teniente, "q")
write.csv(series_caudal_el_teniente_long, paste0("output/output_seleccion_estaciones/series/brutas/series_q_el_teniente_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)

#6. Caudal - El Salvador
estaciones_caudal_el_salvador <- read.csv(paste0("output/output_seleccion_estaciones/estaciones/estaciones_caudal_el_salvador_cal_", ano_inicio, "_", ano_fin, ".csv"))
codigos_caudal_el_salvador <- estaciones_caudal_el_salvador$codigo_nacional
series_caudal_el_salvador <- obtener_observaciones_por_variable_codigo(connection, "q", "observacion_final",ano_inicio, ano_fin, codigos_caudal_el_salvador)
series_caudal_el_salvador_long <- preparar_datos_ancho(series_caudal_el_salvador, "q")
write.csv(series_caudal_el_salvador_long, paste0("output/output_seleccion_estaciones/series/brutas/series_q_el_salvador_", ano_inicio, "_", ano_fin, "_bruta.csv"), row.names = FALSE)




#pp_parral <- obtener_observaciones_por_variable_codigo(connection, "pp", 2023, 2023, "07345001-2")
#write.csv(pp_parral, "pp_parral_2023.csv", row.names = FALSE)


