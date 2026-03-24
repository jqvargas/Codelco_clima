source("backend/connection.R")
source("backend/funciones_utilidad.R")
source("backend/librerias.R")
source("backend/queries.R")

#conectar a la base de datos CCG-UC
connection <- connect_to_db("backend/ATT15580.env")

#0. definir rango años para elegir estaciones
ano_inicio <- 1990
ano_fin <- 2024

calidad_por_variable <- c(pp = 70, temp = 60, q = 70)

#1 definir estaciones de precipitacion
estaciones_pp <- calcular_metricas_calidad_multi(connection, "pp", ano_inicio, ano_fin)
#FILTRO DE CALIDAD
estaciones_pp <- estaciones_pp %>% arrange(-calidad_pp) %>% filter(calidad_pp >= calidad_por_variable[1])
#Region antofagasta. cod_reg = 2. 
#Podemos partir así:
#El teniente: (Ohiggins(6), metropolitana(13), Valparaíso(5))
#Salvador: (Atacama(3), Antofagasta (2))

#Y vamos acotando
#estaciones_pp <- estaciones_pp %>%select(-id)
estaciones_pp_el_teniente <- estaciones_pp %>% filter(cod_reg %in% c(6,13,5))
estaciones_pp_el_salvador <- estaciones_pp %>% filter(cod_reg %in% c(3,2))

write.csv(estaciones_pp_el_teniente, paste0("output/output_seleccion_estaciones/estaciones/estaciones_pp_el_teniente_cal_", calidad_por_variable[1], "_", ano_inicio, "_", ano_fin, ".csv"), row.names = FALSE)
write.csv(estaciones_pp_el_salvador, paste0("output/output_seleccion_estaciones/estaciones/estaciones_pp_el_salvador_cal_", calidad_por_variable[1], "_", ano_inicio, "_", ano_fin, ".csv"), row.names = FALSE)



#2 definir estaciones de temperatura
estaciones_temp <- calcular_metricas_calidad_multi(connection, c("t_min", "t_max"), ano_inicio, ano_fin)
estaciones_temp <- estaciones_temp %>% arrange(-calidad_t_max)  %>% filter(calidad_t_max > calidad_por_variable[2] & calidad_t_min > calidad_por_variable[2])

estaciones_temp_el_teniente <- estaciones_temp %>% filter(cod_reg %in% c(6,13,5))
estaciones_temp_el_salvador <- estaciones_temp %>% filter(cod_reg %in% c(3,2))

write.csv(estaciones_temp_el_teniente, paste0("output/output_seleccion_estaciones/estaciones/estaciones_temp_el_teniente_cal_",calidad_por_variable[2],"_" ,ano_inicio, "_", ano_fin, ".csv"), row.names = FALSE)
write.csv(estaciones_temp_el_salvador, paste0("output/output_seleccion_estaciones/estaciones/estaciones_temp_el_salvador_cal_",calidad_por_variable[2], "_",ano_inicio, "_", ano_fin, ".csv"), row.names = FALSE)


#3 definir estaciones de caudal
estaciones_caudal <- calcular_metricas_calidad_multi(connection, 'q', ano_inicio, ano_fin)
estaciones_caudal <- estaciones_caudal %>% arrange(-calidad_q) %>% filter(calidad_q >= calidad_por_variable[3])

estaciones_caudal_el_teniente <- estaciones_caudal %>% filter(cod_reg %in% c(6,13,5))
estaciones_caudal_el_salvador <- estaciones_caudal %>% filter(cod_reg %in% c(3,2))

write.csv(estaciones_caudal_el_teniente, paste0("output/output_seleccion_estaciones/estaciones/estaciones_caudal_el_teniente_cal_", calidad_por_variable[3], "_", ano_inicio, "_", ano_fin, ".csv"), row.names = FALSE)
write.csv(estaciones_caudal_el_salvador, paste0("output/output_seleccion_estaciones/estaciones/estaciones_caudal_el_salvador_cal_",calidad_por_variable[3], "_", ano_inicio, "_", ano_fin, ".csv"), row.names = FALSE)


#estaciones exportadas a SIG y elegir por localizacion. 

