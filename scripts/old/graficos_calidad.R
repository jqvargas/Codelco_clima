source("backend/funciones/funciones_consulta.R")
source("backend/funciones/funciones_utilidad.R")
source("backend/librerias.R")
source("backend/consultar.R")

connection <- connect_to_db("backend/.env")


#seleccionar estaciones
estaciones_pp <- calcular_metricas_calidad_multi(connection, "pp", 1985, 2019)
codigos_region <- c(15, 5, 13, 6, 7, 8, 9, 14, 10)
estaciones_pp <- estaciones_pp %>% filter(cod_reg %in% codigos_region) %>% filter(calidad_pp > 80)

#obtener series de precipitacion para las estaciones seleccionadas
vector_codigos <- estaciones_pp$codigo_nacional
observaciones_pp <- obtener_observaciones_por_variable_codigo(connection, "pp", 1985, 2019, codigos_nacionales = vector_codigos)



for (region in codigos_region) {
    estaciones_region <- estaciones_pp %>% filter(cod_reg == region)
    observaciones_pp_region <- observaciones_pp %>% filter(codigo_nacional %in% estaciones_region$codigo_nacional)

    nombre_region <- unique(estaciones_region$nom_reg)
    
    # Calculate monthly quality
    quality_pp_monthly <- observaciones_pp_region %>%
        group_by(codigo_nacional, year, month) %>%
        summarise(q = sum(!is.na(pp))) %>%  # Count non-NA values
        mutate(quality = ifelse(q >= 25, 100, q / 25 * 100)) %>%
        ungroup() %>%
        select(codigo_nacional, year, month, quality, q)

    # Add missing dates and fill with NA
    quality_pp_monthly_fechas_completas <- rellenar_fechas_mensuales(quality_pp_monthly)

    # Calculate annual quality
    quality_pp_year <- quality_pp_monthly_fechas_completas %>%
        group_by(codigo_nacional, year) %>%
        summarise(quality = mean(replace_na(quality, 0))) %>%
        ungroup()

    # Add station names and IDs
    quality_pp_year$nombre_estacion = estaciones_region$nombre[match(quality_pp_year$codigo_nacional, estaciones_region$codigo_nacional)]
    quality_pp_year$id = estaciones_region$id[match(quality_pp_year$codigo_nacional, estaciones_region$codigo_nacional)]
    quality_pp_year <- quality_pp_year %>% arrange(id)

    # Plot quality for the region (single plot)
    plot <- plot_calidad(quality_pp_year, "pp", nombre_region, "output_figuras")

    
    #plot_calidad <- function(dataframe, nombre_variable, nombre_region, ruta_guardado) 
    #ggsave(paste0("output_figuras/region_", nombre_region, "_pp_calidad.png"), plot, width = 10, height = 6)

    # Calculate total quality for the period
    #quality_pp_total <- quality_pp_year %>%
    #    group_by(codigo_nacional) %>% 
    #    summarise(quality = mean(quality)) %>%
    #    ungroup()
#
    ## Add total quality to station metadata
    #estaciones_region$calidad_periodo_1985_2019 = quality_pp_total$quality[match(estaciones_region$codigo_nacional, quality_pp_total$codigo_nacional)]
#
    ## Save results for this region
    #write.csv(estaciones_region, paste0("output/estaciones_region_", region, "_pp.csv"), row.names = FALSE)
    #write.csv(quality_pp_year, paste0("output/calidad_anual_region_", region, "_pp.csv"), row.names = FALSE)
}















#######################################################
######################################################
#PARA OBTENER METRICAS DE CALIDAD
#variables <- c("pp")
#calidad_estaciones <- calcular_metricas_calidad_multi(connection, variables, 1981, 2020)
##calidad_estaciones <- calidad_estaciones %>% filter(altura >= 1000)
##calidad_estaciones <- calidad_estaciones %>% filter(calidad_pp >= 70 & calidad_t_max >= 70 & calidad_t_min >=70)
##calidad_estaciones <- calidad_estaciones %>% filter(calidad_pp >= 80)
#calidad_estaciones <- calidad_estaciones %>% arrange(-altura, -calidad_pp)
#
#write.csv(calidad_estaciones, "estaciones_pp_posibles.csv")
#View(calidad_estaciones)
#codigos_filtrar <- c("220002", "230001", "290004", '330020', '360019', '410005', '450005', '520006', '330007', '01310019-5', '03820004-6', '12286001-9')
#
#selected <- c("11140001-6", "11316004-7", "450005", "08301001-0", "08304004-1", "08350002-6", "05703008-9", "05200007-6", "05403006-1", "01010010-0", "02105022-9", "01021007-0")
#estacione_seleccionadas <- calidad_estaciones %>% filter(codigo_nacional %in% selected)
#write.csv(estacione_seleccionadas, "seleccionadas_pp.csv")
##df_top_por_macrozona_y_fuente <- calidad_estaciones %>%
##  mutate(suma_calidad = calidad_pp + calidad_t_max + calidad_t_min) %>%
##  group_by(macrozona, id_fuente) %>%
##  slice_max(order_by = suma_calidad, n = 1, with_ties = FALSE) %>%
##  ungroup()
##View(df_top_por_macrozona_y_fuente)
#estaciones_elegidas <- calidad_estaciones %>% filter(codigo_nacional %in% codigos_filtrar) 
#View(estaciones_elegidas)
##estaciones_elegidas <- estaciones_elegidas %>% filter(calidad_pp > 90) %>% filter(calidad_t_max > 70)
#write.csv(estaciones_elegidas, "estaciones__elegidas_calidad_1981_2020.csv")
#



###############################################
####################################################################################}
#PARA OBTENER OBSERVACIONES, LO USAREMOS DESPUÉS
#codigos <- c("01410012-1", "02104008-8", "02104010-K", '03430006-2', '04301005-0', '04311005-5', '04502005-3', '04511003-6', '05410006-K', '05703008-9', '220002')
#obs_pp <- obtener_observaciones_por_variable_codigo(connection, "pp", ano_inicio = 1980, ano_fin = 2020, codigos_nacionales = codigos)
#unique(obs_pp$codigo_nacional)
#obs_pp_ancho <- preparar_datos_ancho(obs_pp, "pp")
#write.csv(obs_pp_ancho, "paula/PP_estaciones_alta_montana_comparacion_1980_2020.csv", row.names = FALSE)
#
#obs_t_max <- obtener_observaciones_por_variable_codigo(connection, "t_max", ano_inicio = 1980, ano_fin = 2020, codigos_nacionales = codigos)
#unique(obs_t_max$codigo_nacional)
#obs_t_max_ancho <- preparar_datos_ancho(obs_t_max, "t_max")
#write.csv(obs_t_max_ancho, "paula/t_max_estaciones_alta_montana_comparacion_1980_2020.csv", row.names = FALSE)
#
#obs_t_min <- obtener_observaciones_por_variable_codigo(connection, "t_min", ano_inicio = 1980, ano_fin = 2020, codigos_nacionales = codigos)
#unique(obs_t_min$codigo_nacional)
#obs_t_min_ancho <- preparar_datos_ancho(obs_t_min, "t_min")
#write.csv(obs_t_min_ancho, "paula/t_min_estaciones_alta_montana_comparacion_1980_2020.csv", row.names = FALSE)
#
##codigos <- c("220002", "230001", "290004", '330020', '360019', '410005', '450005', '520006', '330007')
##obs_t_mean <- obtener_observaciones_por_variable_codigo(connection, "t_mean", ano_inicio = 1980, ano_fin = 2020, codigos_nacionales = codigos)
##unique(obs_t_mean$codigo_nacional)
##obs_t_mean_ancho <- preparar_datos_ancho(obs_t_mean, "t_mean")
##write.csv(obs_t_mean_ancho, "t_mean_DMC_estaciones_david_comparacion_1980_2020.csv", row.names = FALSE)
## Obtener observaciones para cada variable
#observaciones_pp <- obtener_observaciones_por_variable(conexion, "pp")
#observaciones_tmax <- obtener_observaciones_por_variable(conexion, "t_max")
#observaciones_tmin <- obtener_observaciones_por_variable(conexion, "t_min")
## Obtener todas las estaciones con datos
#estaciones_con_datos <- obtener_estaciones_con_datos(conexion)
#
## Guardar archivos CSV
#write.csv(observaciones_pp, "output/observaciones_pp.csv", row.names = FALSE)
#write.csv(observaciones_tmax, "output/observaciones_tmax.csv", row.names = FALSE)
#write.csv(observaciones_tmin, "output/observaciones_tmin.csv", row.names = FALSE)
#write.csv(estaciones_con_datos, "output/estaciones_con_datos.csv", row.names = FALSE)

#####################################################################################
#####################################################################################
