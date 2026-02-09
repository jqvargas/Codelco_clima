library(tidyverse)

source("scripts/Analisis Extremos_origina.R")
source("scripts/trends_preproceso.r")
#source("backend/funciones/funciones_consulta.R")
#source("backend/funciones/funciones_utilidad.R")
source("backend/librerias.R")
#source("backend/consultar.R")


#preparación de series al formato de entrada de la funcion de calculo de tendencias y extremos
pp <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_pp_1992_2021_rellena_100_EFE.csv")
t_min <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_t_min_1992_2021_rellena_100_EFE.csv")
t_max <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_t_max_1992_2021_rellena_100_EFE.csv")
q <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_q_1992_2021_rellena_100_EFE.csv")

estaciones_pp <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_pp_EFE.csv", sep = ";")
estaciones_temp <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_temp_EFE.csv", sep = ";")
estaciones_caudal <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_caudal_EFE.csv", sep = ";")

#cambiar formato de estaciones para que coincidan con el formato de entrada de la funcion de calculo de tendencias y extremos
estaciones_trends_all <- process_metadata(estaciones_pp, estaciones_temp, estaciones_caudal) 

#write.csv(estaciones_trends_all, "estaciones_all/estaciones_trends_all.csv", row.names = FALSE)
#Mas formateo de datos.
pp_trends_obs <- process_climate_data(pp)
t_min_trends_obs <- process_climate_data(t_min)
t_max_trends_obs <- process_climate_data(t_max)
q_trends_obs <- process_climate_data(q)

colnames(estaciones_trends_all)
unique(estaciones_trends_all$Codigo)

estaciones_trends_all$Codigo <- paste0("X", gsub("-", ".", estaciones_trends_all$Codigo))

summary(q_trends_obs)
#EJECUCIÓN ANALISIS TENDENCIAS Y EXTREMOS 
analisis_extremos(pp_trends_obs, t_min_trends_obs, t_max_trends_obs, q_trends_obs, estaciones_trends_all, "output/tendencias", "EFE")


##Solucion error Paula cambiar puntos por comas
#test <- read.csv("output/tendencias/EFE_Indicadores_extremos.csv")
#head(test)
#test$Lat <- gsub(",", ".", test$Lat)
#test$Lat <- as.numeric(test$Lat)
#
#test$Lon <- gsub(",", ".", test$Lon)
#test$Lon <- as.numeric(test$Lon)
#
#write.csv(test, "output/tendencias/EFE_Indicadores_extremos.csv")
#
#indicadores <- unique(test$indicador)
#
#for(i in indicadores) {
#    dataframe <- test %>% filter(indicador == i)
#
#    write.csv(dataframe, paste0("output/tendencias/indicadores_extremos/EFE_", i, ".csv"), row.names = FALSE)
#}
#
#tendencias <- read.csv("output/tendencias/EFE_Tendencias_decadales.csv")
#tendencias$Lat <- gsub(",", ".", tendencias$Lat)
#tendencias$Lat <- as.numeric(tendencias$Lat)
#
#tendencias$Lon <- gsub(",", ".", tendencias$Lon)
#tendencias$Lon <- as.numeric(tendencias$Lon)
#
#tends <- unique(tendencias$var)
#for(v in tends){
#    dataframe <- tendencias %>% filter(var == v)
#
#    write.csv(dataframe, paste0("output/tendencias/tendencias_vars/EFE_tendencia_", v, ".csv"), row.names = FALSE)
#
#}
