source("backend/librerias.R") 
library(readr)

txt_reg_natural <- read.table("codigos_cuencas_reg_natural.txt")
cuencas_camels <- read.csv("catchment_attributes.csv", sep = ",", header = TRUE, quote = "")
cuencas_camels$X.gauge_id. <- ifelse(nchar(cuencas_camels$X.gauge_id.) == 7, paste0("0", cuencas_camels$X.gauge_id.), cuencas_camels$X.gauge_id.)

for (i in 1:length(txt_reg_natural$V1)) {
  if (nchar(txt_reg_natural$V1[i]) == 7) {
    txt_reg_natural$V1[i] <- paste0("0", txt_reg_natural$V1[i])
  }
}
estaciones_caudal <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_caudal_EFE.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

estaciones_caudal_reg_natural <- estaciones_caudal %>% filter(substr(codigo_nacional, 1, 8) %in% txt_reg_natural$V1)

write.csv(estaciones_caudal_reg_natural, "output/output_seleccion_estaciones/estaciones/estaciones_caudal_reg_natural.csv", row.names = FALSE)


#homologar codigos estaciones para un trabajo mas sencillo
estaciones_caudal$codigo_nacional <- substr(estaciones_caudal$codigo_nacional, 1, 8)
estaciones_caudal_reg_natural$codigo_nacional <- substr(estaciones_caudal_reg_natural$codigo_nacional, 1, 8)


#leer tendencias de caudal medio
#empezamos a generar la tabla resumen.
tendencias_caudal <- read.csv("output/tendencias/tendencias_vars/EFE_tendencia_q.csv")
tendencias_caudal$estacion <- substr(tendencias_caudal$estacion, 2, 9)
names(tendencias_caudal)[names(tendencias_caudal) == "estacion"] <- "codigo_nacional"
tendencias_caudal$nom_reg <- estaciones_caudal$nom_reg[match(tendencias_caudal$codigo_nacional, estaciones_caudal$codigo_nacional)]
tendencias_caudal$tipo <- ifelse(tendencias_caudal$codigo_nacional %in% txt_reg_natural$V1, "reg_natural", "no_natural") 
tendencias_caudal$lat <- estaciones_caudal$lat[match(tendencias_caudal$codigo_nacional, estaciones_caudal$codigo_nacional)]
tendencias_caudal$area_km2 <- cuencas_camels$X.area_km2.[match(tendencias_caudal$codigo_nacional, cuencas_camels$X.gauge_id.)]
tendencias_caudal <- tendencias_caudal %>% filter(mes == "Anual") %>% drop_na()
tendencias_caudal$pendiente_km2 <- tendencias_caudal$pend_decada / tendencias_caudal$area_km2
#tendencias_caudal <- tendencias_caudal %>% filter(p_valor < 0.1)

tendencias_caudal_promedio <- tendencias_caudal %>% 
    group_by(nom_reg, tipo) %>% 
    summarise(pendiente_km2 = mean(pendiente_km2, na.rm = TRUE),
          n_estaciones = n(),
          lat = mean(Lat, na.rm = TRUE)) %>%
    ungroup() %>% arrange(-lat)


tendencias_caudal_promedio$pendiente_1000km2 <- tendencias_caudal_promedio$pendiente_km2 * 1000

write.csv(tendencias_caudal_promedio, "output/tendencias/tendencias_vars/EFE_tendencia_q_promedio_reg_natural.csv", row.names = FALSE)
library(openxlsx)
write.xlsx(tendencias_caudal_promedio, "output/tendencias/tendencias_vars/EFE_tendencia_q_promedio_reg_natural.xlsx", row.names = FALSE)





tendencias_q_max <- read.csv("output/tendencias/indicadores_extremos/EFE_Qmax.csv")
tendencias_q_max$est <- substr(tendencias_q_max$est, 2, 9)
names(tendencias_q_max)[names(tendencias_q_max) == "est"] <- "codigo_nacional"
tendencias_q_max$nom_reg <- estaciones_caudal$nom_reg[match(tendencias_q_max$codigo_nacional, estaciones_caudal$codigo_nacional)]
tendencias_q_max$tipo <- ifelse(tendencias_q_max$codigo_nacional %in% txt_reg_natural$V1, "reg_natural", "no_natural")
tendencias_q_max$lat <- estaciones_caudal$lat[match(tendencias_q_max$codigo_nacional, estaciones_caudal$codigo_nacional)]
tendencias_q_max$area_km2 <- cuencas_camels$X.area_km2.[match(tendencias_q_max$codigo_nacional, cuencas_camels$X.gauge_id.)]
tendencias_q_max <- tendencias_q_max %>% drop_na()
tendencias_q_max <- tendencias_q_max %>% filter(periodo == "Anual")
tendencias_q_max$pendiente_km2 <- tendencias_q_max$pendiente / tendencias_q_max$area_km2
tendencias_q_max <- tendencias_q_max %>% filter(p_valor < 0.1)

tendencias_q_max_promedio <- tendencias_q_max %>% 
    group_by(nom_reg, tipo) %>% 
    summarise(pendiente_km2 = mean(pendiente_km2, na.rm = TRUE),
          n_estaciones = n(),
          lat = mean(Lat, na.rm = TRUE)) %>%
    ungroup() %>% arrange(-lat)

tendencias_q_max_promedio$pendiente_1000km2 <- tendencias_q_max_promedio$pendiente_km2 * 1000

write.csv(tendencias_q_max_promedio, "output/tendencias/indicadores_extremos/EFE_tendencia_qmax_promedio_reg_natural.csv", row.names = FALSE)
write.xlsx(tendencias_q_max_promedio, "output/tendencias/indicadores_extremos/EFE_tendencia_qmax_promedio_reg_natural.xlsx", row.names = FALSE)

