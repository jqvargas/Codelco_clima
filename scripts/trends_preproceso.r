

process_climate_data <- function(data) {
    #el objetivo de esta funcion es preparar las series para entrar al calculo de tendencias, que tengan el formato necesario
    #obtenemos la estacuib del año
    get_season <- function(day, month) {
        if ((month == 12 && day >= 22) || (month == 1) || (month == 2) || (month == 3 && day <= 21)) {
            return(4) # Verano
        } else if ((month == 3 && day >= 22) || (month == 4) || (month == 5) || (month == 6 && day <= 21)) {
            return(1) # Otoño
        } else if ((month == 6 && day >= 22) || (month == 7) || (month == 8) || (month == 9 && day <= 21)) {
            return(2) # Invierno
        } else {
            return(3) # Primavera
        }
    }

    data <- data %>% mutate(Esta = mapply(get_season, Day, Month))
    min_year <- min(data$Year)
    data$an0 <- data$Year - min_year
    data <- data %>% select(fecha, Day, Month, Year, Esta, an0, everything()) %>% select(-fecha)

    return(data)
}
    #write.csv(pp, "series_rellenas/precipitacion_rellenada_trends.csv", row.names = FALSE)
    #write.csv(tn, "series_rellenas/tn_rellenada_trends.csv", row.names = FALSE)
    #write.csv(tx, "series_rellenas/tx_rellenada_trends.csv", row.names = FALSE)


process_metadata <- function(estaciones_pp, estaciones_temp, estaciones_q) {
    #esta funcion tiene el único objetivo de juntar los estaciones y prepararlos para entrar en la funcion de calculo de tendencias.

    #se eliminan las columnas de calidad de los estaciones, dado que al tener distinto nombre evitarian rbind.
    estaciones_pp <- estaciones_pp %>% select(-calidad_pp)
    estaciones_temp <- estaciones_temp %>% select(-calidad_t_min, -calidad_t_max)
    estaciones_q <- estaciones_q %>% select(-calidad_q)

    #se unen los estaciones de las tres variables
    estaciones <- rbind(estaciones_pp, estaciones_temp, estaciones_q)

    #estaciones$nombre <- gsub("[0-9.]", "", estaciones$nombre_estacion)

    
    estaciones <- unique(estaciones)
    

    for (i in 1:nrow(estaciones)) {
        if (grepl("-", estaciones$codigo_nacional[i])) {
            estaciones$Fuente[i] = "DGA"
        } else {
            estaciones$Fuente[i] = "DMC"
        }
    }

    #estaciones <- estaciones %>% select(Fuente, codigo_nacional, nombre_estacion, lat, lon)
    estaciones <- estaciones %>% rename(Codigo = codigo_nacional, Lat = lat, Lon = lon, Estacion = nombre)
    

    #write.csv(estaciones, paste0(carpeta_output,"/", name, "_estaciones_trends.csv"), row.names = FALSE)

    return(estaciones)
}
