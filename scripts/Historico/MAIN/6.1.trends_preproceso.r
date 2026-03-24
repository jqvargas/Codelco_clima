# =============================================================================
# Preproceso de series diarias para cálculo de tendencias e indicadores extremos.
# Acepta formato del proyecto: fecha, year, month, day (minúsculas) + columnas de estaciones.
# Salida: Day, Month, Year, Esta (estación del año 1-4), an0 (año relativo) y columnas de
# estaciones (índices 6 en adelante). analisis_extremos usa Month para agregaciones.
# =============================================================================

process_climate_data <- function(data) {
    # Normalizar nombres de columnas de fecha (proyecto usa year, month, day en minúscula)
    nms <- names(data)
    if ("day" %in% nms && !("Day" %in% nms)) data$Day <- data$day
    if ("month" %in% nms && !("Month" %in% nms)) data$Month <- data$month
    if ("year" %in% nms && !("Year" %in% nms)) data$Year <- data$year

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
    data <- data %>% select(Day, Month, Year, Esta, an0, everything())
    cols_fecha <- c("fecha", "day", "month", "year")
    data <- data %>% select(-any_of(cols_fecha))

    return(data)
}
    #write.csv(pp, "series_rellenas/precipitacion_rellenada_trends.csv", row.names = FALSE)
    #write.csv(tn, "series_rellenas/tn_rellenada_trends.csv", row.names = FALSE)
    #write.csv(tx, "series_rellenas/tx_rellenada_trends.csv", row.names = FALSE)


# Junta estaciones de pp, temp y caudal en una sola tabla para el cálculo de tendencias.
# Acepta columnas nombre_estacion o nombre; elimina solo columnas de calidad presentes.
process_metadata <- function(estaciones_pp, estaciones_temp, estaciones_q) {
    drop_calidad <- function(df) {
        cols_cal <- c("calidad_pp", "calidad_t_min", "calidad_t_max", "calidad_q")
        df %>% select(-any_of(cols_cal))
    }
    estaciones_pp   <- drop_calidad(estaciones_pp)
    estaciones_temp <- drop_calidad(estaciones_temp)
    estaciones_q   <- drop_calidad(estaciones_q)

    estaciones <- rbind(estaciones_pp, estaciones_temp, estaciones_q)
    estaciones <- unique(estaciones)

    for (i in 1:nrow(estaciones)) {
        cod <- estaciones$codigo_nacional[i]
        if (grepl("-", as.character(cod))) {
            estaciones$Fuente[i] <- "DGA"
        } else {
            estaciones$Fuente[i] <- "DMC"
        }
    }

    if ("nombre_estacion" %in% names(estaciones) && !("nombre" %in% names(estaciones)))
        estaciones$nombre <- estaciones$nombre_estacion
    estaciones <- estaciones %>%
        rename(Codigo = codigo_nacional, Lat = lat, Lon = lon, Estacion = nombre)

    return(estaciones)
}
