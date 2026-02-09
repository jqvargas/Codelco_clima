

    #q$fecha <- as.Date(paste0(q$year, "-", q$month, "-", q$day), format = "%Y-%m-%d")

rellenar_caudal <- function(q){
    q <- q %>% 
        pivot_longer(
            cols = starts_with("X"),
            names_to = "codigo_nacional",
            values_to = "q_mean_day"
        )


    q <- rellenar_fechas(q)

    #paso 1: relleno con dia anterior y siguiente

    # PASO 1: rellenamos con la media del día anterior y siguiente
    q <- q %>%
        group_by(codigo_nacional) %>%
        mutate(q_mean_day = ifelse(is.na(q_mean_day), 
                                    (lag(q_mean_day, 1) + lead(q_mean_day, 1)) / 2, 
                                    q_mean_day)) %>%
        ungroup()
    # PASO 2: rellenamos con el valor del mismo día del año anterior
    q <- q %>%
        group_by(codigo_nacional) %>%
        mutate(q_mean_day = ifelse(is.na(q_mean_day), 
                                    lag(q_mean_day, 365), 
                                    q_mean_day)) %>%
        ungroup()
      # PASO 3: rellenamos con mediana mensual de la estación

    q <- q %>%
        group_by(codigo_nacional, month) %>%
        mutate(q_mean_day = ifelse(is.na(q_mean_day), ave(q_mean_day, FUN = function(x) median(x, na.rm = TRUE)), q_mean_day)) %>%
        ungroup()

    q_ancho <- q %>%
        pivot_wider(
            names_from = codigo_nacional,
            values_from = q_mean_day
        )
    head(q_ancho)
    q_ancho <- q_ancho %>% select(-year, -month, -day)
    q_ancho <- q_ancho %>% rename(X = Date)
    return(q_ancho)
}





rellenar_fechas <- function(dataframe){
    #cambio de formato de fechas
    dataframe$Date <- as.Date(with(dataframe, paste(year, month, day, sep = "-")), "%Y-%m-%d")
    #eliminar columnas de fecha
    dataframe <- subset(dataframe, select = -c(year, month, day))
    #rango de fechas
    start_date <- min(dataframe$Date)
    end_date <- max(dataframe$Date)
    #vector de fechas
    full_dates <- seq.Date(start_date, end_date, by = "day")
    #dataframe de todas las combinaciones posibles de fecha y código nacional (PRODUCTO CRUZ)
    all_combinations <- expand.grid(Date = full_dates, codigo_nacional = unique(dataframe$codigo_nacional))
    #fusionar con el dataframe original para obtener las entradas faltantes como NA
    complete_df <- merge(all_combinations, dataframe, by = c("Date", "codigo_nacional"), all.x = TRUE)
    #ordenar el dataframe por fecha y código
    complete_df <- complete_df %>% arrange(codigo_nacional, Date)
    #volver al formato inicial de fechas 
    complete_df$year <- year(complete_df$Date)
    complete_df$month <- month(complete_df$Date)
    complete_df$day <- day(complete_df$Date)
    #complete_df <- subset(complete_df, select = -c(Date))
    return(complete_df)
}
