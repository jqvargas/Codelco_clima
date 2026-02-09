#Funciones que no tienen que ver ni con obtención ni inserción de datos



check_fecha_valid <- function(fecha) {
  # Verifica que el string tenga el formato correcto: "YYYY-MM-DD"
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha)) {
    print(paste0("Formato de fecha de '",fecha, "' incorrecto. La fecha debe estar en formato 'YYYY-MM-DD'"))
    return(FALSE)  # Formato incorrecto
  }
  # Intenta convertir a Date usando el formato esperado
  fecha_convertida <- tryCatch({
    as.Date(fecha, format = "%Y-%m-%d")
  }, error = function(e) {
    return(NA)  # Error en la conversión
  })

  result <- !is.na(fecha_convertida) && format(fecha_convertida, "%Y-%m-%d") == fecha
  if(!result){
    print(paste0("Formato de fecha de '",fecha, "' incorrecto. La fecha debe estar en formato 'YYYY-MM-DD'"))
  }
  # Verifica que la conversión fue exitosa y que la fecha es real
  return(result)
}


df_dga_a_formato_largo <- function(df, variable){
  #llevar de formato ancho a largo y limpieza de codigos nacionales.
  df <- df %>%
      pivot_longer(
          cols = starts_with("X"),
          names_to = "codigo_nacional",
          values_to = sprintf("%s", variable)
      )

  df <- df %>% drop_na()

  df$codigo_nacional <- gsub("^X", "", df$codigo_nacional)  # Elimina la "X" al inicio
  df$codigo_nacional <- gsub("\\.", "-", df$codigo_nacional) # Reemplaza "." por "-"

  df <- df %>% rename(year = Year, month = Month, day = Day)
  df <- df %>% select(-Date)
  df <- df %>% select(-Id)
  return(df)

}

df_dmc_a_formato_largo <- function(df, variable){
  df <- df %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = sprintf("%s", variable)
    )
  df$codigo_nacional <- gsub("X", "", df$codigo_nacional)

  df <- df %>% drop_na()

  if (variable == 'pp'){
    df <- df %>% rename(year = Año, month = Mes, day = Dia)
  }
  else{
    #si es temperatura hacer esto solo por ser el formato original en que viene el archivo de la seed.
    df <- df %>%
      mutate(year = as.numeric(format(as.Date(Fecha, format="%d-%m-%Y"), "%Y")),
             month = as.numeric(format(as.Date(Fecha, format="%d-%m-%Y"), "%m")),
             day = as.numeric(format(as.Date(Fecha, format="%d-%m-%Y"), "%d")))
    df <- df %>% select(-Fecha)
  }
  return(df)
}

preparar_datos_ancho <- function(dataframe, nombre_variable){
    #esta funcion recibe los datos de salida de la BDD SQL en el formato definido por la función obtener_observaciones_por_variable()
    # Luego la funcion genera todas las combinaciones de codigo_nacional y fechas posibles
    # Rellena con NA aquellas fechas sin mediciones 
    #finalmente lleva a formato ancho los datos.
    # El objetivo de la funcion es entregarle datos a mis colegas en formato cómodo para ellos. 
    
    #valores_originales <- sum(!is.na(dataframe[[nombre_variable]]))

    dataframe <- dataframe %>% select(-fecha)
    #cambio de formato de fechas
    dataframe$fecha <- as.Date(sprintf("%04d-%02d-%02d", dataframe$year, dataframe$month, dataframe$day))
    #eliminar columnas de fecha
    dataframe <- subset(dataframe, select = -c(year, month, day))
    #rango de fechas
    start_date <- min(dataframe$fecha)
    end_date <- max(dataframe$fecha)
    #vector de fechas
    full_dates <- seq.Date(start_date, end_date, by = "day")
    #dataframe de todas las combinaciones posibles de fecha y código nacional (PRODUCTO CRUZ)
    all_combinations <- expand.grid(fecha = full_dates, codigo_nacional = unique(dataframe$codigo_nacional))
    #fusionar con el dataframe original para obtener las entradas faltantes como NA
    complete_df <- merge(all_combinations, dataframe, by = c("fecha", "codigo_nacional"), all.x = TRUE)
    #ordenar el dataframe por fecha y código
    complete_df <- complete_df %>% arrange(codigo_nacional, fecha)

    complete_df <- complete_df %>%
      mutate(
        year = as.numeric(format(fecha, "%Y")),
        month = as.numeric(format(fecha, "%m")),
        day = as.numeric(format(fecha, "%d"))
      )

    # Reordenar columnas: fecha, year, month, day, luego el resto
    other_cols <- setdiff(names(complete_df), c("fecha", "year", "month", "day"))
    complete_df <- complete_df[, c("fecha", "year", "month", "day", other_cols)]

    complete_df <- pivot_wider(complete_df, names_from = codigo_nacional, values_from = all_of(nombre_variable))

    complete_df[,-1] <- lapply(complete_df[,-1], function(x) {
       x <- as.numeric(as.character(x))
       return(x)
     })


    #valores_finales <- sum(!is.na(complete_df[,-1]))  # Excluye la columna 'fecha'
#
    #cat("Valores numéricos originales:", valores_originales, "\n")
    #cat("Valores numéricos después del procesamiento:", valores_finales, "\n")
    return(complete_df)

    
}