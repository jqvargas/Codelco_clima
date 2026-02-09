#Funciones relacionadas a consultas y obtención de datos. 
# 1 Operaciones básicas de base de datos
obtener_fuentes <- function(connection){
  result <- dbGetQuery(connection, "SELECT * from fuente;")
  return(result)
}

obtener_id_fuente <- function(connection, fuente){
  #obtener id de la fuente según el nombre entregado por el usuario
  query <- sprintf("SELECT id FROM fuente WHERE nombre = '%s';", fuente)

  result <- tryCatch({
    dbGetQuery(connection, query)
  }, error = function(e) {
    print(paste0("Error en la consulta SQL para variable: ", nombre_variable_abr, "->", e$message))
    stop()
  })

  if(nrow(result) == 0){
    #si no se encuenta nada levantar mensaje:
    print(paste0("La fuente: '", fuente, "' no es una fuente válida en la Base de Datos"))
    #consultar cuales son las fuentes disponibles e imprimir
    fuentes <- obtener_fuentes(connection)
    print("Las fuentes validas para ingresar son: ")
    print(fuentes$nombre)
  }
  
  return(result$id)

  #Esta función retornará el id resultante de la fuente si lo encuentra.
  #Si no lo encuentra retornará un objeto tipo integer vacío.
  
}

obtener_nombre_abr_variable <- function(connection, id_variable){
  query <- sprintf("SELECT nombre_abr FROM variable WHERE id = %d;", id_variable)
  result <- dbGetQuery(connection, query)
  return(result$nombre_abr)
}
obtener_variables <- function(connection, nombre_variable){
  result <- dbGetQuery(connection, "
  SELECT * from variable;")
  return(result)
}
obtener_id_variable <- function(connection, nombre_variable_abr){
  #obtener id de la fuente según el nombre entregado por el usuario
  query <- sprintf("SELECT id FROM variable WHERE nombre_abr = '%s';", nombre_variable_abr)

  result <- tryCatch({
    dbGetQuery(connection, query)
  }, error = function(e) {
    print(paste0("Error en la consulta SQL para variable: ", nombre_variable_abr, "->", e$message))
    stop()
  })

  if(nrow(result) == 0){
    #si no se encuenta nada levantar mensaje:
    print(paste0("La Variable: '", nombre_variable_abr, "' no es una variable válida en la Base de Datos"))
    #consultar cuales son las variables disponibles e imprimir
    variables <- obtener_variables(connection)
    print("Debe ingresar 'nombre_abr' de la variable que busca")
    print("Las variables validas para ingresar son: ")
    print(variables)
  }
  #Esta función retornará el id resultante de la  variable si lo encuentra.
  #Si no lo encuentra retornará un objeto tipo integer vacío.
  return(result$id)
}

obtener_id_estacion <- function(connection, codigo_nacional, nombre_fuente){
  #la obtención de la estación debe ser por codigo_nacional y fuente, dado que posible que se repita el codigo_nacional entre distintas fuentes
  id_fuente <- obtener_id_fuente(connection, nombre_fuente)
  if(length(id_fuente) > 0){
    query <- sprintf("SELECT id FROM estacion WHERE codigo_nacional = '%s' AND id_fuente = '%d';", codigo_nacional, id_fuente)
    result <- tryCatch({
    dbGetQuery(connection, query)
    }, error = function(e) {
      print(paste0("Error en la consulta SQL para estacion: ", codigo_nacional, "->", e$message))
      stop()
    })
    if(nrow(result) == 0){
      #si no se encuenta nada levantar mensaje:
      print(paste0("La estación: '", codigo_nacional, "' de la fuente '", nombre_fuente, "' no es una combinación válida en la Base de Datos"))
    }
    return(result$id)
  }
  #Esta función retornará el id resultante de la estación si lo encuentra.
  #Si no lo encuentra retornará un objeto tipo integer vacío.
  return(integer(0))
}

# 2 Consultas de datos sobre estaciones
obtener_estaciones_fuente <- function(connection, nombre_fuente){
  #Esta funcion obtiene las estaciones según el nombre de la fuente entregado.
  #Si nombre_fuente == all, se entregan todas las estaciones
  if(nombre_fuente == "all"){
    result <- dbGetQuery(connection, "
    SELECT * from estacion;")
    return(result)
  }
  else{
    id_fuente <- obtener_id_fuente(connection, nombre_fuente)
    if(length(id_fuente) > 0){
      query <- sprintf("SELECT * FROM estacion WHERE id_fuente = '%d';", id_fuente)
      result <- dbGetQuery(connection, query)
    }
    else{
      stop()
    }
  }
}

obtener_estaciones_con_datos <- function(conexion) {
  consulta <- "
    SELECT DISTINCT estacion.* 
    FROM estacion 
    JOIN observacion ON estacion.id = observacion.id_estacion
    ORDER BY estacion.codigo_nacional;
  "
  estaciones <- dbGetQuery(conexion, consulta)

  estaciones <- estaciones %>% filter(!grepl("Estacion desconocida", nombre)) %>% select(-id)
  return(estaciones)
}

obtener_estaciones_con_datos_temperatura <- function(conexion) {
  consulta <- "
    SELECT DISTINCT estacion.* 
    FROM estacion 
    JOIN observacion ON estacion.id = observacion.id_estacion
    WHERE observacion.id_variable IN (2, 3, 4)
    ORDER BY estacion.codigo_nacional;
  "
  estaciones <- dbGetQuery(conexion, consulta)

  estaciones <- estaciones %>% filter(!grepl("Estacion desconocida", nombre)) %>% select(-id)
  return(estaciones)
}


obtener_estaciones_con_datos_pp <- function(conexion) {
  consulta <- "
    SELECT DISTINCT estacion.* 
    FROM estacion 
    JOIN observacion ON estacion.id = observacion.id_estacion
    WHERE observacion.id_variable IN (1)
    ORDER BY estacion.codigo_nacional;
  "
  estaciones <- dbGetQuery(conexion, consulta)

  #estaciones <- estaciones %>% filter(!grepl("Estacion desconocida", nombre)) %>% select(-id)
  return(estaciones)
}



obtener_estaciones_con_datos_caudal<- function(conexion) {
  consulta <- "
    SELECT DISTINCT estacion.* 
    FROM estacion 
    JOIN observacion ON estacion.id = observacion.id_estacion
    WHERE observacion.id_variable IN (5)
    ORDER BY estacion.codigo_nacional;
  "
  estaciones <- dbGetQuery(conexion, consulta)

  estaciones <- estaciones %>% filter(!grepl("Estacion desconocida", nombre)) %>% select(-id)
  return(estaciones)
}

# 3 Obtener observaciones de variables meteorógicas.
obtener_observaciones_por_variable_codigo <- function(conexion, nombre_variable, ano_inicio = NULL, ano_fin = NULL, codigos_nacionales = NULL) {
  #esta es para VARIAS estaciones. 
  #codigos nacionales es el vector de codigos nacionales
  id_variable <- obtener_id_variable(conexion, nombre_variable)
  
  # Construir la consulta base
  consulta <- sprintf("
    SELECT 
      observacion.fecha as fecha, 
      observacion.valor as %s, 
      observacion.year as year,
      observacion.month as month,
      observacion.day as day,
      estacion.codigo_nacional as codigo_nacional
    FROM observacion 
    JOIN estacion ON observacion.id_estacion = estacion.id 
    WHERE observacion.id_variable = %s
    AND estacion.nombre NOT LIKE '%%Estacion desconocida%%'
  ", nombre_variable, id_variable)
  
  # Filtros por año
  if (!is.null(ano_inicio)) {
    consulta <- paste0(consulta, " AND observacion.year >= ", ano_inicio)
  }
  if (!is.null(ano_fin)) {
    consulta <- paste0(consulta, " AND observacion.year <= ", ano_fin)
  }
  
  # Filtro por códigos nacionales
  if (!is.null(codigos_nacionales)) {
    codigos_sql <- paste0("'", codigos_nacionales, "'", collapse = ", ")
    consulta <- paste0(consulta, " AND estacion.codigo_nacional IN (", codigos_sql, ")")
  }
  
  consulta <- paste0(consulta, " ORDER BY estacion.codigo_nacional, observacion.fecha;")
  
  # Ejecutar consulta
  observaciones <- dbGetQuery(conexion, consulta)
  return(observaciones)
}


obtener_observaciones_por_variable <- function(conexion, nombre_variable, fecha_inicio = NULL, fecha_fin = NULL, estaciones = NULL) {
    #para VARIAS estaciones
    #es igual a la anterior pero se entrega un dataframe de estaciones en lugar de codigos nacionales.
    id_variable <- obtener_id_variable(conexion, nombre_variable)
  # Construir la consulta con filtro de fecha opcional
    consulta <- sprintf("
      SELECT 
        observacion.fecha as fecha, 
        observacion.valor as %s, 
        observacion.year as year,
        observacion.month as month,
        observacion.day as day,
        estacion.codigo_nacional as codigo_nacional
      FROM observacion 
      JOIN estacion ON observacion.id_estacion = estacion.id 
      WHERE observacion.id_variable = %s
      AND estacion.nombre NOT LIKE '%%Estacion desconocida%%'
    ", nombre_variable, id_variable)
  
    # Agregar filtro de fecha si se proporciona
    if (!is.null(fecha_inicio)) {
      consulta <- paste0(consulta, " AND observacion.fecha >= '", fecha_inicio, "'")
    }

     if (!is.null(fecha_inicio)) {
      consulta <- paste0(consulta, " AND observacion.fecha <= '", fecha_fin, "'")
    }
      # Agregar filtro de estaciones si se proporciona
    if (!is.null(estaciones)) {
      codigos <- paste0("'", estaciones$codigo_nacional, "'", collapse = ", ")
      consulta <- paste0(consulta, " AND estacion.codigo_nacional IN (", codigos, ")")
    }
  
    
    consulta <- paste0(consulta, " ORDER BY estacion.codigo_nacional, observacion.fecha;")
    # Ejecutar consulta
    observaciones <- dbGetQuery(conexion, consulta)
    return(observaciones)
}

obtener_observaciones_por_variable_codigo <- function(conexion, nombre_variable, nombre_tabla, ano_inicio = NULL, ano_fin = NULL, codigos_nacionales = NULL) {
  #esta es para VARIAS estaciones. 
  #codigos nacionales es el vector de codigos nacionales
  id_variable <- obtener_id_variable(conexion, nombre_variable)
  
  # Construir la consulta base (usando nombre_tabla)
  consulta <- sprintf("
    SELECT 
      %1$s.fecha as fecha, 
      %1$s.valor as %2$s, 
      %1$s.year as year,
      %1$s.month as month,
      %1$s.day as day,
      estacion.codigo_nacional as codigo_nacional
    FROM %1$s
    JOIN estacion ON %1$s.id_estacion = estacion.id 
    WHERE %1$s.id_variable = %3$s
    AND estacion.nombre NOT LIKE '%%Estacion desconocida%%'
  ", nombre_tabla, nombre_variable, id_variable)
  
  # Filtros por año
  if (!is.null(ano_inicio)) {
    consulta <- paste0(consulta, " AND ", nombre_tabla, ".year >= ", ano_inicio)
  }
  if (!is.null(ano_fin)) {
    consulta <- paste0(consulta, " AND ", nombre_tabla, ".year <= ", ano_fin)
  }
  
  # Filtro por códigos nacionales
  if (!is.null(codigos_nacionales)) {
    codigos_sql <- paste0("'", codigos_nacionales, "'", collapse = ", ")
    consulta <- paste0(consulta, " AND estacion.codigo_nacional IN (", codigos_sql, ")")
  }
  
  consulta <- paste0(consulta, " ORDER BY estacion.codigo_nacional, ", nombre_tabla, ".fecha;")
  
  # Ejecutar consulta
  observaciones <- dbGetQuery(conexion, consulta)
  return(observaciones)
}


obtener_datos_estacion_validada <- function(connection, nombre_fuente, nombre_variable_abr, codigo_nacional, fecha_ini = NULL, fecha_fin = NULL){
  #para UNA SOLA estacion. funcion usada durante la etapa de depuración/concatenación de series base con descargadas
  #si no se proporciona fecha_ini ni fecha fin, la consulta devolverá todas las fechas disponibles. 
  id_variable = obtener_id_variable(connection, nombre_variable_abr)
  id_estacion = obtener_id_estacion(connection, codigo_nacional, nombre_fuente)
  
  # Verificar si las fechas son válidas (solo si se proporcionan)
  if (!is.null(fecha_ini) && fecha_ini != "" && !check_fecha_valid(fecha_ini)) {
    stop("Fecha de inicio inválida")
  }
  if (!is.null(fecha_fin) && fecha_fin != "" && !check_fecha_valid(fecha_fin)) {
    stop("Fecha de fin inválida")
  }
  if (length(id_variable) > 0 && length(id_estacion) > 0) {
    # Construcción base de la consulta SQL
    query <- sprintf("SELECT valor as %s, fecha, day, month, year 
                      FROM observacion
                      WHERE id_variable = %d AND id_estacion = %d", 
                     nombre_variable_abr, id_variable, id_estacion)
    
    # Agregar condición de fechas solo si se proporcionan
    if (!is.null(fecha_ini) && fecha_ini != "" && !is.null(fecha_fin) && fecha_fin != "") {
      query <- sprintf("%s AND fecha BETWEEN '%s' AND '%s'", query, fecha_ini, fecha_fin)
    } 
    
    result <- dbGetQuery(connection, query)
    
    if (nrow(result) == 0) {

      result <- data.frame(
      fecha = as.Date(character()),
      valor = numeric(),
      day = integer(),
      month = integer(),
      year = integer(),
      stringsAsFactors = FALSE
    )
      names(result)[names(result) == "valor"] <- nombre_variable_abr
      return(result)
    }else {
      result$codigo_nacional <- codigo_nacional
      result <- result %>% arrange(year, month, day)
      return(result)
   
    }
  } else {
    print("Números inválidos")
  }
}



obtener_datos_estacion_preliminar <- function(connection, nombre_fuente, nombre_variable_abr, codigo_nacional, fecha_ini = NULL, fecha_fin = NULL){
  #si no se proporciona fecha_ini ni fecha fin, la consulta devolverá todas las fechas disponibles. 
  #para UNA SOLA estacion. funcion usada durante la etapa de depuración/concatenación de series base con descargadas
  id_variable = obtener_id_variable(connection, nombre_variable_abr)
  id_estacion = obtener_id_estacion(connection, codigo_nacional, nombre_fuente)
  
  # Verificar si las fechas son válidas (solo si se proporcionan)
  if (!is.null(fecha_ini) && fecha_ini != "" && !check_fecha_valid(fecha_ini)) {
    stop("Fecha de inicio inválida")
  }
  if (!is.null(fecha_fin) && fecha_fin != "" && !check_fecha_valid(fecha_fin)) {
    stop("Fecha de fin inválida")
  }
  if (length(id_variable) > 0 && length(id_estacion) > 0) {
    # Construcción base de la consulta SQL
    query <- sprintf("SELECT valor as %s, fecha, day, month, year 
                      FROM observacion_preliminar 
                      WHERE id_variable = %d AND id_estacion = %d", 
                     nombre_variable_abr, id_variable, id_estacion)
    
    # Agregar condición de fechas solo si se proporcionan
    if (!is.null(fecha_ini) && fecha_ini != "" && !is.null(fecha_fin) && fecha_fin != "") {
      query <- sprintf("%s AND fecha BETWEEN '%s' AND '%s'", query, fecha_ini, fecha_fin)
    } 
    
    result <- dbGetQuery(connection, query)
    
    if (nrow(result) == 0) {
      # Asegurar estructura mínima si no hay resultados, prevenir errores.
      result <- data.frame(
        fecha = as.Date(character()),
        valor = numeric(),
        day = integer(),
        month = integer(),
        year = integer(),
        stringsAsFactors = FALSE
      )
      names(result)[names(result) == "valor"] <- nombre_variable_abr
      return(result)
    } else {
      result$codigo_nacional <- codigo_nacional
      result <- result %>% arrange(year, month, day)
      return(result)
    }
  } else {
    print("Números inválidos")
  }
}

comparar_num_observaciones_por_estacion <- function(connection, id_fuente, id_variable) {
  # Asegura que los argumentos sean enteros
  id_fuente <- as.integer(id_fuente)
  id_variable <- as.integer(id_variable)
  
  consulta <- sprintf("
    WITH obs_validadas AS (
        SELECT e.codigo_nacional, COUNT(*) AS n_validadas
        FROM observacion o
        JOIN estacion e ON o.id_estacion = e.id
        JOIN fuente f ON e.id_fuente = f.id
        WHERE o.id_variable = %d AND f.id = %d
        GROUP BY e.codigo_nacional
    ),
    obs_preliminares AS (
        SELECT e.codigo_nacional, COUNT(*) AS n_preliminares
        FROM observacion_preliminar op
        JOIN estacion e ON op.id_estacion = e.id
        JOIN fuente f ON e.id_fuente = f.id
        WHERE op.id_variable = %d AND f.id = %d
        GROUP BY e.codigo_nacional
    )
    SELECT 
        COALESCE(v.codigo_nacional, p.codigo_nacional) AS estacion,
        COALESCE(n_validadas, 0) AS num_observaciones_validadas,
        COALESCE(n_preliminares, 0) AS num_observaciones_preliminares
    FROM obs_validadas v
    FULL OUTER JOIN obs_preliminares p 
    ON v.codigo_nacional = p.codigo_nacional
    ORDER BY estacion;
  ", id_variable, id_fuente, id_variable, id_fuente)
  
  resultado <- DBI::dbGetQuery(connection, consulta)
  return(resultado)
}



# 4 obtener calidad de las estaciones según sus observaciones.
calcular_metricas_calidad_multi <- function(conexion, nombres_variables, ano_inicio, ano_fin) {
  # esta es dinámica sirve para varias variables, se calcula la calidad para cada variable entregada en objeto nombres_variables
  # nombres_variables <- c('pp', 't_max', 't_min')
  total_dias <- (ano_fin - ano_inicio + 1) * 365.25
  df_final <- NULL

  for (nombre_variable in nombres_variables) {
    id_variable <- obtener_id_variable(conexion, nombre_variable)
    
    consulta <- sprintf("
      SELECT 
        estacion.*, 
        COUNT(*) AS num_observaciones
      FROM observacion_final 
      JOIN estacion ON observacion_final.id_estacion = estacion.id 
      WHERE observacion_final.id_variable = %s
        AND observacion_final.year BETWEEN %d AND %d
        AND estacion.nombre NOT LIKE '%%Estacion desconocida%%'
      GROUP BY estacion.id
", id_variable, ano_inicio, ano_fin)
    
    resultados <- dbGetQuery(conexion, consulta)
    
    resultados[[paste0("calidad_", nombre_variable)]] <- (resultados$num_observaciones / total_dias) * 100
    resultados <- resultados %>% select(-num_observaciones)

    if (is.null(df_final)) {
      df_final <- resultados
    } else {
      df_final <- df_final %>%
        left_join(resultados, by = colnames(resultados)[!colnames(resultados) %in% paste0("calidad_", nombre_variable)])
    }
  }
  
  
  df_final <- df_final %>%
    mutate(periodo = paste0(ano_inicio, "-", ano_fin)) %>%
    select(-tipo, -fecha_ini)
  
  return(df_final)
}

calcular_metricas_calidad <- function(conexion, nombre_variable, ano_inicio, ano_fin) {
  # sólo para una única variable.
  # Obtener ID de la variable
  id_variable <- obtener_id_variable(conexion, nombre_variable)
  
  # Calcular total de observaciones posibles (días en el período)
  total_dias <- (ano_fin - ano_inicio + 1) * 365.25
  
  # Consulta para contar observaciones por estación
  consulta <- sprintf("
    SELECT 
      estacion.codigo_nacional,
      estacion.nombre,
      estacion.id_fuente, 
      COUNT(*) as num_observaciones
    FROM observacion 
    JOIN estacion ON observacion.id_estacion = estacion.id 
    WHERE observacion.id_variable = %s
    AND observacion.year BETWEEN %d AND %d
    AND estacion.nombre NOT LIKE '%%Estacion desconocida%%'
    GROUP BY estacion.codigo_nacional, estacion.id_fuente, estacion.nombre
  ", id_variable, ano_inicio, ano_fin)
  
  # Obtener resultados
  resultados <- dbGetQuery(conexion, consulta)
  
  # Calcular porcentaje de calidad
  resultados$calidad <- (resultados$num_observaciones / total_dias) * 100
  
  # Agregar información de período y variable
  resultados$periodo <- paste0(ano_inicio, "-", ano_fin)
  resultados$variable <- nombre_variable
  
  # Seleccionar y reordenar columnas
  resultados <- resultados %>% select(codigo_nacional, nombre, id_fuente, variable, periodo, calidad)
  
  return(resultados)
}

#esta funcion NO DEBE ejecutarse para variables nuevas que se estén descargando.
obtener_estaciones_con_datos_variable <- function(connection, id_fuente, id_variable) {
  # Retorna estaciones que tienen al menos un dato de la variable especificada
  # Asegurar que los IDs sean numéricos
  id_fuente <- as.integer(id_fuente)
  id_variable <- as.integer(id_variable)
  
  query <- sprintf("
    SELECT DISTINCT e.codigo_nacional
    FROM estacion e
    JOIN observacion_final o ON e.id = o.id_estacion
    WHERE e.id_fuente = %d
      AND o.id_variable = %d
  ", id_fuente, id_variable)
  
  estaciones <- dbGetQuery(connection, query)
  return(estaciones$codigo_nacional)
}
