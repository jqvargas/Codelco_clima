
#Codigo que solo tiene la función para extraer datos desde archivos .h5 de ARClim

#funcion para extraer datos del gcm sin correccion de sesgo, desde archivo .h5
# - varnme es el nombre de la variable a extraer: pr, tasmax o tasmin
# - d_cmip es el directorio base en donde están todos los archivos ed ARClim
# - modelo es el GCM al que se le quiere extraer información
# - crds es el csv de coordenadas objetivo
# - tempo es la escala temporal a la que se quiere llegar, diario "D", mensual "M" o anual "A".
# - esc es el escenario de emisión
# - mz es la macrozona si es que corresponde, si no se asume nulo
# - dir es el directorio de salida en donde se guardarán los archivos

gcm.raw <- function(
    varname,
    d_cmip,
    modelo,
    crds,
    tempo="M",
    esc,
    mz=NULL,
    dir.out
    ){
  
  #formo el dataframe de fechas para los GCM de ARClim
  ss <- rep(1970:2069, each=365) #se repiten años
  posi <- which(ss %in% 1970:2069)
  date <- seq.Date(as.Date("1-1-1990", format="%d-%m-%Y"),
                   as.Date("31-12-1990", format="%d-%m-%Y"),"day")
  date2 <- rep(date, (2069-1970)+1)
  date2 <- as.data.frame(do.call("rbind", strsplit(as.character(date2), "-")))
  date2[,1] <- ss[posi]
  colnames(date2) <- c("Year", "Month", "Day")
  
  #enlisto los archivos .txt para luego buscarlos
  dir.e <- file.path(d_cmip,vars[i])
  list_gcm <- list_file(dir.e,pattern=".h5",type="h5")
  file.gcm <- list_gcm[[1]]
  ftab.gcm <- list_gcm[[2]]
  
  print(paste("voy en modelo", modelo,"en variable", 
              varname, "a las", Sys.time()))
  
  #busco los archivos que me interesan: modelo y escenario
  ac <- ftab.gcm$longname[which(ftab.gcm$model==modelo & ftab.gcm$esc==esc)] 
  
  #tomo solo el primero en caso de tener más de una realización
  rese <- c(grep("r1i1p1", ac)[1])
  if(is.na(mean(rese))){next}
  
  #ruta del archivoescogido
  gcm.h5 <- file.path(d_cmip,varname,ac[rese])
  
  # leer lat/lon
  lts <- h5read(gcm.h5, "lat")
  lns <- h5read(gcm.h5, "lon")
  
  # nombre del dataset a leer
  dset_name <- switch(varname,
                      pr = "pr",
                      tasmax = "tasmax",
                      tasmin = "tasmin",
                      tas = "tas",
                      stop("variable no reconocida"))
  
  # almacenar resultados de todas las coordenadas del archivo h
  list_coords_series <- vector("list", nrow(crds))
  
  for (c in 1:nrow(crds)) {
    
    lat0 <- crds$Lat[c]
    lon0 <- crds$Lon[c]
    
    # encontrar la celda contenedora (la más cercana al centro)
    ilat <- which.min(abs(lts - lat0))
    ilon <- which.min(abs(lns - lon0))
    
    # índice lineal si el dataset es [time, space]
    # asumo lon es la dimensión espacial, pero ajusta si es distinto
    chosen_idx <- ilon  
    
    # leer solo esa columna del dataset (serie temporal)
    serie_vector <- as.numeric(
      h5read(gcm.h5, dset_name, index = list(NULL, chosen_idx))
    )
    
    df0 <- data.frame(date2, data = serie_vector)
    
    if(tempo=="M"){
      pos.d <- 3 #indica en qué columna está la data para la posterior unión
      
      # aplicar agregado mensual
      if (varname == "pr") {
        DF2 <- aggregate(df0$data, 
                         list(Month = df0$Month, Year = df0$Year), sum)
      } else if (varname %in% c("tasmax", "tasmin")) {
        DF2 <- aggregate(df0$data, 
                         list(Month = df0$Month, Year = df0$Year), mean)
      }
      
      names(DF2)[3] <- paste0(lat0, "_", lon0)
      
      #carpeta de salida
      crp.s.t <- "ARClim_Mensual"
    }else if(tempo=="D"){
      pos.d <- 4 #indica en qué columna está la data para la posterior unión
      DF2 <- df0
      
      #carpeta de salida
      crp.s.t <- "ARClim_Diaria"
    }else if(tempo=="A"){
      pos.d <- 2 #indica en qué columna está la data para la posterior unión
      
      # aplicar agregado anual
      if (varname == "pr") {
        DF2 <- aggregate(df0$data, 
                         list(Year = df0$Year), sum)
      } else if (varname %in% c("tasmax", "tasmin")) {
        DF2 <- aggregate(df0$data, 
                         list(Year = df0$Year), mean)
      }
      
      names(DF2)[2] <- paste0(lat0, "_", lon0)
      
      #carpeta de salida
      crp.s.t <- "ARClim_Anual"
    }
    
    if(c==1){
      DF3 <- DF2
    }else{
      DF3 <- cbind(DF3,DF2[,pos.d])
    }
  }
  
  names(DF3)[-c(1,pos.d-1)] <- paste0(crds$Lat, "_", crds$Lon)
  DF3[,-c(1,pos.d-1)] <- round(DF3[,-c(1,pos.d-1)],1)
  
  #reviso si está la carpeta de salida creada
  dir.out <- paste0(dir.out,crp.s.t)
  if (!dir.exists(dir.out)) {
    dir.create(dir.out, recursive = TRUE)
  }
  
  write.csv(DF3,
            paste0(dir.out,"/", varname,"_", modelo,"_",esc,"_",tempo,"_",mz,"_raw.csv"),
            row.names = FALSE)
  
  h5closeAll()
  
  return (DF3)
}