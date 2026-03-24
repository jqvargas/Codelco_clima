
#Codigo que solo tiene las funciones para extraer datos desde archivos .nc de GCM

#obtención de fechas del archivo en estudio
ext.fech <- function(it){
  fech <- seq(as.Date(paste0(it[1,2],"/", it[1,1], "/1")), as.Date(paste0(it[2,2],"/", it[2,1], "/30")), by="month")#Serie de fechas (mensual) periodo GCM
  fech <- if(length(fech)==1){
    data.frame(fech, rbind(unlist(strsplit(as.character(fech), "-"))))[,-4]
  }else{
    data.frame(fech,do.call("rbind", strsplit(as.character(fech), "-"))[,-3])}
  
  colnames(fech) <- c("Fecha", "Year", "Month")
  
  return(fech)
}

#obtención de cuatro coordenadas más cercanas
# - Revisar nombres del archivo de coordenadas, porque se exige minúsculas: lat y lon
ext.crds <- function(data, crds, ind){
  lts <- data$dim$lat$vals
  lts.posi <- 1:length(lts)
  lns <- data$dim$lon$vals
  lns[lns > 180] <- lns[lns > 180] - 360
  lns.posi <- 1:length(lns) 
  
  crd.tab <- data.frame(Lt.p=rep(lts.posi, length(lns.posi)), Ln.p=rep(lns.posi, each=length(lts.posi)), #Generar tabla con 4 vecinos interpoladores
                        Lat=rep(lts, length(lns)), Lon=rep(lns, each=length(lts))) #Junto anterior, definicion de coordenadas y posiciones del modelo
  crd.tab0 <- crd.tab[which(crd.tab$Lat < max(crds$lat, na.rm = TRUE)+5 & crd.tab$Lat > min(crds$lat,na.rm = TRUE)-5 &
                              crd.tab$Lon < max(crds$lon,na.rm = TRUE)+5 & crd.tab$Lon > min(crds$lon,na.rm = TRUE)-5),] #se aÃÂ±ade 2 grados al norte, sur, ests y oesta para ampliar la ventana
  
  #Distancia Euclidea de las coordenadas del modelo a la coordenada del punto a analizar
  dist <- sqrt(abs(as.numeric(crds$lat[ind])-crd.tab0$Lat)^2 + 
                 abs(as.numeric(crds$lon[ind])-crd.tab0$Lon)^2)
  df <- crd.tab0[which(dist%in%sort(dist)[1:4]),]#4 coordenadas mÃÂ¡s cercanas del GCM, ordenadas
  
  if(nrow(df)>4){df <- df[1:4,]} ## cortar si hay mas de 4 puntos a igual distancia
  
  return (df)
}

#Abrir y extraer datos de archivo NetCDF para las 4 coordenadas más cercanas
ext.data <- function(data, var, df, ps) {
  dats <- data.frame(
    p1 = ncvar_get(data, var, c(as.numeric(df[1,2]), as.numeric(df[1,1]), ps[1]), c(1,1,length(ps))),
    p2 = ncvar_get(data, var, c(as.numeric(df[2,2]), as.numeric(df[2,1]), ps[1]), c(1,1,length(ps))),
    p3 = ncvar_get(data, var, c(as.numeric(df[3,2]), as.numeric(df[3,1]), ps[1]), c(1,1,length(ps))),
    p4 = ncvar_get(data, var, c(as.numeric(df[4,2]), as.numeric(df[4,1]), ps[1]), c(1,1,length(ps)))
  )
  
  return(dats)
}

#Obtencion de serie mensual bruta de GCM.
# - ac es el vector con los nombres completos de los archivos a leer
# - dts es el vector con las fechas que tiene cada archivo
# - ini.h es el año de inicio para descargar datos. Es decir, si se requiere 1985, no se descargarán datos desde 1984 hacia atrás.
# - cont es un contador para cada apertura de archivos y generar salida
# - d_cmip6 es el directorio para abrir el archivo. Incluye la carpeta de la variable
# - var es el nombre de la variable dentro de los netcdf
# - crds es el archivo de coordenadas con nombres "lat" y "lon"
# - interp es un objeto binario para si se quiere interpolar o no en la obtención de datos desde el GCM
# - ind es el índice o coordenada "j" en la que se está
# - is_wind es un objeto binario para indicar si se debe descargar datos para la segunda componente del viento. Se asume falso
# - ac2 es el segundo vector con los nombres completos de los archivos a leer de la segunda componente de viento. Se supone nulo
# - var2 es el nombre de la segunda componente de viento. Se supone nulo
GCM.raw <- function(ac, dts, ini.h, cont, d_cmip6, var, crds, interp, ind, is_wind = FALSE, ac2 = NULL, var2 = NULL) {
  
  df0 <- data.frame(Fecha=0, Year=0, Month=0, Data=0)
  for (h in 1:length(ac)) {
    it <- do.call("c", strsplit(dts[h],"-")) 
    it <- cbind(as.numeric(substr(it, 5, 6)), #Genera matriz mes/año de inicio de GCM que estÃÂ¡ leyendo
                as.numeric(substr(it, 1, 4)))
    
    if (it[2,2] %in% ini.h:2100) { #condiciÃÂ³n para no correr aÃÂ±os fuera del estudio
      
      #obtengo las fechas del archivo en estudio
      fech <- ext.fech(it)
      fech.aux <- fech #fecha auxiliar para modificar el objeto ps en caso de ser necesario
      
      #objeto para saber las posiciones que se deben consultar del archivo netcdf
      ps <- which(fech$Year %in% c(ini.h:2100))
      
      #reviso que ambos años no esten en df0
      if(!(it[2,2] %in% c(df0$Year[1]:df0$Year[nrow(df0)])) & !(it[1,2] %in% c(df0$Year[1]:df0$Year[nrow(df0)]))){
        cont <- cont + 1
        data <- nc_open(paste0(d_cmip6, ac[h]))
        if (is_wind) data2 <- nc_open(paste0(d_cmip6, var2, "/", ac2[h]))
        
        #objeto para modificar posiciones ps en fech. Utilizado para generar el data.frame de dats0
        ch.ps <- FALSE
        
      }else if(!(it[2,2] %in% c(df0$Year[1]:df0$Year[nrow(df0)])) & (it[1,2] %in% c(df0$Year[1]:df0$Year[nrow(df0)]))){
        #caso donde el primer año sí está, pero último no
        if(it[1,1]<10){
          aux.m <- paste0(0,it[1,1])
        }else{
          aux.m <- it[1,1]
        }
        
        #objeto para modificar posiciones ps en fech. Utilizado para generar el data.frame de dats0
        ch.ps <- TRUE
        
        #objeto para saber si último año de df0 está completo o no
        dif.m <- 12-length(df0$Month[which(df0$Year==df0$Year[nrow(df0)])])
        
        #si el mes del primer año del archivo está en df0, corro todo un año, ya que ademas tiene los 12 meses (dif.m=0)
        if(aux.m%in%(df0$Month[which(df0$Year==it[1,2])]) &  dif.m==0){ 
          aux.it <- as.numeric(df0$Year[nrow(df0)])+1
          fech <- fech[which(fech$Year %in% c(aux.it:2100)),] #acoto fechas para no repetir años
          
          #cambio objeto ps segun el filtro de fech
          ps.fech <- which(fech.aux$Fecha==fech$Fecha[1])
          ps <- ps[ps.fech:length(ps)]
        }else if(aux.m%in%(df0$Month[which(df0$Year==it[1,2])]) & dif.m!=0){ #caso donde al último año le falta meses
          aux.it <- as.numeric(df0$Year[nrow(df0)])
          fech <- fech[which(fech$Year %in% c(aux.it:2100)),] #acoto fechas para no repetir años
          fech <- fech[(12-dif.m+1):nrow(fech),] #acoto fechas para no repetir años
          
          #cambio objeto ps segun el filtro de fech
          ps.fech <- which(fech.aux$Fecha==fech$Fecha[1])
          ps <- ps[ps.fech:length(ps)]
        } #si el mes no está en df0, no filtro
        
        cont <- cont + 1
        data <- nc_open(paste0(d_cmip6, ac[h]))
        if (is_wind) data2 <- nc_open(paste0(d_cmip6, var2, "/", ac2[h]))
      }else{ #caso donde ambos años están dentro. O al menos el último lo está y por tanto el primero también debería
        next
      }
      
      #se obtienen las 4 coordenadas más cercanas
      df <- ext.crds(data,crds, ind)
      
      #se extraen los datos de esas 4 coordenadas
      dats <- ext.data(data, var, df, ps)
      if (is_wind) { #si se estudia viento se abre un segundo archivo
        dats2 <- ext.data(data2, var2, df, ps)
        dats <- sqrt(dats^2 + dats2^2)
      }
      
      #Distancia Euclidea de la coordenada más cercana del modelo a la coordenada del punto a analizar
      dist <- sqrt(abs(as.numeric(crds$lat[ind])-df$Lat[1])^2 + 
                     abs(as.numeric(crds$lon[ind])-df$Lon[1])^2)
      
      #Caso donde se tuvo que modificar ps
      if(ch.ps){
        ps <- c(1:length(ps))
      }
      
      if(interp == "No"){
        dats0 <- data.frame(fech[ps,], data=dats[,1])
      }else{
        if (0.1 < sort(dist)[1]){ # si esta a ms de 0.1 grado de distancia, se interpola
          dats0 <- data.frame(fech[ps,], data=unlist(apply(dats, 1 ,function(x)as.numeric(idw(x[1:4], df[,3:4], data.frame(crds$lat[ind],crds$lon[ind]))))))
        }else{ #si no, entonces se 
          dats0 <- data.frame(fech[ps,], data=dats[,1]) # toma el mÃÂ¡s cercano
        }
      }
      
      dat <- data.frame(dats0)
      colnames(dat)[4] <- paste0(crds$lat[ind],"_",crds$lon[ind])
      
      #Conversion Kg*m2*s-1 a mm/mes
      if (var=="pr"){
        dat$nDays <- rep(c(31,28.25,31,30,31,30,31,31,30,31,30,31), (nrow(dat)/12))
        dat[4] <- dat[4]*86400*dat$nDays
      } #Temperaturas se trabajan en Kelvin, recordar pasar a ÃÂ°C
      
      #relleno de datos en caso de que GCM no llegue al 2100
      if(dat$Year[nrow(dat)]>2098 & dat$Year[nrow(dat)]!=2100 & h==length(ac)){
        #condicion para archivos particionados y que tienen archivos para cada año
        if(!(it[1,2]==it[2,2] & it[1,2]==2099)){
          # Fecha máxima actual
          max_fecha <- max(dat$Fecha)
          
          # Fecha final deseada
          fecha_final <- as.Date("2100-12-01")  # ajusta si usas otro día del mes
          
          # Secuencia mensual completa
          fechas_completas <- data.frame(
            Fecha = seq(from = min(dat$Fecha),
                        to   = fecha_final,
                        by   = "month")
          )
          
          # Unir con el original
          dat <- merge(fechas_completas, dat, by = "Fecha", all.x = TRUE)
        }
      }
      
      #valores mensuales despuÃÂ©s de interpolar para est "j" y var "i" en estudio
      if(cont==1){df0 <- dat[1:4]}else{df0 <- rbind(df0, dat[1:4])} # h==1
      
      nc_close(data)
      if (is_wind){nc_close(data2)}
      gc()
      
    }# cierre #condiciÃÂ³n para no correr aÃÂ±os fuera del estudio
  }# cierre archivo ac "h"
  return(df0)
}


bucle.sec <- function(esci, d_cmip, var, models, interp, sits.c, dir.out){
  #enlisto los archivos .csv para luego buscarlos
  list_nc <- list_file(d_cmip, pattern=".nc",type="nc",vars=var,esci=esci)
  file.gcm <- list_nc[[1]]
  fsel.gcm <- list_nc[[2]]
  
  for(k in 1:length(models.b)){ #recorro cada modelo  -  length(models.b)
    mesage(paste("Voy en modelo",models.b[k],"(", k,")","de",length(models.b),"en variable", var,"(", i,")",
                "y escenario", esci,"a las", Sys.time()))
    
    ac <- fsel.gcm$longname[which(fsel.gcm$model==models.b[k])]
    rese <- c(grep("historical", ac)[1],grep(esci, ac)[1]) #ayuda cuando hay mÃÂ¡s archivos para un mismo periodo
    if(is.na(mean(rese))){next}
    st <- unique(do.call("rbind", strsplit(ac,"_"))[,5])
    if(length(st)==1){
      rese <- c(rese[1]:length(ac)) 
    }else{
      anq <- grep("r1i",st)
      if(length(anq)==0){anq <- grep("r2i",st)}
      if(length(anq)==0){next}
      rese <- which(do.call("rbind", strsplit(ac,"_"))[,5]==st[anq[1]])
    }
    ac <- ac[rese]
    dts <- fsel.gcm$int[which(fsel.gcm$model==models.b[k])][rese]
    
    for(j in 1:(nrow(sits.c))){ #(nrow(sits))
      cont <- 0 # contador de archivos de un mismo modelo
      if(j%%20==0){print(paste("voy en celda", "(", j, ")", "de", 
                               nrow(sits.c), "a las", Sys.time()))}
      #Obtencion serie mensual bruta GCM
      df0 <- GCM.raw(ac, dts, ini.h, cont, d_cmip, var, sits.c, interp,j)
      
      if(j==1){
        DF0 <- df0
      }else{
        DF0 <- cbind(DF0,df0[,4])
      }#agrega cada coordenada como columna
      
    }#Cierre coordenada "j"
    
    colnames(DF) <- paste0(sits.c$lat[1:nrow(sits.c)], "_", sits.c$lon[1:nrow(sits.c)]) #Nombres de columna seran coordenadas
    
    write.csv(DF1, paste0(dir.out,"/",vars[i], "/",vars[i],"_",models.b[k],"_", esci,"_UQM.csv"),row.names = F)
    
  }#Cierre modelo "k"
}

