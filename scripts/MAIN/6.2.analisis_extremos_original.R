
analisis_extremos <- function(pp, tn, tx, q, metadatos, carpeta_output, name){
  library(trend)
  if (!dir.exists(carpeta_output)) dir.create(carpeta_output, recursive = TRUE)
  ###Analisis de indicadores extremos Kimal-LoAguirre
# if q != NA


for(i in 6:ncol(pp)){
  for(j in 1:nrow(pp)){
    if(is.na(pp[j,i])){
      pp[j,i] <- median(pp[which(pp$Month==pp$Month[j]),i], na.rm=T)
    }
  }
}


for(i in 6:ncol(tn)){
  for(j in 1:nrow(tn)){
    if(is.na(tn[j,i])){
      tn[j,i] <- mean(tn[which(tn$Month==tn$Month[j]),i], na.rm=T)
    }
  }
}
for(i in 6:ncol(tx)){
  for(j in 1:nrow(tx)){
    if(is.na(tx[j,i])){
      tx[j,i] <- mean(tx[which(tx$Month==tx$Month[j]),i], na.rm=T)
    }
  }
}



#Tendencias (decadales en base a series mensuales y anuales)
#Series mensuales
pp.m <- aggregate(pp[,6:ncol(pp)], list(pp[,2], pp[,3]), sum)
tn.m <- aggregate(tn[,6:ncol(tn)], list(tn[,2], tn[,3]), mean)                  
tx.m <- aggregate(tx[,6:ncol(tx)], list(tx[,2], tx[,3]),mean)
#q.m <- aggregate(q[,6:ncol(q)], list(q[,2], q[,3]),mean)
colnames(tn.m)[ncol(tn.m)] <- colnames(tn)[ncol(tn)]
colnames(tx.m)[ncol(tx.m)] <- colnames(tx)[ncol(tx)]

#Series anuales
pp.an <- aggregate(pp[,6:ncol(pp)], list(pp[,3]), sum)
tn.an <- aggregate(tn[,6:ncol(tn)], list(tn[,3]), mean)                  
tx.an <- aggregate(tx[,6:ncol(tx)], list(tx[,3]),mean)


#Curva estacional (mensual)
pp.mn <- aggregate(pp.m[,3:ncol(pp.m)], list(pp.m[,1]), mean)
tn.mn <- aggregate(tn.m[,3:ncol(tn.m)], list(tn.m[,1]), mean)                  
tx.mn <- aggregate(tx.m[,3:ncol(tx.m)], list(tx.m[,1]),mean)



#Precipitacion
for(j in 1:(ncol(pp.m)-2)){
  dat.an.pp <- pp.an[,j+1]
  pend <- lm(dat.an.pp ~ c(1:length(dat.an.pp)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp <- mk.test(dat.an.pp)$p.value
  res0 <- data.frame(var = "pp", estacion=colnames(pp)[j+5], mes="Anual",pend_decada= pend, p_valor=mkp)
  for(i in 1:12){
    dat.pp <- pp.m[which(pp.m$Group.1==i),j+2]
    
    pend <- lm(dat.pp ~ c(1:length(dat.pp)))$coefficients[2]*10 #Una decada son 10 a?os
    mkp <- mk.test(dat.pp)$p.value
    res <- data.frame(var = "pp", estacion=colnames(pp)[j+5], mes=i,pend_decada= pend, p_valor=mkp)
    if(i==1){df <- res}else{df <- rbind(df, res)}
  }
  df <- rbind(df, res0)
  if(j==1){df2 <- df}else{df2 <- rbind(df2, df)}
}

#Caudal
if (!is.null(q)) {

  for(i in 6:ncol(q)){
    for(j in 1:nrow(q)){
      if(is.na(q[j,i])){
        q[j,i] <- mean(q[which(q$Month==q$Month[j]),i], na.rm=T)
      }
    }
  }
  print("entre a proceso de caudal")
  q.m <- aggregate(q[,6:ncol(q)], list(q[,2], q[,3]),mean)
  print("logre agregacion mensual")
  q.an <- aggregate(q[,6:ncol(q)], list(q[,3]),mean)
  print("logre agregacion anual")
  q.mn <- aggregate(q.m[,3:ncol(q.m)], list(q.m[,1]),mean)
  print("logre agregacion estacional")

  for(j in 1:(ncol(q.m)-2)){
    dat.an.q <- q.an[,j+1]
    pend <- lm(dat.an.q ~ c(1:length(dat.an.q)))$coefficients[2]*10 #Una decada son 10 a?os
    mkp <- mk.test(dat.an.q)$p.value
    res0 <- data.frame(var = "q", estacion=colnames(q)[j+5], mes="Anual",pend_decada= pend, p_valor=mkp)
    for(i in 1:12){
      dat.q <- q.m[which(q.m$Group.1==i),j+2]

      pend <- lm(dat.q ~ c(1:length(dat.q)))$coefficients[2]*10 #Una decada son 10 a?os
      mkp <- mk.test(dat.q)$p.value
      res <- data.frame(var = "q", estacion=colnames(q)[j+5], mes=i,pend_decada= pend, p_valor=mkp)
      if(i==1){df <- res}else{df <- rbind(df, res)}
    }
    df <- rbind(df, res0)
    if(j==1){df3 <- df}else{df3 <- rbind(df3, df)}
  }
} else {
  df3 <- data.frame(var = character(), estacion = character(), mes = character(), pend_decada = numeric(), p_valor = numeric())
}

#Temperatura m?nima
for(j in 1:(ncol(tn.m)-2)){
  dat.an.tn <- tn.an[,j+1]
  pend <- lm(dat.an.tn ~ c(1:length(dat.an.tn)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp <- mk.test(dat.an.tn)$p.value
  res0 <- data.frame(var = "Tn", estacion=colnames(tn)[j+5], mes="Anual",pend_decada= pend, p_valor=mkp)
  for(i in 1:12){
    dat.tn <- tn.m[which(tn.m$Group.1==i),j+2]
    
    pend <- lm(dat.tn ~ c(1:length(dat.tn)))$coefficients[2]*10 #Una decada son 10 a?os
    mkp <- mk.test(dat.tn)$p.value
    res <- data.frame(var = "Tn", estacion=colnames(tn)[j+5], mes=i,pend_decada= pend, p_valor=mkp)
    if(i==1){df <- res}else{df <- rbind(df, res)}
  }
  df <- rbind(df, res0)
  if(j==1){df4 <- df}else{df4 <- rbind(df4, df)}
}

#Temperatura m?xima
for(j in 1:(ncol(tx.m)-2)){
  dat.an.tx <- tx.an[,j+1]
  pend <- lm(dat.an.tx ~ c(1:length(dat.an.tx)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp <- mk.test(dat.an.tx)$p.value
  res0 <- data.frame(var = "Tx", estacion=colnames(tx)[j+5], mes="Anual",pend_decada= pend, p_valor=mkp)
  for(i in 1:12){
    dat.tx <- tx.m[which(tx.m$Group.1==i),j+2]
    
    pend <- lm(dat.tx ~ c(1:length(dat.tx)))$coefficients[2]*10 #Una decada son 10 a?os
    mkp <- mk.test(dat.tx)$p.value
    res <- data.frame(var = "Tx", estacion=colnames(tx)[j+5], mes=i,pend_decada= pend, p_valor=mkp)
    if(i==1){df <- res}else{df <- rbind(df, res)}
  }
  df <- rbind(df, res0)
  if(j==1){df5 <- df}else{df5 <- rbind(df5, df)}
}
fin.p <- rbind(df2, df3, df4, df5)
fin.p <- data.frame(Nombre=metadatos$Estacion[match(fin.p$estacion, metadatos$Codigo)],
                    Lat=metadatos$Lat[match(fin.p$estacion, metadatos$Codigo)],
                    Lon=metadatos$Lon[match(fin.p$estacion, metadatos$Codigo)],
                    fin.p)

write.csv(fin.p, paste0(carpeta_output, "/", name, "_Tendencias_decadales.csv"),row.names = FALSE)


#Obtencion de indicadores extremos
#Asociados a temperatura maxima
#Max Tmax,	Temperatura m?xima de las temperaturas m?ximas
for(i in 6:ncol(tx)){
  mtx.an <- aggregate(tx[,i], list(tx[,3]), max)[,2]
  mtx.season <- aggregate(tx[,i], list(tx[,4], tx[,5]), max)
  mtx.ot <- mtx.season[which(mtx.season[,1]==1),3]
  mtx.inv <- mtx.season[which(mtx.season[,1]==2),3]
  mtx.pri <- mtx.season[which(mtx.season[,1]==3),3]
  mtx.ver <- mtx.season[which(mtx.season[,1]==4),3]
  mtx.ver <- mtx.ver[-length(mtx.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(mtx.an ~ c(1:length(mtx.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(mtx.an)$p.value
  pend.ot <- lm(mtx.ot ~ c(1:length(mtx.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(mtx.ot)$p.value
  pend.inv <- lm(mtx.inv ~ c(1:length(mtx.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(mtx.inv)$p.value
  pend.pri <- lm(mtx.pri ~ c(1:length(mtx.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(mtx.pri)$p.value
  pend.ver <- lm(mtx.ver ~ c(1:length(mtx.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(mtx.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tx)[i], indicador="MaxTmax", unidad = "C_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tx)[i], indicador="MaxTmax", unidad = "C_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tx)[i], indicador="MaxTmax", unidad = "C_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tx)[i], indicador="MaxTmax", unidad = "C_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tx)[i], indicador="MaxTmax", unidad = "C_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df1 <- res}else{df1 <- rbind(df1, res)}
}

#Min Tmax,	Temperatura m?nima de las temperaturas m?ximas
for(i in 6:ncol(tx)){
  mtx.an <- aggregate(tx[,i], list(tx[,3]), min)[,2]
  mtx.season <- aggregate(tx[,i], list(tx[,4], tx[,5]), min)
  mtx.ot <- mtx.season[which(mtx.season[,1]==1),3]
  mtx.inv <- mtx.season[which(mtx.season[,1]==2),3]
  mtx.pri <- mtx.season[which(mtx.season[,1]==3),3]
  mtx.ver <- mtx.season[which(mtx.season[,1]==4),3]
  mtx.ver <- mtx.ver[-length(mtx.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(mtx.an ~ c(1:length(mtx.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(mtx.an)$p.value
  pend.ot <- lm(mtx.ot ~ c(1:length(mtx.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(mtx.ot)$p.value
  pend.inv <- lm(mtx.inv ~ c(1:length(mtx.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(mtx.inv)$p.value
  pend.pri <- lm(mtx.pri ~ c(1:length(mtx.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(mtx.pri)$p.value
  pend.ver <- lm(mtx.ver ~ c(1:length(mtx.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(mtx.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tx)[i], indicador="MinTmax", unidad = "C_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tx)[i], indicador="MinTmax", unidad = "C_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tx)[i], indicador="MinTmax", unidad = "C_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tx)[i], indicador="MinTmax", unidad = "C_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tx)[i], indicador="MinTmax", unidad = "C_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df2 <- res}else{df2 <- rbind(df2, res)}
}


#Max Tmin,	Temperatura m?xima de las temperaturas m?nimas
for(i in 6:ncol(tn)){
  mtn.an <- aggregate(tn[,i], list(tn[,3]), max)[,2]
  mtn.season <- aggregate(tn[,i], list(tn[,4], tn[,5]), max)
  mtn.ot <- mtn.season[which(mtn.season[,1]==1),3]
  mtn.inv <- mtn.season[which(mtn.season[,1]==2),3]
  mtn.pri <- mtn.season[which(mtn.season[,1]==3),3]
  mtn.ver <- mtn.season[which(mtn.season[,1]==4),3]
  mtn.ver <- mtn.ver[-length(mtn.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(mtn.an ~ c(1:length(mtn.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(mtn.an)$p.value
  pend.ot <- lm(mtn.ot ~ c(1:length(mtn.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(mtn.ot)$p.value
  pend.inv <- lm(mtn.inv ~ c(1:length(mtn.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(mtn.inv)$p.value
  pend.pri <- lm(mtn.pri ~ c(1:length(mtn.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(mtn.pri)$p.value
  pend.ver <- lm(mtn.ver ~ c(1:length(mtn.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(mtn.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tn)[i], indicador="MaxTmin", unidad = "C_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tn)[i], indicador="MaxTmin", unidad = "C_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tn)[i], indicador="MaxTmin", unidad = "C_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tn)[i], indicador="MaxTmin", unidad = "C_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tn)[i], indicador="MaxTmin", unidad = "C_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df3 <- res}else{df3 <- rbind(df3, res)}
}

#Min Tmin,	Temperatura m?nima de las temperaturas m?nimas
for(i in 6:ncol(tn)){
  mtn.an <- aggregate(tn[,i], list(tn[,3]), min)[,2]
  mtn.season <- aggregate(tn[,i], list(tn[,4], tn[,5]), min)
  mtn.ot <- mtn.season[which(mtn.season[,1]==1),3]
  mtn.inv <- mtn.season[which(mtn.season[,1]==2),3]
  mtn.pri <- mtn.season[which(mtn.season[,1]==3),3]
  mtn.ver <- mtn.season[which(mtn.season[,1]==4),3]
  mtn.ver <- mtn.ver[-length(mtn.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(mtn.an ~ c(1:length(mtn.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(mtn.an)$p.value
  pend.ot <- lm(mtn.ot ~ c(1:length(mtn.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(mtn.ot)$p.value
  pend.inv <- lm(mtn.inv ~ c(1:length(mtn.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(mtn.inv)$p.value
  pend.pri <- lm(mtn.pri ~ c(1:length(mtn.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(mtn.pri)$p.value
  pend.ver <- lm(mtn.ver ~ c(1:length(mtn.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(mtn.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tn)[i], indicador="MinTmin", unidad = "C_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tn)[i], indicador="MinTmin", unidad = "C_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tn)[i], indicador="MinTmin", unidad = "C_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tn)[i], indicador="MinTmin", unidad = "C_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tn)[i], indicador="MinTmin", unidad = "C_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df4 <- res}else{df4 <- rbind(df4, res)}
}


#Warm Nights,	N? dias con Tmin sobre percentil 90
for(i in 6:ncol(tn)){
  v.p90 <- unname(quantile(tn[,i], .9))
  wn.an <- aggregate(tn[,i], list(tn[,3]), function(x){length(which(x > v.p90))})[,2]
  wn.season <- aggregate(tn[,i], list(tn[,4], tn[,5]), function(x){length(which(x > v.p90))})
  wn.ot <- wn.season[which(wn.season[,1]==1),3]
  wn.inv <- wn.season[which(wn.season[,1]==2),3]
  wn.pri <- wn.season[which(wn.season[,1]==3),3]
  wn.ver <- wn.season[which(wn.season[,1]==4),3]
  wn.ver <- wn.ver[-length(wn.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an.wn <- lm(wn.an ~ c(1:length(wn.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(wn.an)$p.value
  pend.ot <- lm(wn.ot ~ c(1:length(wn.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(wn.ot)$p.value
  pend.inv <- lm(wn.inv ~ c(1:length(wn.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(wn.inv)$p.value
  pend.pri <- lm(wn.pri ~ c(1:length(wn.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(wn.pri)$p.value
  pend.ver <- lm(wn.ver ~ c(1:length(wn.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(wn.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tn)[i], indicador="WarmNights", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tn)[i], indicador="WarmNights", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tn)[i], indicador="WarmNights", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tn)[i], indicador="WarmNights", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tn)[i], indicador="WarmNights", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df5 <- res}else{df5 <- rbind(df5, res)}
}

#Cold Nights,	N? dias con Tmin bajo percentil 90
for(i in 6:ncol(tn)){
  v.p10 <- unname(quantile(tn[,i], .1))
  cn.an <- aggregate(tn[,i], list(tn[,3]), function(x){length(which(x < v.p10))})[,2]
  cn.season <- aggregate(tn[,i], list(tn[,4], tn[,5]), function(x){length(which(x < v.p10))})
  cn.ot <- cn.season[which(cn.season[,1]==1),3]
  cn.inv <- cn.season[which(cn.season[,1]==2),3]
  cn.pri <- cn.season[which(cn.season[,1]==3),3]
  cn.ver <- cn.season[which(cn.season[,1]==4),3]
  cn.ver <- cn.ver[-length(cn.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(cn.an ~ c(1:length(cn.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(cn.an)$p.value
  pend.ot <- lm(cn.ot ~ c(1:length(cn.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(cn.ot)$p.value
  pend.inv <- lm(cn.inv ~ c(1:length(cn.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(cn.inv)$p.value
  pend.pri <- lm(cn.pri ~ c(1:length(cn.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(cn.pri)$p.value
  pend.ver <- lm(cn.ver ~ c(1:length(cn.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(cn.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tn)[i], indicador="ColdNights", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tn)[i], indicador="ColdNights", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tn)[i], indicador="ColdNights", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tn)[i], indicador="ColdNights", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tn)[i], indicador="ColdNights", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df6 <- res}else{df6 <- rbind(df6, res)}
}

#Warm days,	N? dias con Tmax sobre percentil 90
for(i in 6:ncol(tx)){
  v.p90 <- unname(quantile(tx[,i], .9))
  wd.an <- aggregate(tx[,i], list(tx[,3]), function(x){length(which(x > v.p90))})[,2]
  wd.season <- aggregate(tx[,i], list(tx[,4], tx[,5]), function(x){length(which(x > v.p90))})
  wd.ot <- wd.season[which(wd.season[,1]==1),3]
  wd.inv <- wd.season[which(wd.season[,1]==2),3]
  wd.pri <- wd.season[which(wd.season[,1]==3),3]
  wd.ver <- wd.season[which(wd.season[,1]==4),3]
  wd.ver <- wd.ver[-length(wd.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(wd.an ~ c(1:length(wd.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(wd.an)$p.value
  pend.ot <- lm(wd.ot ~ c(1:length(wd.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(wd.ot)$p.value
  pend.inv <- lm(wd.inv ~ c(1:length(wd.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(wd.inv)$p.value
  pend.pri <- lm(wd.pri ~ c(1:length(wd.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(wd.pri)$p.value
  pend.ver <- lm(wd.ver ~ c(1:length(wd.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(wd.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tx)[i], indicador="WarmDays", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tx)[i], indicador="WarmDays", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tx)[i], indicador="WarmDays", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tx)[i], indicador="WarmDays", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tx)[i], indicador="WarmDays", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df7 <- res}else{df7 <- rbind(df7, res)}
}

#Cold days,	N? dias con Tmax bajo percentil 10
for(i in 6:ncol(tx)){
  v.p10 <- unname(quantile(tx[,i], .1))
  cd.an <- aggregate(tx[,i], list(tx[,3]), function(x){length(which(x < v.p10))})[,2]
  cd.season <- aggregate(tx[,i], list(tx[,4], tx[,5]), function(x){length(which(x < v.p10))})
  cd.ot <- cd.season[which(cd.season[,1]==1),3]
  cd.inv <- cd.season[which(cd.season[,1]==2),3]
  cd.pri <- cd.season[which(cd.season[,1]==3),3]
  cd.ver <- cd.season[which(cd.season[,1]==4),3]
  cd.ver <- cd.ver[-length(cd.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(cd.an ~ c(1:length(cd.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(cd.an)$p.value
  pend.ot <- lm(cd.ot ~ c(1:length(cd.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(cd.ot)$p.value
  pend.inv <- lm(cd.inv ~ c(1:length(cd.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(cd.inv)$p.value
  pend.pri <- lm(cd.pri ~ c(1:length(cd.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(cd.pri)$p.value
  pend.ver <- lm(cd.ver ~ c(1:length(cd.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(cd.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tx)[i], indicador="ColdDays", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tx)[i], indicador="ColdDays", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tx)[i], indicador="ColdDays", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tx)[i], indicador="ColdDays", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tx)[i], indicador="ColdDays", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df8 <- res}else{df8 <- rbind(df8, res)}
}

#Summer days,	N? dias con Tmax 25?C
for(i in 6:ncol(tx)){
  sd.an <- aggregate(tx[,i], list(tx[,3]), function(x){length(which(x > 25))})[,2]
  sd.season <- aggregate(tx[,i], list(tx[,4], tx[,5]), function(x){length(which(x > 25))})
  sd.ot <- sd.season[which(sd.season[,1]==1),3]
  sd.inv <- sd.season[which(sd.season[,1]==2),3]
  sd.pri <- sd.season[which(sd.season[,1]==3),3]
  sd.ver <- sd.season[which(sd.season[,1]==4),3]
  sd.ver <- sd.ver[-length(sd.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(sd.an ~ c(1:length(sd.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(sd.an)$p.value
  pend.ot <- lm(sd.ot ~ c(1:length(sd.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(sd.ot)$p.value
  pend.inv <- lm(sd.inv ~ c(1:length(sd.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(sd.inv)$p.value
  pend.pri <- lm(sd.pri ~ c(1:length(sd.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(sd.pri)$p.value
  pend.ver <- lm(sd.ver ~ c(1:length(sd.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(sd.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tx)[i], indicador="SummerDays", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tx)[i], indicador="SummerDays", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tx)[i], indicador="SummerDays", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tx)[i], indicador="SummerDays", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tx)[i], indicador="SummerDays", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df9 <- res}else{df9 <- rbind(df9, res)}
}

#Frost days,	N? dias con Tmin bajo 0?C
for(i in 6:ncol(tn)){
  fd.an <- aggregate(tn[,i], list(tn[,3]), function(x){length(which(x < 0))})[,2]
  fd.season <- aggregate(tn[,i], list(tn[,4], tn[,5]), function(x){length(which(x < 0))})
  fd.ot <- fd.season[which(fd.season[,1]==1),3]
  fd.inv <- fd.season[which(fd.season[,1]==2),3]
  fd.pri <- fd.season[which(fd.season[,1]==3),3]
  fd.ver <- fd.season[which(fd.season[,1]==4),3]
  fd.ver <- fd.ver[-length(fd.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(fd.an ~ c(1:length(fd.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(fd.an)$p.value
  pend.ot <- lm(fd.ot ~ c(1:length(fd.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(fd.ot)$p.value
  pend.inv <- lm(fd.inv ~ c(1:length(fd.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(fd.inv)$p.value
  pend.pri <- lm(fd.pri ~ c(1:length(fd.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(fd.pri)$p.value
  pend.ver <- lm(fd.ver ~ c(1:length(fd.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(fd.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tn)[i], indicador="FrostDays", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tn)[i], indicador="FrostDays", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tn)[i], indicador="FrostDays", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tn)[i], indicador="FrostDays", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tn)[i], indicador="FrostDays", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df10 <- res}else{df10 <- rbind(df10, res)}
}

#Rango promedio temperaturas extremas
for(i in 6:ncol(tn)){
  rango <- tx[,i]-tn[,i]
  rg.an <- aggregate(rango, list(tn[,3]), mean)[,2]
  rg.season <- aggregate(rango, list(tn[,4], tn[,5]), mean)
  rg.ot <- rg.season[which(rg.season[,1]==1),3]
  rg.inv <- rg.season[which(rg.season[,1]==2),3]
  rg.pri <- rg.season[which(rg.season[,1]==3),3]
  rg.ver <- rg.season[which(rg.season[,1]==4),3]
  rg.ver <- rg.ver[-length(rg.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(rg.an ~ c(1:length(rg.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(rg.an)$p.value
  pend.ot <- lm(rg.ot ~ c(1:length(rg.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(rg.ot)$p.value
  pend.inv <- lm(rg.inv ~ c(1:length(rg.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(rg.inv)$p.value
  pend.pri <- lm(rg.pri ~ c(1:length(rg.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(rg.pri)$p.value
  pend.ver <- lm(rg.ver ~ c(1:length(rg.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(rg.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(tn)[i], indicador="Rango", unidad = "C_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(tn)[i], indicador="Rango", unidad = "C_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(tn)[i], indicador="Rango", unidad = "C_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(tn)[i], indicador="Rango", unidad = "C_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(tn)[i], indicador="Rango", unidad = "C_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df11 <- res}else{df11 <- rbind(df11, res)}
}

#Desviacion estandar precipitaciones
for(i in 6:ncol(pp)){
  std.an <- aggregate(pp[,i], list(pp[,3]), sd)[,2]
  std.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), sd)
  std.ot <- std.season[which(std.season[,1]==1),3]
  std.inv <- std.season[which(std.season[,1]==2),3]
  std.pri <- std.season[which(std.season[,1]==3),3]
  std.ver <- std.season[which(std.season[,1]==4),3]
  std.ver <- std.ver[-length(std.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(std.an ~ c(1:length(std.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(std.an)$p.value
  pend.ot <- lm(std.ot ~ c(1:length(std.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(std.ot)$p.value
  pend.inv <- lm(std.inv ~ c(1:length(std.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(std.inv)$p.value
  pend.pri <- lm(std.pri ~ c(1:length(std.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(std.pri)$p.value
  pend.ver <- lm(std.ver ~ c(1:length(std.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(std.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="Desviacion", unidad = "mm", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="Desviacion", unidad = "mm", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="Desviacion", unidad = "mm", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="Desviacion", unidad = "mm", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="Desviacion", unidad = "mm", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df12 <- res}else{df12 <- rbind(df12, res)}
}

#Precipitacion mayor a 10 mm
for(i in 6:ncol(pp)){
  r10.an <- aggregate(pp[,i], list(pp[,3]), function(x){length(which(x > 10))})[,2]
  r10.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), function(x){length(which(x > 10))})
  r10.ot <- r10.season[which(r10.season[,1]==1),3]
  r10.inv <- r10.season[which(r10.season[,1]==2),3]
  r10.pri <- r10.season[which(r10.season[,1]==3),3]
  r10.ver <- r10.season[which(r10.season[,1]==4),3]
  r10.ver <- r10.ver[-length(r10.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(r10.an ~ c(1:length(r10.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(r10.an)$p.value
  pend.ot <- lm(r10.ot ~ c(1:length(r10.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(r10.ot)$p.value
  pend.inv <- lm(r10.inv ~ c(1:length(r10.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(r10.inv)$p.value
  pend.pri <- lm(r10.pri ~ c(1:length(r10.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(r10.pri)$p.value
  pend.ver <- lm(r10.ver ~ c(1:length(r10.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(r10.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="R10mm", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="R10mm", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="R10mm", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="R10mm", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="R10mm", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df13 <- res}else{df13 <- rbind(df13, res)}
}

#Precipitacion mayor a 20 mm
for(i in 6:ncol(pp)){
  r20.an <- aggregate(pp[,i], list(pp[,3]), function(x){length(which(x > 20))})[,2]
  r20.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), function(x){length(which(x > 20))})
  r20.ot <- r20.season[which(r20.season[,1]==1),3]
  r20.inv <- r20.season[which(r20.season[,1]==2),3]
  r20.pri <- r20.season[which(r20.season[,1]==3),3]
  r20.ver <- r20.season[which(r20.season[,1]==4),3]
  r20.ver <- r20.ver[-length(r20.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(r20.an ~ c(1:length(r20.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(r20.an)$p.value
  pend.ot <- lm(r20.ot ~ c(1:length(r20.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(r20.ot)$p.value
  pend.inv <- lm(r20.inv ~ c(1:length(r20.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(r20.inv)$p.value
  pend.pri <- lm(r20.pri ~ c(1:length(r20.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(r20.pri)$p.value
  pend.ver <- lm(r20.ver ~ c(1:length(r20.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(r20.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="R20mm", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="R20mm", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="R20mm", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="R20mm", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="R20mm", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df14 <- res}else{df14 <- rbind(df14, res)}
}

#Precipitacion menor a 1 mm (dias sin lluvia)
for(i in 6:ncol(pp)){
  tdd.an <- aggregate(pp[,i], list(pp[,3]), function(x){length(which(x < 1))})[,2]
  tdd.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), function(x){length(which(x < 1))})
  tdd.ot <- tdd.season[which(tdd.season[,1]==1),3]
  tdd.inv <- tdd.season[which(tdd.season[,1]==2),3]
  tdd.pri <- tdd.season[which(tdd.season[,1]==3),3]
  tdd.ver <- tdd.season[which(tdd.season[,1]==4),3]
  tdd.ver <- tdd.ver[-length(tdd.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(tdd.an ~ c(1:length(tdd.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(tdd.an)$p.value
  pend.ot <- lm(tdd.ot ~ c(1:length(tdd.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(tdd.ot)$p.value
  pend.inv <- lm(tdd.inv ~ c(1:length(tdd.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(tdd.inv)$p.value
  pend.pri <- lm(tdd.pri ~ c(1:length(tdd.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(tdd.pri)$p.value
  pend.ver <- lm(tdd.ver ~ c(1:length(tdd.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(tdd.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="TDD", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="TDD", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="TDD", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="TDD", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="TDD", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df15 <- res}else{df15 <- rbind(df15, res)}
}

#Precipitacion m?xima
for(i in 6:ncol(pp)){
  rx1.an <- aggregate(pp[,i], list(pp[,3]), max)[,2]
  rx1.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), max)
  rx1.ot <- rx1.season[which(rx1.season[,1]==1),3]
  rx1.inv <- rx1.season[which(rx1.season[,1]==2),3]
  rx1.pri <- rx1.season[which(rx1.season[,1]==3),3]
  rx1.ver <- rx1.season[which(rx1.season[,1]==4),3]
  rx1.ver <- rx1.ver[-length(rx1.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(rx1.an ~ c(1:length(rx1.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(rx1.an)$p.value
  pend.ot <- lm(rx1.ot ~ c(1:length(rx1.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(rx1.ot)$p.value
  pend.inv <- lm(rx1.inv ~ c(1:length(rx1.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(rx1.inv)$p.value
  pend.pri <- lm(rx1.pri ~ c(1:length(rx1.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(rx1.pri)$p.value
  pend.ver <- lm(rx1.ver ~ c(1:length(rx1.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(rx1.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="rx1", unidad = "mm_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="rx1", unidad = "mm_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="rx1", unidad = "mm_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="rx1", unidad = "mm_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="rx1", unidad = "mm_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df16 <- res}else{df16 <- rbind(df16, res)}
}

#Precipitacion mayor a percentil 95
for(i in 6:ncol(pp)){
  v.p95 <- unname(quantile(pp[,i], .95))
  r95.an <- aggregate(pp[,i], list(pp[,3]), function(x){length(which(x > v.p95))})[,2]
  r95.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), function(x){length(which(x > v.p95))})
  r95.ot <- r95.season[which(r95.season[,1]==1),3]
  r95.inv <- r95.season[which(r95.season[,1]==2),3]
  r95.pri <- r95.season[which(r95.season[,1]==3),3]
  r95.ver <- r95.season[which(r95.season[,1]==4),3]
  r95.ver <- r95.ver[-length(r95.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(r95.an ~ c(1:length(r95.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(r95.an)$p.value
  pend.ot <- lm(r95.ot ~ c(1:length(r95.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(r95.ot)$p.value
  pend.inv <- lm(r95.inv ~ c(1:length(r95.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(r95.inv)$p.value
  pend.pri <- lm(r95.pri ~ c(1:length(r95.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(r95.pri)$p.value
  pend.ver <- lm(r95.ver ~ c(1:length(r95.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(r95.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="R95", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="R95", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="R95", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="R95", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="R95", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df17 <- res}else{df17 <- rbind(df17, res)}
}

#Precipitacion mayor a percentil 75
for(i in 6:ncol(pp)){
  v.p75 <- unname(quantile(pp[,i], .75))
  r75.an <- aggregate(pp[,i], list(pp[,3]), function(x){length(which(x > v.p75))})[,2]
  r75.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), function(x){length(which(x > v.p75))})
  r75.ot <- r75.season[which(r75.season[,1]==1),3]
  r75.inv <- r75.season[which(r75.season[,1]==2),3]
  r75.pri <- r75.season[which(r75.season[,1]==3),3]
  r75.ver <- r75.season[which(r75.season[,1]==4),3]
  r75.ver <- r75.ver[-length(r75.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(r75.an ~ c(1:length(r75.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(r75.an)$p.value
  pend.ot <- lm(r75.ot ~ c(1:length(r75.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(r75.ot)$p.value
  pend.inv <- lm(r75.inv ~ c(1:length(r75.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(r75.inv)$p.value
  pend.pri <- lm(r75.pri ~ c(1:length(r75.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(r75.pri)$p.value
  pend.ver <- lm(r75.ver ~ c(1:length(r75.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(r75.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="R75", unidad = "dias_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="R75", unidad = "dias_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="R75", unidad = "dias_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="R75", unidad = "dias_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="R75", unidad = "dias_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df18 <- res}else{df18 <- rbind(df18, res)}
}

#Suma Precipitaciones mayor a percentil 95
for(i in 6:ncol(pp)){
  v.p95 <- unname(quantile(pp[,i], .95))
  pr95.an <- aggregate(pp[,i], list(pp[,3]), function(x){sum(x[which(x > v.p95)])})[,2]
  pr95.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), function(x){sum(x[which(x > v.p95)])})
  pr95.ot <- pr95.season[which(pr95.season[,1]==1),3]
  pr95.inv <- pr95.season[which(pr95.season[,1]==2),3]
  pr95.pri <- pr95.season[which(pr95.season[,1]==3),3]
  pr95.ver <- pr95.season[which(pr95.season[,1]==4),3]
  pr95.ver <- pr95.ver[-length(pr95.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(pr95.an ~ c(1:length(pr95.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(pr95.an)$p.value
  pend.ot <- lm(pr95.ot ~ c(1:length(pr95.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(pr95.ot)$p.value
  pend.inv <- lm(pr95.inv ~ c(1:length(pr95.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(pr95.inv)$p.value
  pend.pri <- lm(pr95.pri ~ c(1:length(pr95.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(pr95.pri)$p.value
  pend.ver <- lm(pr95.ver ~ c(1:length(pr95.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(pr95.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="PR95", unidad = "mm_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="PR95", unidad = "mm_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="PR95", unidad = "mm_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="PR95", unidad = "mm_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="PR95", unidad = "mm_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df19 <- res}else{df19 <- rbind(df19, res)}
}

#Suma Precipitaciones mayor a percentil 75
for(i in 6:ncol(pp)){
  v.p75 <- unname(quantile(pp[,i], .75))
  pr75.an <- aggregate(pp[,i], list(pp[,3]), function(x){sum(x[which(x > v.p75)])})[,2]
  pr75.season <- aggregate(pp[,i], list(pp[,4], pp[,5]), function(x){sum(x[which(x > v.p75)])})
  pr75.ot <- pr75.season[which(pr75.season[,1]==1),3]
  pr75.inv <- pr75.season[which(pr75.season[,1]==2),3]
  pr75.pri <- pr75.season[which(pr75.season[,1]==3),3]
  pr75.ver <- pr75.season[which(pr75.season[,1]==4),3]
  pr75.ver <- pr75.ver[-length(pr75.ver)] #Saco ultimo verano al estar incompleto
  
  pend.an <- lm(pr75.an ~ c(1:length(pr75.an)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.an <- mk.test(pr75.an)$p.value
  pend.ot <- lm(pr75.ot ~ c(1:length(pr75.ot)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ot <- mk.test(pr75.ot)$p.value
  pend.inv <- lm(pr75.inv ~ c(1:length(pr75.inv)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.inv <- mk.test(pr75.inv)$p.value
  pend.pri <- lm(pr75.pri ~ c(1:length(pr75.pri)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.pri <- mk.test(pr75.pri)$p.value
  pend.ver <- lm(pr75.ver ~ c(1:length(pr75.ver)))$coefficients[2]*10 #Una decada son 10 a?os
  mkp.ver <- mk.test(pr75.ver)$p.value
  res <-  rbind(
    data.frame(est=colnames(pp)[i], indicador="PR75", unidad = "mm_decada", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
    data.frame(est=colnames(pp)[i], indicador="PR75", unidad = "mm_decada", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
    data.frame(est=colnames(pp)[i], indicador="PR75", unidad = "mm_decada", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
    data.frame(est=colnames(pp)[i], indicador="PR75", unidad = "mm_decada", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
    data.frame(est=colnames(pp)[i], indicador="PR75", unidad = "mm_decada", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
  )
  if(i==6){df20 <- res}else{df20 <- rbind(df20, res)}
}  

if (!is.null(q)) {
for(i in 6:ncol(q)){
    qmax.an <- aggregate(q[,i], list(q[,3]), max)[,2]
    qmax.season <- aggregate(q[,i], list(q[,4], q[,5]), max)
    qmax.ot <- qmax.season[which(qmax.season[,1]==1),3]
    qmax.inv <- qmax.season[which(qmax.season[,1]==2),3]
    qmax.pri <- qmax.season[which(qmax.season[,1]==3),3]
    qmax.ver <- qmax.season[which(qmax.season[,1]==4),3]
    qmax.ver <- qmax.ver[-length(qmax.ver)] # Saco último verano al estar incompleto
    
    pend.an <- lm(qmax.an ~ c(1:length(qmax.an)))$coefficients[2]*10 # Una década son 10 años
    mkp.an <- mk.test(qmax.an)$p.value
    pend.ot <- lm(qmax.ot ~ c(1:length(qmax.ot)))$coefficients[2]*10 # Una década son 10 años
    mkp.ot <- mk.test(qmax.ot)$p.value
    pend.inv <- lm(qmax.inv ~ c(1:length(qmax.inv)))$coefficients[2]*10 # Una década son 10 años
    mkp.inv <- mk.test(qmax.inv)$p.value
    pend.pri <- lm(qmax.pri ~ c(1:length(qmax.pri)))$coefficients[2]*10 # Una década son 10 años
    mkp.pri <- mk.test(qmax.pri)$p.value
    pend.ver <- lm(qmax.ver ~ c(1:length(qmax.ver)))$coefficients[2]*10 # Una década son 10 años
    mkp.ver <- mk.test(qmax.ver)$p.value
    res <-  rbind(
        data.frame(est=colnames(q)[i], indicador="Qmax", unidad = "m3/s", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
        data.frame(est=colnames(q)[i], indicador="Qmax", unidad = "m3/s", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
        data.frame(est=colnames(q)[i], indicador="Qmax", unidad = "m3/s", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
        data.frame(est=colnames(q)[i], indicador="Qmax", unidad = "m3/s", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
        data.frame(est=colnames(q)[i], indicador="Qmax", unidad = "m3/s", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
    )
    if(i==6){df21 <- res}else{df21 <- rbind(df21, res)}
}
} else {
  df21 <- data.frame(est = character(), indicador = character(), unidad = character(), periodo = character(), pendiente = numeric(), p_valor = numeric())
}

# Qmin, Caudal mínimo
if (!is.null(q)) {
for(i in 6:ncol(q)){
    qmin.an <- aggregate(q[,i], list(q[,3]), min)[,2]
    qmin.season <- aggregate(q[,i], list(q[,4], q[,5]), min)
    qmin.ot <- qmin.season[which(qmin.season[,1]==1),3]
    qmin.inv <- qmin.season[which(qmin.season[,1]==2),3]
    qmin.pri <- qmin.season[which(qmin.season[,1]==3),3]
    qmin.ver <- qmin.season[which(qmin.season[,1]==4),3]
    qmin.ver <- qmin.ver[-length(qmin.ver)] # Saco último verano al estar incompleto
    
    pend.an <- lm(qmin.an ~ c(1:length(qmin.an)))$coefficients[2]*10 # Una década son 10 años
    mkp.an <- mk.test(qmin.an)$p.value
    pend.ot <- lm(qmin.ot ~ c(1:length(qmin.ot)))$coefficients[2]*10 # Una década son 10 años
    mkp.ot <- mk.test(qmin.ot)$p.value
    pend.inv <- lm(qmin.inv ~ c(1:length(qmin.inv)))$coefficients[2]*10 # Una década son 10 años
    mkp.inv <- mk.test(qmin.inv)$p.value
    pend.pri <- lm(qmin.pri ~ c(1:length(qmin.pri)))$coefficients[2]*10 # Una década son 10 años
    mkp.pri <- mk.test(qmin.pri)$p.value
    pend.ver <- lm(qmin.ver ~ c(1:length(qmin.ver)))$coefficients[2]*10 # Una década son 10 años
    mkp.ver <- mk.test(qmin.ver)$p.value
    res <-  rbind(
        data.frame(est=colnames(q)[i], indicador="Qmin", unidad = "m3/s", periodo="Anual", pendiente=pend.an, p_valor=mkp.an),
        data.frame(est=colnames(q)[i], indicador="Qmin", unidad = "m3/s", periodo="Otono", pendiente=pend.ot, p_valor=mkp.ot),
        data.frame(est=colnames(q)[i], indicador="Qmin", unidad = "m3/s", periodo="Invierno", pendiente=pend.inv, p_valor=mkp.inv),
        data.frame(est=colnames(q)[i], indicador="Qmin", unidad = "m3/s", periodo="Primavera", pendiente=pend.pri, p_valor=mkp.pri),
        data.frame(est=colnames(q)[i], indicador="Qmin", unidad = "m3/s", periodo="Verano", pendiente=pend.ver, p_valor=mkp.ver)
    )
    if(i==6){d22 <- res}else{d22 <- rbind(d22, res)}
}
} else {
  d22 <- data.frame(est = character(), indicador = character(), unidad = character(), periodo = character(), pendiente = numeric(), p_valor = numeric())
}

resi <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, 
                df13, df14, df15, df16, df17, df18, df19, df20, df21  ,d22)
resi <- data.frame(Nombre=metadatos$Estacion[match(resi$est, metadatos$Codigo)],
                    Lat=metadatos$Lat[match(resi$est, metadatos$Codigo)],
                    Lon=metadatos$Lon[match(resi$est, metadatos$Codigo)],
                    resi)

write.csv(resi, paste0(carpeta_output, "/", name, "_Indicadores_extremos.csv"), row.names = FALSE)
}
