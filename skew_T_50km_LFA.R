#######################################################################
###CODIGO PARA REALIZAR UN ESUQEMA SKEW-T A PARTIR DE SALIDOS DE GFS###
####################DECKER GUZMAN ZABALAGA#############################
############LABORATORIO DE FÍSICA DE LA ATMÓSFERA######################
#######################################################################

rm(list=ls(all=TRUE))# ELIMINAMOS LA MEMORIA
library(rNOMADS)
#install.packages("remotes")
#remotes::install_github("bczernecki/thunder")
library(rvest)
library(thunder)
{
#AEROPUERTO INTERNACIONAL DE EL ALTO
#SE COLOCA LAS COORDENADAS DESEADAS EN GRADOS DECIMALES 
#aeropuerto del alto 
lat <- -16.52
lon <- -68.18

#BUSCAMOS LA ULTIMA EJECUCIÓN DEL MODELO DEL SISTEMA DE PRONOSTICO GLOBAL PARA 50KM
model.urls <- GetDODSDates("gfs_0p50")
#resolucion <- 50
latest.model <- tail(model.urls$url, 1)

model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

#FABRICAMOS LOS NODOS DEL MODELO MAS CERCANO
lons <- seq(0, 360, by = 0.50)
lats <- seq(-90, 90, by = 0.50)


lon.diff <- abs(lon + 360 - lons)
lat.diff <- abs(lat - lats)
model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 
model.lat.ind <- which(lat.diff == min(lat.diff)) - 1

#SUBCONJUNTO DEL MODELO Y ESTADO DEL MODELO PARA SU INICIO
time <- c(0,0) 
lon.inds <- c(model.lon.ind - 2, model.lon.ind + 2)
lat.inds <- c(model.lat.ind - 2, model.lat.ind + 2)
levels <- c(0,46) #TODAS LOS NIVELES DE PRESION QUE BRINDA GFS 46
#COLOCAMOS LAS VARIABLES DESEADAS
variables <- c("tmpprs", "hgtprs", "ugrdprs", "vgrdprs","rhprs") 
#EXTRAEMOS LAS VARIABLES REQUERIDAS
model.data <- DODSGrab(latest.model, latest.model.run, variables,
                       time, lon.inds, lat.inds, levels)



#0View(model.data$forecast.date)
#INTERPOLAMOS EN EL PUNTO DE INTERES 
profile <- BuildProfile(model.data, lon, lat,
                        spatial.average = TRUE, points = 4)

#EXTRAEMOS LAS VARIABLES COMO VECTOR PARA LUEGO REALIZAR UN DATA FRAME
tmp <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "tmpprs"),] - 272.15 #°C
hgt <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "hgtprs"),]
u <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),] *1.94384# EN NUDOS
v <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]*1.94384
rh <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "rhprs"),]
#CALCULAMOS EL PUNTO DE ROCIO
dpt<- tmp - ((100 - rh)/5)
#CALCULAMOS EL VECTOR VELOCIDAD EN NUDOS
ws<- round(sqrt(u^2+v^2),2)
#CALCULAMOS LA ALTURA A PARTIR DE LA ALTURA GEOPONTECIAL 
h<-hgt/9.80665
h_ok <- 63710088*hgt/(63710088-hgt)
h_ok <-round(h_ok,0)#REDONDEAMOS 
#CALCULAMOS LA PRESION PARA CADA ALTURA 
press <- 1013.25*(1-0.0000225577*h_ok)^5.2559

#CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS A PARTIR DE U Y V
alfa1<-0# si u<0 y v<0, 
alfa2<-180#si v>0,
alfa3<-360#si u>0 y v<0.
#REALIZAMOS NUESTRO DATA FRAME
df<-data.frame(hgt,tmp,u,v,rh,dpt,ws,h,h_ok,press)
#CREAMOS UNA FUNCION PARA CONVERTIR RADIANES A GRADOS
rad2deg <- function(rad) {(rad * 180) / (pi)}
#REALIZAMOS 3 CONDICIONALES PARA EL VALOR DE ALFA
df$alfa<-ifelse(df$v>0,alfa2,
                ifelse(df$v<0 & df$u<0,alfa1,
                       ifelse(df$v<0 & df$u>0,alfa3,df$alfa==0)))
#CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS Y AGREGAMOS AL DATA FRAME
df$wd<-round(rad2deg(atan(u/v)) + df$alfa,0) 
#altitud [m] (puede estar sobre el nivel del mar o sobre el nivel del suelo,
#ya que la función siempre considera el primer nivel como superficie, 
#es decir, h = 0 m) - altitud [metros] sllp

df<-df[-c(1:12),]#RECORTAMOS LAS PRIMERAS 6 FILAS 

#COLOCAMOS LAS VARIABLES REQUERIDAS PARA REALIZAR EL ESQUEMA
pressure = df$press # PRESION [hPa]
altitude = df$h_ok # ALTITUD [m]
temp = df$tmp  # TEMPERATURA [°C]
dpt = df$dpt # TEMP PUNTO DE ROCIO [°C]
wd = df$wd # DIRECCION DEL VIENTO [°]
ws =df$ws # VELOCIDAD DEL VIENTO [nudos]#accuracy = 2 


setwd("/home/decker/figuras_skewt_gfs_50km")
sounding_save(filename = "EL_ALTO_AEROPUERTO-12z_50km_26-08-2021_46niveles.png",max_speed = 40,
              title = "El Alto Aeropuerto GFS 50km",
              pressure, altitude, temp, dpt, wd, ws, parcel = "SB")
} # AEROPUERTO DEL ALTO
{

  #SE COLOCA LAS COORDENADAS DESEADAS EN GRADOS DECIMALES 
  # Trinidad          
  lat <- -14.8205
  lon <- -64.917

  #BUSCAMOS LA ULTIMA EJECUCIÓN DEL MODELO DEL SISTEMA DE PRONOSTICO GLOBAL PARA 50KM
  model.urls <- GetDODSDates("gfs_0p50")
  #resolucion <- 50
  latest.model <- tail(model.urls$url, 1)
  
  model.runs <- GetDODSModelRuns(latest.model)
  latest.model.run <- tail(model.runs$model.run, 1)
  
  #FABRICAMOS LOS NODOS DEL MODELO MAS CERCANO
  lons <- seq(0, 360, by = 0.50)
  lats <- seq(-90, 90, by = 0.50)
  
  
  lon.diff <- abs(lon + 360 - lons)
  lat.diff <- abs(lat - lats)
  model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 
  model.lat.ind <- which(lat.diff == min(lat.diff)) - 1
  
  #SUBCONJUNTO DEL MODELO Y ESTADO DEL MODELO PARA SU INICIO
  time <- c(0,0) 
  lon.inds <- c(model.lon.ind - 2, model.lon.ind + 2)
  lat.inds <- c(model.lat.ind - 2, model.lat.ind + 2)
  levels <- c(0,46) #TODAS LOS NIVELES DE PRESION QUE BRINDA GFS 46
  #COLOCAMOS LAS VARIABLES DESEADAS
  variables <- c("tmpprs", "hgtprs", "ugrdprs", "vgrdprs","rhprs") 
  #EXTRAEMOS LAS VARIABLES REQUERIDAS
  model.data <- DODSGrab(latest.model, latest.model.run, variables,
                         time, lon.inds, lat.inds, levels)
  
  #INTERPOLAMOS EN EL PUNTO DE INTERES 
  profile <- BuildProfile(model.data, lon, lat,
                          spatial.average = TRUE, points = 4)
  
  #EXTRAEMOS LAS VARIABLES COMO VECTOR PARA LUEGO REALIZAR UN DATA FRAME
  tmp <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "tmpprs"),] - 272.15 #°C
  hgt <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "hgtprs"),]
  u <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),] *1.94384# EN NUDOS
  v <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]*1.94384
  rh <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "rhprs"),]
  #CALCULAMOS EL PUNTO DE ROCIO
  dpt<- tmp - ((100 - rh)/5)
  #CALCULAMOS EL VECTOR VELOCIDAD EN NUDOS
  ws<- round(sqrt(u^2+v^2),2)
  #CALCULAMOS LA ALTURA A PARTIR DE LA ALTURA GEOPONTECIAL 
  h<-hgt/9.80665
  h_ok <- 63710088*hgt/(63710088-hgt)
  h_ok <-round(h_ok,0)#REDONDEAMOS 
  #CALCULAMOS LA PRESION PARA CADA ALTURA 
  press <- 1013.25*(1-0.0000225577*h_ok)^5.2559
  
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS A PARTIR DE U Y V
  alfa1<-0# si u<0 y v<0, 
  alfa2<-180#si v>0,
  alfa3<-360#si u>0 y v<0.
  #REALIZAMOS NUESTRO DATA FRAME
  df<-data.frame(hgt,tmp,u,v,rh,dpt,ws,h,h_ok,press)
  #CREAMOS UNA FUNCION PARA CONVERTIR RADIANES A GRADOS
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  #REALIZAMOS 3 CONDICIONALES PARA EL VALOR DE ALFA
  df$alfa<-ifelse(df$v>0,alfa2,
                  ifelse(df$v<0 & df$u<0,alfa1,
                         ifelse(df$v<0 & df$u>0,alfa3,df$alfa==0)))
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS Y AGREGAMOS AL DATA FRAME
  df$wd<-round(rad2deg(atan(u/v)) + df$alfa,0) 
  #altitud [m] (puede estar sobre el nivel del mar o sobre el nivel del suelo,
  #ya que la función siempre considera el primer nivel como superficie, 
  #es decir, h = 0 m) - altitud [metros] sllp

  
  #COLOCAMOS LAS VARIABLES REQUERIDAS PARA REALIZAR EL ESQUEMA
  pressure = df$press # PRESION [hPa]
  altitude = df$h_ok # ALTITUD [m]
  temp = df$tmp  # TEMPERATURA [°C]
  dpt = df$dpt # TEMP PUNTO DE ROCIO [°C]
  wd = df$wd # DIRECCION DEL VIENTO [°]
  ws =df$ws # VELOCIDAD DEL VIENTO [nudos]#accuracy = 2 
  
  
  setwd("/home/decker/figuras_skewt_gfs_50km")
  sounding_save(filename = "TRINIDAD-12z_50km_26-08-2021_46niveles.png",max_speed = 40,
                title = "Trinidad GFS 50km",
                pressure, altitude, temp, dpt, wd, ws, parcel = "SB")
  } # TRINIDAD
{
  #SE COLOCA LAS COORDENADAS DESEADAS EN GRADOS DECIMALES 
  # Santa Cruz       
  lat <- -17.65
  lon <- -66.13
  
  #BUSCAMOS LA ULTIMA EJECUCIÓN DEL MODELO DEL SISTEMA DE PRONOSTICO GLOBAL PARA 50KM
  model.urls <- GetDODSDates("gfs_0p50")
  #resolucion <- 50
  latest.model <- tail(model.urls$url, 1)
  
  model.runs <- GetDODSModelRuns(latest.model)
  latest.model.run <- tail(model.runs$model.run, 1)
  
  #FABRICAMOS LOS NODOS DEL MODELO MAS CERCANO
  lons <- seq(0, 360, by = 0.50)
  lats <- seq(-90, 90, by = 0.50)
  
  
  lon.diff <- abs(lon + 360 - lons)
  lat.diff <- abs(lat - lats)
  model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 
  model.lat.ind <- which(lat.diff == min(lat.diff)) - 1
  
  #SUBCONJUNTO DEL MODELO Y ESTADO DEL MODELO PARA SU INICIO
  time <- c(0,0) 
  lon.inds <- c(model.lon.ind - 2, model.lon.ind + 2)
  lat.inds <- c(model.lat.ind - 2, model.lat.ind + 2)
  levels <- c(0,46) #TODAS LOS NIVELES DE PRESION QUE BRINDA GFS 46
  #COLOCAMOS LAS VARIABLES DESEADAS
  variables <- c("tmpprs", "hgtprs", "ugrdprs", "vgrdprs","rhprs") 
  #EXTRAEMOS LAS VARIABLES REQUERIDAS
  model.data <- DODSGrab(latest.model, latest.model.run, variables,
                         time, lon.inds, lat.inds, levels)
  
  #INTERPOLAMOS EN EL PUNTO DE INTERES 
  profile <- BuildProfile(model.data, lon, lat,
                          spatial.average = TRUE, points = 4)
  
  #EXTRAEMOS LAS VARIABLES COMO VECTOR PARA LUEGO REALIZAR UN DATA FRAME
  tmp <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "tmpprs"),] - 272.15 #°C
  hgt <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "hgtprs"),]
  u <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),] *1.94384# EN NUDOS
  v <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]*1.94384
  rh <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "rhprs"),]
  #CALCULAMOS EL PUNTO DE ROCIO
  dpt<- tmp - ((100 - rh)/5)
  #CALCULAMOS EL VECTOR VELOCIDAD EN NUDOS
  ws<- round(sqrt(u^2+v^2),2)
  #CALCULAMOS LA ALTURA A PARTIR DE LA ALTURA GEOPONTECIAL 
  h<-hgt/9.80665
  h_ok <- 63710088*hgt/(63710088-hgt)
  h_ok <-round(h_ok,0)#REDONDEAMOS 
  #CALCULAMOS LA PRESION PARA CADA ALTURA 
  press <- 1013.25*(1-0.0000225577*h_ok)^5.2559
  
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS A PARTIR DE U Y V
  alfa1<-0# si u<0 y v<0, 
  alfa2<-180#si v>0,
  alfa3<-360#si u>0 y v<0.
  #REALIZAMOS NUESTRO DATA FRAME
  df<-data.frame(hgt,tmp,u,v,rh,dpt,ws,h,h_ok,press)
  #CREAMOS UNA FUNCION PARA CONVERTIR RADIANES A GRADOS
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  #REALIZAMOS 3 CONDICIONALES PARA EL VALOR DE ALFA
  df$alfa<-ifelse(df$v>0,alfa2,
                  ifelse(df$v<0 & df$u<0,alfa1,
                         ifelse(df$v<0 & df$u>0,alfa3,df$alfa==0)))
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS Y AGREGAMOS AL DATA FRAME
  df$wd<-round(rad2deg(atan(u/v)) + df$alfa,0) 
  #altitud [m] (puede estar sobre el nivel del mar o sobre el nivel del suelo,
  #ya que la función siempre considera el primer nivel como superficie, 
  #es decir, h = 0 m) - altitud [metros] sllp
  df<-df[-c(1:2),]#RECORTAMOS LAS PRIMERAS 6 FILAS 
  
  #COLOCAMOS LAS VARIABLES REQUERIDAS PARA REALIZAR EL ESQUEMA
  pressure = df$press # PRESION [hPa]
  altitude = df$h_ok # ALTITUD [m]
  temp = df$tmp  # TEMPERATURA [°C]
  dpt = df$dpt # TEMP PUNTO DE ROCIO [°C]
  wd = df$wd # DIRECCION DEL VIENTO [°]
  ws =df$ws # VELOCIDAD DEL VIENTO [nudos]#accuracy = 2 
  
  
  setwd("/home/decker/figuras_skewt_gfs_50km")
  sounding_save(filename = "Santa_Cruz-12z_50km_26-08-2021_46niveles.png",max_speed = 40,
                title = "Santa Cruz GFS 50km",
                pressure, altitude, temp, dpt, wd, ws, parcel = "SB") 
} # Santa cruz
{
  
  #SE COLOCA LAS COORDENADAS DESEADAS EN GRADOS DECIMALES 
  # Oruro             
  lat <- -18.05
  lon <- -67.07
  
  #BUSCAMOS LA ULTIMA EJECUCIÓN DEL MODELO DEL SISTEMA DE PRONOSTICO GLOBAL PARA 50KM
  model.urls <- GetDODSDates("gfs_0p50")
  #resolucion <- 50
  latest.model <- tail(model.urls$url, 1)
  
  model.runs <- GetDODSModelRuns(latest.model)
  latest.model.run <- tail(model.runs$model.run, 1)
  
  #FABRICAMOS LOS NODOS DEL MODELO MAS CERCANO
  lons <- seq(0, 360, by = 0.50)
  lats <- seq(-90, 90, by = 0.50)
  
  
  lon.diff <- abs(lon + 360 - lons)
  lat.diff <- abs(lat - lats)
  model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 
  model.lat.ind <- which(lat.diff == min(lat.diff)) - 1
  
  #SUBCONJUNTO DEL MODELO Y ESTADO DEL MODELO PARA SU INICIO
  time <- c(0,0) 
  lon.inds <- c(model.lon.ind - 2, model.lon.ind + 2)
  lat.inds <- c(model.lat.ind - 2, model.lat.ind + 2)
  levels <- c(0,46) #TODAS LOS NIVELES DE PRESION QUE BRINDA GFS 46
  #COLOCAMOS LAS VARIABLES DESEADAS
  variables <- c("tmpprs", "hgtprs", "ugrdprs", "vgrdprs","rhprs") 
  #EXTRAEMOS LAS VARIABLES REQUERIDAS
  model.data <- DODSGrab(latest.model, latest.model.run, variables,
                         time, lon.inds, lat.inds, levels)
  
  
  
  #0View(model.data$forecast.date)
  #INTERPOLAMOS EN EL PUNTO DE INTERES 
  profile <- BuildProfile(model.data, lon, lat,
                          spatial.average = TRUE, points = 4)
  
  #EXTRAEMOS LAS VARIABLES COMO VECTOR PARA LUEGO REALIZAR UN DATA FRAME
  tmp <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "tmpprs"),] - 272.15 #°C
  hgt <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "hgtprs"),]
  u <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),] *1.94384# EN NUDOS
  v <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]*1.94384
  rh <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "rhprs"),]
  #CALCULAMOS EL PUNTO DE ROCIO
  dpt<- tmp - ((100 - rh)/5)
  #CALCULAMOS EL VECTOR VELOCIDAD EN NUDOS
  ws<- round(sqrt(u^2+v^2),2)
  #CALCULAMOS LA ALTURA A PARTIR DE LA ALTURA GEOPONTECIAL 
  h<-hgt/9.80665
  h_ok <- 63710088*hgt/(63710088-hgt)
  h_ok <-round(h_ok,0)#REDONDEAMOS 
  #CALCULAMOS LA PRESION PARA CADA ALTURA 
  press <- 1013.25*(1-0.0000225577*h_ok)^5.2559
  
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS A PARTIR DE U Y V
  alfa1<-0# si u<0 y v<0, 
  alfa2<-180#si v>0,
  alfa3<-360#si u>0 y v<0.
  #REALIZAMOS NUESTRO DATA FRAME
  df<-data.frame(hgt,tmp,u,v,rh,dpt,ws,h,h_ok,press)
  #CREAMOS UNA FUNCION PARA CONVERTIR RADIANES A GRADOS
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  #REALIZAMOS 3 CONDICIONALES PARA EL VALOR DE ALFA
  df$alfa<-ifelse(df$v>0,alfa2,
                  ifelse(df$v<0 & df$u<0,alfa1,
                         ifelse(df$v<0 & df$u>0,alfa3,df$alfa==0)))
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS Y AGREGAMOS AL DATA FRAME
  df$wd<-round(rad2deg(atan(u/v)) + df$alfa,0) 
  #altitud [m] (puede estar sobre el nivel del mar o sobre el nivel del suelo,
  #ya que la función siempre considera el primer nivel como superficie, 
  #es decir, h = 0 m) - altitud [metros] sllp
  
  df<-df[-c(1:12),]#RECORTAMOS LAS PRIMERAS 6 FILAS 
  
  #COLOCAMOS LAS VARIABLES REQUERIDAS PARA REALIZAR EL ESQUEMA
  pressure = df$press # PRESION [hPa]
  altitude = df$h_ok # ALTITUD [m]
  temp = df$tmp  # TEMPERATURA [°C]
  dpt = df$dpt # TEMP PUNTO DE ROCIO [°C]
  wd = df$wd # DIRECCION DEL VIENTO [°]
  ws =df$ws # VELOCIDAD DEL VIENTO [nudos]#accuracy = 2 
  
  
  setwd("/home/decker/figuras_skewt_gfs_50km")
  sounding_save(filename = "Oruro-12z_50km_26-08-2021_46niveles.png",max_speed = 40,
                title = "Oruro GFS 50km",
                pressure, altitude, temp, dpt, wd, ws, parcel = "SB") 
}#Oruro
{
  #SE COLOCA LAS COORDENADAS DESEADAS EN GRADOS DECIMALES 
  # Cochabamba   
  lat <- -17.45
  lon <- -66.10
  
  #BUSCAMOS LA ULTIMA EJECUCIÓN DEL MODELO DEL SISTEMA DE PRONOSTICO GLOBAL PARA 50KM
  model.urls <- GetDODSDates("gfs_0p50")
  #resolucion <- 50
  latest.model <- tail(model.urls$url, 1)
  
  model.runs <- GetDODSModelRuns(latest.model)
  latest.model.run <- tail(model.runs$model.run, 1)
  
  #FABRICAMOS LOS NODOS DEL MODELO MAS CERCANO
  lons <- seq(0, 360, by = 0.50)
  lats <- seq(-90, 90, by = 0.50)
  
  
  lon.diff <- abs(lon + 360 - lons)
  lat.diff <- abs(lat - lats)
  model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 
  model.lat.ind <- which(lat.diff == min(lat.diff)) - 1
  
  #SUBCONJUNTO DEL MODELO Y ESTADO DEL MODELO PARA SU INICIO
  time <- c(0,0) 
  lon.inds <- c(model.lon.ind - 2, model.lon.ind + 2)
  lat.inds <- c(model.lat.ind - 2, model.lat.ind + 2)
  levels <- c(0,46) #TODAS LOS NIVELES DE PRESION QUE BRINDA GFS 46
  #COLOCAMOS LAS VARIABLES DESEADAS
  variables <- c("tmpprs", "hgtprs", "ugrdprs", "vgrdprs","rhprs") 
  #EXTRAEMOS LAS VARIABLES REQUERIDAS
  model.data <- DODSGrab(latest.model, latest.model.run, variables,
                         time, lon.inds, lat.inds, levels)
  
  #INTERPOLAMOS EN EL PUNTO DE INTERES 
  profile <- BuildProfile(model.data, lon, lat,
                          spatial.average = TRUE, points = 4)
  
  #EXTRAEMOS LAS VARIABLES COMO VECTOR PARA LUEGO REALIZAR UN DATA FRAME
  tmp <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "tmpprs"),] - 272.15 #°C
  hgt <- profile[[1]]$profile.data[,which(profile[[1]]$variables == "hgtprs"),]
  u <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),] *1.94384# EN NUDOS
  v <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]*1.94384
  rh <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "rhprs"),]
  #CALCULAMOS EL PUNTO DE ROCIO
  dpt<- tmp - ((100 - rh)/5)
  #CALCULAMOS EL VECTOR VELOCIDAD EN NUDOS
  ws<- round(sqrt(u^2+v^2),2)
  #CALCULAMOS LA ALTURA A PARTIR DE LA ALTURA GEOPONTECIAL 
  h<-hgt/9.80665
  h_ok <- 63710088*hgt/(63710088-hgt)
  h_ok <-round(h_ok,0)#REDONDEAMOS 
  #CALCULAMOS LA PRESION PARA CADA ALTURA 
  press <- 1013.25*(1-0.0000225577*h_ok)^5.2559
  
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS A PARTIR DE U Y V
  alfa1<-0# si u<0 y v<0, 
  alfa2<-180#si v>0,
  alfa3<-360#si u>0 y v<0.
  #REALIZAMOS NUESTRO DATA FRAME
  df<-data.frame(hgt,tmp,u,v,rh,dpt,ws,h,h_ok,press)
  #CREAMOS UNA FUNCION PARA CONVERTIR RADIANES A GRADOS
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  #REALIZAMOS 3 CONDICIONALES PARA EL VALOR DE ALFA
  df$alfa<-ifelse(df$v>0,alfa2,
                  ifelse(df$v<0 & df$u<0,alfa1,
                         ifelse(df$v<0 & df$u>0,alfa3,df$alfa==0)))
  #CALCULAMOS LA DIRECCION DEL VIENTO EN GRADOS Y AGREGAMOS AL DATA FRAME
  df$wd<-round(rad2deg(atan(u/v)) + df$alfa,0) 
  #altitud [m] (puede estar sobre el nivel del mar o sobre el nivel del suelo,
  #ya que la función siempre considera el primer nivel como superficie, 
  #es decir, h = 0 m) - altitud [metros] sllp
  df<-df[-c(1:10),]#RECORTAMOS LAS PRIMERAS 6 FILAS 
  
  #COLOCAMOS LAS VARIABLES REQUERIDAS PARA REALIZAR EL ESQUEMA
  pressure = df$press # PRESION [hPa]
  altitude = df$h_ok # ALTITUD [m]
  temp = df$tmp  # TEMPERATURA [°C]
  dpt = df$dpt # TEMP PUNTO DE ROCIO [°C]
  wd = df$wd # DIRECCION DEL VIENTO [°]
  ws =df$ws # VELOCIDAD DEL VIENTO [nudos]#accuracy = 2 
  
  
  setwd("/home/decker/figuras_skewt_gfs_50km")
  sounding_save(filename = "Cochabamba-12z_50km_26-08-2021_46niveles.png",max_speed = 40,
                title = "Cochabamba GFS 50km",
                pressure, altitude, temp, dpt, wd, ws, parcel = "SB") 
}# cochabamba
