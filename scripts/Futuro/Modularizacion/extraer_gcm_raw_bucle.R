#codigo que tiene la función para extraer datos para diferentes combinaciones de modelos, variables y escenarios

# Cargar librerías y funciones
source("backend/librerias_extraer_gcm.R")
source("backend/list_files_h5_nc_csv_txt.R") 
source("backend/gcm_raw_cmip.R")

extraer_gcm_cmip <- function(esc, vars, d_cmip, sits.c,
                        models.b, interp, dir.out){
  
  for(g in 1:length(esc)){
    esci <- esc[g]
    for(i in 1:length(vars)){
      var <- vars[i]
      d_cmip6 <- paste0(d_cmip,var,"/")
      bucle.sec(esci, d_cmip6, var, models.b, interp, sits.c, dir.out)
    }#Cierre variable "i"
  }#Cierre escenario "g"
}

#Lo SIGUIENTE POR REVISAR

# Ejecución desde línea de comandos (ejecutar desde caracterizacion_historica):
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 4) {
    var      <- args[1]
    modelo   <- args[2]
    esc      <- args[3]
    tempo    <- args[4]
    
    vars <- c("pr","tas", "tasmax", "tasmin")
    esc <- c("ssp370","ssp245","ssp585")
    interp <- "Sí"
    ini.h <- 1985
    
    models.b <- c("MPI-ESM1-2-LR","BCC-CSM2-MR","CanESM5","HadGEM3-GC31-LL","MIROC-ES2L") 
    
    d_cmip <- "//146.155.26.34/Datos/GCM/CMIP6/amon/"
    sits.c <- read.csv(paste0(dir, "/Grilla_AMC.csv")) #coordenadas a las que se les obtendra las series
    colnames(sits.c) <- c("FID", "lat", "lon")
    dir_out <- "output/"
    
    gcm_raw(
      varname = var,
      d_cmip = d_cmip,
      modelo = modelo,
      crds = crds,
      tempo = tempo,
      esc = esc,
      dir_out = dir_out
    )
    
  } else {
    
    message("Uso:")
    message("Rscript scripts/MAIN/1.extraer_gcm_raw.R pr MPI-ESM1-2-LR ssp585 M")
    
  }
}