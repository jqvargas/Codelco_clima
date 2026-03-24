
#codigo que tiene la función para extraer datos para diferentes combinaciones de modelos, variables y escenarios

# Cargar librerías y funciones
source("backend/librerias_extraer_arclim.R")
source("backend/list_files_h5_nc_csv_txt.R")
source("backend/gcm_raw_arclim.R")

extraer_gcm_arclim <- function(vars, models, escenarios, tempos,
                        d_cmip, crds, dir_out){
  
  for (varname in vars){
    for (modelo in models){
      for (esc in escenarios){
        for (tempo in tempos){
          
          gcm_raw(
            varname = varname,
            d_cmip = d_cmip,
            modelo = modelo,
            crds = crds,
            tempo = tempo,
            esc = esc,
            dir_out = dir_out
          )
          
        }
      }
    }
  }
  
}

# Ejecución desde línea de comandos (ejecutar desde caracterizacion_historica):
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 4) {
    var      <- args[1]
    modelo   <- args[2]
    esc      <- args[3]
    tempo    <- args[4]
    
    d_cmip <- "//146.155.26.60/Datos/ARClim_CMIP6"
    crds <- read.csv("input/coordenadas.csv")
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