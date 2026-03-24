
#Codigo que solo tiene la función para enlistar archivos consultados en la carpeta de ruta "dir"

#funcion para enlistar archivos
# - dir es el directorio que se debe enlistar
# - subfolder es alguna carpeta al interior del directorio que se quiera enlistar, si no se asume nulo
# - pattern es el patrón que se busca enlistar: ".txt", ".h5", ".nc", "csv
# - type es el tipo de archivo. Es necesario para entregar nombres a columnas
# - vars es el nombre de la variable utilizada para un filtro requerido cuando se enlistan archivos .nc
# - models es el nombre del modelo utilizado para un filtro requerido cuando se enlistan archivos .nc
# - esci es el escenario de emisión utilizado para un filtro requerido cuando se enlistan archivos .nc

list_file <- function(dir, subfolder = NULL, pattern,
                      type = c("txt","h5","nc","csv"),
                      vars = NULL, models = NULL, esci = NULL){
  
  type <- match.arg(type)
  
  # Construcción del path
  path <- if (!is.null(subfolder)) file.path(dir, subfolder) else dir
  
  # Listado de archivos
  files <- list.files(path, pattern = pattern)
  
  if (length(files) == 0) {
    stop("No se encontraron archivos con ese patrón.")
  }
  
  # Separación por "_"
  ftab <- as.data.frame(do.call("rbind", strsplit(files, "_")),
                        stringsAsFactors = FALSE)
  
  # Agrego nombre completo
  ftab$longname <- files
  
  # ---- Asignación dinámica de nombres ----
  
  if (type == "txt") {
    
    if (ncol(ftab) - 1 != 3)
      stop("Los archivos .txt no tienen 3 campos esperados.")
    
    colnames(ftab)[1:3] <- c("data", "Lat", "Lon")
    
  }
  
  if (type == "h5") {
    
    if (ncol(ftab) - 1 != 4)
      stop("Los archivos .h5 no tienen 4 campos esperados.")
    
    colnames(ftab)[1:4] <- c("var", "model", "esc", "rea")
    
  }
  
  if (type == "nc") {
    
    if (ncol(ftab) - 1 != 7)
      stop("Los archivos .nc no tienen 7 campos esperados.")
    
    colnames(ftab)[1:7] <- c("var", "time", "model",
                             "per", "vari", "nn", "int")
    
    # Remuevo ".nc" de la última columna
    ftab$int <- substr(ftab$int, 1, nchar(ftab$int) - 3)
    
    # ---- Filtros opcionales ----
    
    if (!is.null(esci)) {
      ftab <- ftab[
        which(
          (ftab$model %in% ftab$model[ftab$per == esci] &
             ftab$per == "historical") |
            ftab$per == esci
        ), ]
    }
    
    if (!is.null(vars)) {
      ftab <- ftab[ftab$var %in% vars, ]
    }
    
    if (!is.null(models)) {
      ftab <- ftab[ftab$model %in% models, ]
    }
    
    ftab <- ftab[order(ftab$model), ]
    
  }
  
  if (type == "csv") {
    
    if (ncol(ftab) - 1 != 6)
      stop("Los archivos .csv no tienen 6 campos esperados.")
    
    colnames(ftab)[1:6] <- c("var", "model", "esc", "tempo", "otro", "est")
    
  }
  
  return(list(files = files, table = ftab))
}