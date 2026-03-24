
#Codigo que solo tiene las librerías necesarias para extraer datos desde ARClim .h5

# --------------------------------------------------------------
# Carga de librerías necesarias para el proyecto de base de datos
# --------------------------------------------------------------
# Este script instala y carga las librerías esenciales para la 
# conexión a la base de datos, manipulación de datos, gestión 
# de archivos y otros procesos clave.
#
# **Conexión a la base de datos:**
# - DBI, RPostgres → Conexión y gestión de bases de datos PostgreSQL.
#
# **Manipulación y análisis de datos:**
# - dplyr, tidyverse → Procesamiento y transformación de datos.
# - phylin → Cálculos estadísticos espaciales
# - stringr → Manejo y manipulación de texto.
# - rhdf5 → Lectura y manipulación de archivos ".h5".
#
# **Manejo de archivos y formatos:**
# - data.table → lectura y escritura de archivos en formato data.table (largo)
#
# **Solicitudes web y APIs:**
# - httr, RCurl → Realización de peticiones a APIs y descarga de datos web.
# - XML → Procesamiento de datos en formato XML.
#

bibliotecas <- c("DBI", "RPostgres", "phylin", "stringr", "dplyr", "rhdf5", "data.table", "RCurl", "XML", "httr") 

for (libreria in bibliotecas) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
  }
  library(libreria, character.only = TRUE)
}

