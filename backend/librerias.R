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
# - stringr → Manejo y manipulación de texto.
# - lubridate → Trabajo con fechas y tiempos.
# - tidyr, purrr → Organización y manipulación avanzada de datos.
# - readr, zoo, forcats → Lectura y manipulación de datos tabulares.
#
# **Manejo de archivos y formatos:**
# - readxl, writexl → Lectura y escritura de archivos Excel.
# - jsonlite, rjson, RJSONIO → Lectura y escritura de archivos JSON.
# - zip → Compresión y descompresión de archivos.
#
# **Solicitudes web y APIs:**
# - httr, RCurl → Realización de peticiones a APIs y descarga de datos web.
# - XML → Procesamiento de datos en formato XML.
#
# **Visualización de datos:**
# - ggplot2, patchwork → Creación de gráficos y composición de visualizaciones.
#
# **Interfaz de usuario en R:**
# - shiny → Desarrollo de aplicaciones web interactivas.
#
# **Utilidades varias:**
# - dotenv → Carga de variables de entorno desde un archivo .env.
# - beepr → Generación de alertas sonoras en el código.

bibliotecas <- c("DBI", "RPostgres", "dplyr", "tidyverse", "dotenv", 
                 "readxl", "writexl", "stringr", "lubridate", "tidyr", 
                 "purrr", "beepr", "rjson", "RCurl", "XML", "RJSONIO", 
                 "httr", "ggplot2", "readr", "zoo", "patchwork", "forcats", 
                 "shiny", "zip", "jsonlite", "rvest") 

for (libreria in bibliotecas) {
  if (!require(libreria, character.only = TRUE)) {
    install.packages(libreria, dependencies = TRUE)
  }
  library(libreria, character.only = TRUE)
}