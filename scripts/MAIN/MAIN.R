#Definir directorio de trabajo
# setwd("C:/Users/CCG UC/OneDrive - Universidad Católica de Chile/uc365_Codelco_RiesgosCC - General/C1y2-Desarrollo/1-Clima/caracterizacion_historica")

# ==============================================================================
# PARÁMETROS GLOBALES
# Definidos una única vez aquí. Todos los scripts del pipeline leen estos valores
# desde el entorno global; ningún script los redefine fuera de este bloque.
# ==============================================================================
ano_inicio           <- 1990
ano_fin              <- 2024
calidad_por_variable <- c(pp = 70, temp = 60, q = 70)
# ==============================================================================

# Cargar funciones y librerías
source("backend/connection.R")
source("backend/funciones_utilidad.R")
source("backend/librerias.R")
source("backend/queries.R")
source("scripts/MAIN/0.analisis_calidad_estaciones.R")
source("scripts/MAIN/1.seleccion_estaciones.R")
source("scripts/MAIN/2.obtener_series_brutas.R")
source("scripts/MAIN/3.depurado_datos.R")
source("scripts/MAIN/4.rellenar_series_missForest.R")
source("scripts/MAIN/4.5.filtrar_estaciones_shapefile.R")
source("scripts/MAIN/5.agregar_series_mensuales_missForest.R")
source("scripts/MAIN/6.tendencias_historica.R")
source("scripts/MAIN/graficar.R")

#conectar a la base de datos CCG-UC
connection <- connect_to_db("backend/ATT15580.env")

# 0. Paso previo: Explorar la calidad de las estaciones
#estaciones <- filtrar_estaciones(connection, 0, ano_inicio, ano_fin)

# 0.1. Gráficos de análisis de calidad de las estaciones
#dir_estaciones <- "output/output_seleccion_estaciones/estaciones"
#dir_graficos_calidad <- "output/output_seleccion_estaciones/graficos_calidad"
#generar_graficos_calidad_estaciones_0(
#  dir_estaciones = dir_estaciones,
#  dir_graficos   = dir_graficos_calidad,
#  ano_inicio     = ano_inicio,
#  ano_fin        = ano_fin
#)

# 1. Seleccionar las estaciones de acuerdo con umbral de calidad y por variable
#    (cada variable puede tener un umbral distinto)
for (var in names(calidad_por_variable)) {
  filtrar_estaciones(connection, calidad_por_variable[var], ano_inicio, ano_fin, variable = var)
}

# 2. Obtener series brutas (pp, temp, q para El Teniente y El Salvador)
#    Lee estaciones_*_el_{division}_{calidad}_{ano_inicio}_{ano_fin}.csv
#    usando la calidad definida para cada variable
dir_estaciones <- "output/output_seleccion_estaciones/estaciones"
dir_series     <- "output/output_seleccion_estaciones/series/brutas"
for (var in c("pp", "temp", "q")) {
  for (div in c("el_teniente", "el_salvador")) {
    obtener_series_brutas(
      connection     = connection,
      variable       = var,
      division       = div,
      calidad        = calidad_por_variable[var],
      ano_inicio     = ano_inicio,
      ano_fin        = ano_fin,
      dir_estaciones = dir_estaciones,
      dir_series     = dir_series
    )
  }
}

# 3. Depuración de outliers en series brutas (marcar como NA antes del relleno)
dir_depurado <- "output/output_seleccion_estaciones/series/depurado"
ejecutar_depurado_datos(
  dir_brutas            = dir_series,
  dir_depurado          = dir_depurado,
  calidad_por_variable  = calidad_por_variable,
  ano_inicio            = ano_inicio,
  ano_fin               = ano_fin,
  usar_limites_fisicos  = TRUE,
  usar_iqr              = TRUE,
  k_iqr                 = 1.5
)

# 4. Rellenar series con missForest (lee desde series depuradas)
ejecutar_rellenado_missForest(
  dir_series_brutas    = dir_depurado,
  calidad_por_variable  = calidad_por_variable,
  ano_inicio            = ano_inicio,
  ano_fin               = ano_fin,
  ntree                 = 100,
  usar_tiempo           = TRUE
)

# 4.5. Filtrar estaciones por shapefile (Envolvente DET / Envolvente DSAL)
#      después del rellenado, antes de agregar series mensuales.
#      Origen: series/rellenas. Salida series: ./series_filtradas_envolvente (raíz; rutas cortas).
#      Los CSV originales estaciones_* no se tocan; las tablas recortadas se guardan en la misma
#      carpeta .../estaciones como estaciones_filtradas_* (mismo sufijo cal/años; ruta más corta).
dir_series_rellenas <- "output/output_seleccion_estaciones/series/rellenas"
dir_series_rellenas_envolvente <- "series_filtradas_envolvente"
ejecutar_filtrado_shapefile(
  dir_rellenas            = dir_series_rellenas,
  dir_rellenas_filtradas  = dir_series_rellenas_envolvente,
  dir_estaciones          = "output/output_seleccion_estaciones/estaciones",
  dir_shapefiles          = "scripts/envolvente",
  calidad_por_variable    = calidad_por_variable,
  ano_inicio              = ano_inicio,
  ano_fin                 = ano_fin,
  actualizar_estaciones   = TRUE,
  verbose                 = TRUE
)

# 5. Agregar series rellenas (missForest) a escala mensual → series/mensual
#    (usa series ya recortadas por envolvente)
agregar_series_mensuales_missForest(
  dir_rellenas = dir_series_rellenas_envolvente,
  dir_mensual  = "output/output_seleccion_estaciones/series/mensual"
)

# 5.1. Agregar series rellenas a escala anual → series/anual
#      Misma lógica: PP = suma, Temp y Caudal = media
agregar_series_anuales_missForest(
  dir_rellenas = dir_series_rellenas_envolvente,
  dir_anual    = "output/output_seleccion_estaciones/series/anual"
)

# 5.2. Resumen de periodo completo → 8 CSV + Excel consolidado (una hoja, 8 filas)
#      PP: promedio del acumulado anual (mm/año); temp y caudal: promedio diario del periodo.
#      Salida: series/historico/*_historico.csv y resumen_historico_consolidado.xlsx
exportar_promedio_historico_csv(
  dir_rellenas = dir_series_rellenas_envolvente,
  dir_salida   = "output/output_seleccion_estaciones/series/historico",
  verbose      = TRUE
)

#  6. Tendencias históricas e indicadores extremos
#      Requiere el vector completo calidad_por_variable (pp, temp y q) para armar
#      los nombres de archivo de cada variable; no pasar un solo umbral.
ejecutar_tendencias_historica(
  dir_series_rellenas  = dir_series_rellenas_envolvente,
  calidad_por_variable = calidad_por_variable,
  ano_inicio           = ano_inicio,
  ano_fin              = ano_fin,
  dir_output           = "output/tendencias_historica"
)

# 7. Gráficos de resultados de tendencias (Salvador y Teniente)
graficar_tendencias_finales(
  dir_tendencias = "output/tendencias_historica",
  dir_graficos   = "output/tendencias_historica/graficos",
  verbose        = TRUE
)
