# =============================================================================
# Genera los 6 mapas (2 divisiones × 3 variables) sin re-filtrar series.
# - Puntos y N total: metadatos en estaciones/todas filtrados por umbral de
#   calidad (misma lógica que el paso 1), = población antes del corte por shapefile.
# - Color dentro/fuera: intersección con el polígono de la envolvente.
# - caption: comparación con códigos presentes en los CSV rellena_missForest.
#
# Uso desde la carpeta caracterizacion_historica:
#   Rscript scripts/MAIN/generar_mapas_diagnostico_shapefile.R
# O en R: source("scripts/MAIN/generar_mapas_diagnostico_shapefile.R"); main_generar_mapas()
# =============================================================================

# Raíz del proyecto (caracterizacion_historica). Ajuste si ejecuta desde otro sitio.
PROJ_ROOT <- "C:/Users/CCG UC/OneDrive - Universidad Católica de Chile/uc365_Codelco_RiesgosCC - General/C1y2-Desarrollo/1-Clima/caracterizacion_historica"

ano_inicio <- 1990
ano_fin <- 2024
calidad_por_variable <- c(pp = 70, temp = 60, q = 70)

leer_codigos_desde_rellena <- function(path) {
  if (!file.exists(path)) return(character(0))
  df <- readr::read_csv(path, show_col_types = FALSE, n_max = 2)
  df <- as.data.frame(df)
  cols_fecha <- c("fecha", "Fecha", "year", "Year", "month", "Month", "day", "Day")
  cols <- setdiff(names(df), intersect(cols_fecha, names(df)))
  cols <- cols[vapply(cols, function(c) is.numeric(df[[c]]), logical(1))]
  vapply(cols, function(col) {
    if (startsWith(col, "X")) sub("^X", "", gsub("\\.", "-", col)) else col
  }, character(1))
}

filtrar_meta_desde_todas <- function(var, div_short, cal, ini, fin, dir_todas) {
  if (var == "pp") {
    f <- file.path(dir_todas, sprintf("estaciones_pp_el_%s_0_%s_%s.csv", div_short, ini, fin))
  } else if (var == "temp") {
    f <- file.path(dir_todas, sprintf("estaciones_temp_el_%s_0_%s_%s.csv", div_short, ini, fin))
  } else {
    f <- file.path(dir_todas, sprintf("estaciones_caudal_el_%s_0_%s_%s.csv", div_short, ini, fin))
  }
  if (!file.exists(f)) stop("No existe: ", f)
  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  if (var == "pp") {
    d[d$calidad_pp >= cal, , drop = FALSE]
  } else if (var == "temp") {
    d[d$calidad_t_max > cal & d$calidad_t_min > cal, , drop = FALSE]
  } else {
    d[d$calidad_q >= cal, , drop = FALSE]
  }
}

main_generar_mapas <- function(
  proj = PROJ_ROOT,
  ano_ini = ano_inicio,
  ano_f = ano_fin,
  calidades = calidad_por_variable
) {
  setwd(proj)
  source(file.path(proj, "scripts/MAIN/4.5.filtrar_estaciones_shapefile.R"), local = FALSE)

  dir_todas <- file.path(proj, "output/output_seleccion_estaciones/estaciones/todas")
  dir_rellenas <- file.path(proj, "output/output_seleccion_estaciones/series/rellenas")
  dir_shapefiles <- file.path(proj, "scripts/envolvente")
  dir_mapas <- file.path(proj, "output/output_seleccion_estaciones/mapas_diagnostico_shapefile")
  dir_cache <- file.path(proj, "output/output_seleccion_estaciones/_cache_gadm_chile")

  div_archivo <- c(el_teniente = "teniente", el_salvador = "salvador")

  chile_sf <- obtener_sf_chile(dir_cache = dir_cache, verbose = TRUE)

  for (division in c("el_teniente", "el_salvador")) {
    archivo_shp <- file.path(dir_shapefiles, shapefiles_division[division])
    if (!file.exists(archivo_shp)) {
      warning("Shapefile no encontrado: ", archivo_shp)
      next
    }
    shp <- sf::st_read(archivo_shp, quiet = TRUE)
    div <- div_archivo[division]

    for (var in c("pp", "temp", "q")) {
      cal <- calidades[var]
      if (is.na(cal)) next

      meta <- tryCatch(
        filtrar_meta_desde_todas(var, div, cal, ano_ini, ano_f, dir_todas),
        error = function(e) {
          warning(conditionMessage(e))
          NULL
        }
      )
      if (is.null(meta) || nrow(meta) == 0) {
        message("Sin metadata para ", division, " / ", var, "; se omite mapa.")
        next
      }

      codigos_poly <- filtrar_estaciones_por_shapefile(meta, shp)

      if (var == "pp") {
        rel <- file.path(dir_rellenas, sprintf("series_pp_el_%s_%s_%s_%s_rellena_missForest.csv", div, cal, ano_ini, ano_f))
        codes_rel <- leer_codigos_desde_rellena(rel)
      } else if (var == "temp") {
        rel1 <- file.path(dir_rellenas, sprintf("series_t_max_el_%s_%s_%s_%s_rellena_missForest.csv", div, cal, ano_ini, ano_f))
        rel2 <- file.path(dir_rellenas, sprintf("series_t_min_el_%s_%s_%s_%s_rellena_missForest.csv", div, cal, ano_ini, ano_f))
        codes_rel <- unique(c(leer_codigos_desde_rellena(rel1), leer_codigos_desde_rellena(rel2)))
      } else {
        rel <- file.path(dir_rellenas, sprintf("series_q_el_%s_%s_%s_%s_rellena_missForest.csv", div, cal, ano_ini, ano_f))
        codes_rel <- leer_codigos_desde_rellena(rel)
      }

      cap <- sprintf(
        "Población meta (calidad, desde estaciones/todas): N=%i | Dentro polígono: %i | Códigos en serie rellena: %i",
        nrow(meta), length(codigos_poly), length(codes_rel)
      )

      arch_map <- file.path(dir_mapas, paste0("mapa_filtrado_", division, "_", var, "_", ano_ini, "_", ano_f, ".png"))
      guardar_mapa_diagnostico_division(
        estaciones = meta,
        shp = shp,
        codigos_dentro = codigos_poly,
        division = division,
        archivo_salida = arch_map,
        variable_codigo = var,
        etiqueta_shapefile = basename(archivo_shp),
        caption = cap,
        chile_sf = chile_sf,
        verbose = TRUE
      )
    }
  }
  message("Listo. Mapas en: ", dir_mapas)
  invisible(dir_mapas)
}

if (!interactive()) {
  main_generar_mapas()
}
