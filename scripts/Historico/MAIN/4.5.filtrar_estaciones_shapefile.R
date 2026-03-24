# =============================================================================
# 4.5 — Filtrar estaciones por shapefile (DET / DSAL)
# Tras rellenado missForest; antes de agregar a mensual/anual.
# Lee series en output/.../series/rellenas y escribe CSV de series recortadas (solo utils I/O).
# Lee CSV de estaciones solo como insumo; con actualizar_estaciones escribe copias filtradas
# (prefijo estaciones_filtradas_) en la misma carpeta dir_estaciones por defecto (ruta más corta en Windows).
# Opcional: dir_estaciones_filtradas = file.path(dir_estaciones, "estaciones_filtradas_envolvente").
# Por defecto salida en la raíz del proyecto: ./series_filtradas_envolvente
# Requiere: sf, ggplot2 (mapas). Opcional mapa Chile: geodata o rnaturalearth
# =============================================================================

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Instale sf: install.packages(\"sf\")")
}
library(sf)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Instale ggplot2: install.packages(\"ggplot2\")")
}

# --- Rutas por defecto (working directory = carpeta caracterizacion_historica) ---
dir_series_default       <- "output/output_seleccion_estaciones/series"
dir_estaciones_default   <- "output/output_seleccion_estaciones/estaciones"
dir_shapefiles_default   <- "scripts/envolvente"
# Salida corta bajo la raíz del proyecto (evita rutas enormes bajo output/...)
dir_filtradas_default    <- "series_filtradas_envolvente"
dir_mapas_default        <- "mapas_shapefile"

shapefiles_division <- c(
  el_teniente = "Envolvente DET.shp",
  el_salvador = "Envolvente DSAL.shp"
)

read_csv_safe <- function(path) {
  d <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (!is.null(d)) return(d)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# En Windows, write.csv(..., fileEncoding = "UTF-8") a veces falla al abrir el destino
# ("cannot open file" / "No such file or directory") con rutas largas, OneDrive o encoding
# de ruta; escribir en tempdir() y file.copy evita ese fallo.
write_csv_utf8_via_temp <- function(x, dest_path) {
  d <- dirname(dest_path)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d_abs <- normalizePath(d, winslash = "/", mustWork = TRUE)
  dest_full <- file.path(d_abs, basename(dest_path))
  tmp <- tempfile(pattern = "csvutf8_", tmpdir = tempdir(), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.csv(x, tmp, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  ok <- file.copy(from = tmp, to = dest_full, overwrite = TRUE)
  if (!ok || !file.exists(dest_full)) {
    stop("No se pudo escribir el CSV: ", dest_full)
  }
  invisible(dest_full)
}

resolver_ruta_estaciones <- function(dir_estaciones, variable, div, cal, ano_inicio, ano_fin) {
  prefix <- if (variable == "pp") {
    paste0("estaciones_pp_el_", div)
  } else if (variable == "temp") {
    paste0("estaciones_temp_el_", div)
  } else {
    paste0("estaciones_caudal_el_", div)
  }
  cand <- c(
    file.path(dir_estaciones, paste0(prefix, "_cal_", cal, "_", ano_inicio, "_", ano_fin, ".csv")),
    file.path(dir_estaciones, paste0(prefix, "_", cal, "_", ano_inicio, "_", ano_fin, ".csv"))
  )
  for (p in cand) if (file.exists(p)) return(p)
  NULL
}

match_columna_estacion <- function(nombres_columnas, codigo) {
  cod <- as.character(codigo)
  if (cod %in% nombres_columnas) return(cod)
  alt <- paste0("X", gsub("-", "\\.", cod))
  if (alt %in% nombres_columnas) return(alt)
  NULL
}

filtrar_estaciones_por_shapefile <- function(estaciones, shp, col_lat = NULL, col_lon = NULL) {
  nms <- names(estaciones)
  if (is.null(col_lat)) col_lat <- if ("lat" %in% nms) "lat" else if ("Lat" %in% nms) "Lat" else stop("Falta lat/Lat")
  if (is.null(col_lon)) col_lon <- if ("lon" %in% nms) "lon" else if ("Lon" %in% nms) "Lon" else stop("Falta lon/Lon")
  if (nrow(estaciones) == 0) return(character(0))
  pts <- st_as_sf(estaciones, coords = c(col_lon, col_lat), crs = 4326, remove = FALSE)
  shp <- st_transform(shp, st_crs(pts))
  dentro <- lengths(st_intersects(pts, shp)) > 0
  estaciones$codigo_nacional[dentro]
}

obtener_sf_chile <- function(dir_cache = NULL, verbose = TRUE) {
  if (is.null(dir_cache)) dir_cache <- tempdir()
  if (!dir.exists(dir_cache)) dir.create(dir_cache, recursive = TRUE)
  if (requireNamespace("geodata", quietly = TRUE)) {
    g <- tryCatch(
      {
        r <- geodata::gadm(country = "CHL", level = 1, path = dir_cache)
        sf::st_as_sf(r)
      },
      error = function(e) NULL
    )
    if (!is.null(g) && nrow(g) > 0) {
      if (verbose) message("Mapa base: GADM Chile (geodata).")
      return(g)
    }
  }
  if (requireNamespace("rnaturalearth", quietly = TRUE)) {
    v <- tryCatch(
      rnaturalearth::ne_countries(scale = "medium", country = "Chile", returnclass = "sf"),
      error = function(e) NULL
    )
    if (!is.null(v) && nrow(v) > 0) {
      if (verbose) message("Mapa base: Natural Earth (contorno Chile).")
      return(v)
    }
  }
  if (verbose) message("Sin mapa base: geodata o rnaturalearth (opcional).")
  NULL
}

guardar_mapa_diagnostico_division <- function(
  estaciones, shp, codigos_dentro, division, archivo_salida,
  variable_codigo = NULL, etiqueta_shapefile = NULL, caption = NULL,
  chile_sf = NULL, margen_frac = 0.18, verbose = TRUE
) {
  if (is.null(estaciones) || nrow(estaciones) == 0) {
    if (verbose) message("  Mapa omitido (sin estaciones): ", division)
    return(invisible(NULL))
  }
  nms <- names(estaciones)
  col_lat <- if ("lat" %in% nms) "lat" else if ("Lat" %in% nms) "Lat" else stop("lat/Lat")
  col_lon <- if ("lon" %in% nms) "lon" else if ("Lon" %in% nms) "Lon" else stop("lon/Lon")
  cn <- as.character(estaciones$codigo_nacional)
  estaciones$codigo_nacional <- cn
  estaciones$.dentro <- cn %in% as.character(codigos_dentro)
  pts <- st_as_sf(estaciones, coords = c(col_lon, col_lat), crs = 4326, remove = FALSE)
  shp_wgs <- st_transform(shp, 4326)
  shp_union <- st_union(shp_wgs)
  bb <- st_bbox(shp_union)
  dx <- (bb["xmax"] - bb["xmin"]) * margen_frac
  dy <- (bb["ymax"] - bb["ymin"]) * margen_frac
  xlim <- c(unname(bb["xmin"] - dx), unname(bb["xmax"] + dx))
  ylim <- c(unname(bb["ymin"] - dy), unname(bb["ymax"] + dy))
  n_in <- sum(estaciones$.dentro)
  n_out <- nrow(estaciones) - n_in
  labels_var <- c(pp = "Precipitación", temp = "Temperatura", q = "Caudal")
  bloque_div <- if (!is.null(variable_codigo) && variable_codigo %in% names(labels_var)) {
    paste0(division, " · ", unname(labels_var[variable_codigo]))
  } else {
    division
  }
  titulo <- sprintf("%s — dentro: %i / fuera: %i (total N=%i)", bloque_div, n_in, n_out, nrow(estaciones))
  p <- ggplot2::ggplot()
  if (!is.null(chile_sf)) {
    chile_wgs <- tryCatch(st_transform(chile_sf, 4326), error = function(e) NULL)
    if (!is.null(chile_wgs)) {
      p <- p + ggplot2::geom_sf(data = chile_wgs, fill = "gray92", color = "gray55", linewidth = 0.25)
    }
  }
  p <- p +
    ggplot2::geom_sf(data = shp_union, fill = NA, color = "#08519c", linewidth = 0.9) +
    ggplot2::geom_sf(data = pts, ggplot2::aes(color = .data$.dentro, shape = .data$.dentro), size = 2.2, stroke = 0.35) +
    ggplot2::scale_color_manual(
      name = "Envolvente", values = c(`TRUE` = "#238b45", `FALSE` = "#cb181d"),
      labels = c(`TRUE` = "Dentro", `FALSE` = "Fuera"), breaks = c(TRUE, FALSE), drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      name = "Envolvente", values = c(`TRUE` = 16, `FALSE` = 4),
      labels = c(`TRUE` = "Dentro", `FALSE` = "Fuera"), breaks = c(TRUE, FALSE), drop = FALSE
    ) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, crs = sf::st_crs(4326)) +
    ggplot2::labs(
      title = titulo,
      subtitle = if (is.null(etiqueta_shapefile) || !nzchar(etiqueta_shapefile)) NULL else etiqueta_shapefile,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.major = ggplot2::element_line(color = "gray88", linewidth = 0.3)
    )
  if (!dir.exists(dirname(archivo_salida))) dir.create(dirname(archivo_salida), recursive = TRUE)
  ggplot2::ggsave(archivo_salida, p, width = 9, height = 8, dpi = 160, bg = "white")
  if (verbose) message("  Mapa guardado: ", archivo_salida)
  invisible(archivo_salida)
}

# Lectura/escritura solo base R (CSV estándar, sin readr/vroom)
filtrar_una_serie_por_codigos <- function(archivo_origen, codigos_filtrados, archivo_destino, verbose = TRUE) {
  if (!file.exists(archivo_origen)) return(invisible(NULL))
  if (length(codigos_filtrados) == 0) {
    if (verbose) message("  Sin estaciones dentro; omito: ", basename(archivo_origen))
    return(invisible(NULL))
  }

  df <- read_csv_safe(archivo_origen)
  cols_fecha <- c("fecha", "Fecha", "year", "Year", "month", "Month", "day", "Day")
  cols_fecha <- intersect(cols_fecha, names(df))
  cols_estaciones <- setdiff(names(df), cols_fecha)
  cols_estaciones <- cols_estaciones[vapply(cols_estaciones, function(c) is.numeric(df[[c]]), logical(1))]

  columnas_a_mantener <- character(0)
  for (cod in codigos_filtrados) {
    m <- match_columna_estacion(cols_estaciones, cod)
    if (!is.null(m)) columnas_a_mantener <- c(columnas_a_mantener, m)
  }
  if (length(columnas_a_mantener) == 0) {
    if (verbose) message("  Sin columnas coincidentes: ", basename(archivo_origen))
    return(invisible(NULL))
  }

  cols_finales <- c(intersect(c("fecha", "year", "month", "day"), names(df)), columnas_a_mantener)
  df_filtrado <- df[, cols_finales, drop = FALSE]

  ddir <- dirname(archivo_destino)
  if (!dir.exists(ddir)) dir.create(ddir, recursive = TRUE)
  write_csv_utf8_via_temp(df_filtrado, archivo_destino)

  if (verbose) {
    message(
      "  Filtrado: ", basename(archivo_origen), " → ", basename(archivo_destino),
      " (", length(columnas_a_mantener), " estaciones; antes ", length(cols_estaciones), ")"
    )
  }
  invisible(df_filtrado)
}

ejecutar_filtrado_shapefile <- function(
  dir_rellenas             = file.path(dir_series_default, "rellenas"),
  dir_rellenas_filtradas   = NULL,
  dir_estaciones           = dir_estaciones_default,
  dir_estaciones_filtradas = NULL,
  dir_shapefiles           = dir_shapefiles_default,
  calidad_por_variable,
  ano_inicio,
  ano_fin,
  divisiones               = c("el_teniente", "el_salvador"),
  actualizar_estaciones    = TRUE,
  generar_mapas_diagnostico = TRUE,
  dir_mapas                = NULL,
  dir_cache_geodata        = NULL,
  margen_mapa              = 0.18,
  verbose                  = TRUE
) {
  if (!dir.exists(dir_rellenas)) stop("No existe carpeta rellenas: ", dir_rellenas)
  if (!dir.exists(dir_shapefiles)) stop("No existe carpeta shapefiles: ", dir_shapefiles)

  if (is.null(dir_rellenas_filtradas)) {
    dir_rellenas_filtradas <- dir_filtradas_default
  }
  if (is.null(dir_mapas)) {
    dir_mapas <- dir_mapas_default
  }
  if (is.null(dir_cache_geodata)) {
    dir_cache_geodata <- file.path(tempdir(), "gadm_chile_shapefile")
  }

  if (!dir.exists(dir_rellenas_filtradas)) {
    dir.create(dir_rellenas_filtradas, recursive = TRUE)
  }

  dir_estaciones_salida <- dir_estaciones_filtradas
  if (is.null(dir_estaciones_salida)) {
    dir_estaciones_salida <- dir_estaciones
  }

  if (actualizar_estaciones) {
    if (!dir.exists(dir_estaciones)) {
      stop("No existe carpeta estaciones: ", dir_estaciones)
    }
    if (file.exists(dir_estaciones_salida) && !dir.exists(dir_estaciones_salida)) {
      stop(
        "La ruta de salida existe pero no es una carpeta (elimine o renombre el archivo): ",
        dir_estaciones_salida
      )
    }
    if (!dir.exists(dir_estaciones_salida)) {
      dc <- dir.create(dir_estaciones_salida, recursive = TRUE)
      if (!dc || !dir.exists(dir_estaciones_salida)) {
        stop(
          "No se pudo crear la carpeta de salida de estaciones filtradas: ",
          dir_estaciones_salida
        )
      }
    }
  }

  chile_sf <- NULL
  if (generar_mapas_diagnostico) {
    chile_sf <- obtener_sf_chile(dir_cache = dir_cache_geodata, verbose = verbose)
  }

  if (verbose) {
    message("\n========== FILTRADO POR SHAPEFILE ==========")
    message("Series recortadas → ", dir_rellenas_filtradas)
    if (actualizar_estaciones) {
      message("Estaciones filtradas (CSV nuevos) → ", dir_estaciones_salida)
    }
    if (generar_mapas_diagnostico) message("Mapas → ", dir_mapas)
    message("Origen series (sin tocar) → ", dir_rellenas)
    message("Origen estaciones (sin tocar) → ", dir_estaciones, "\n")
  }

  div_archivo <- c(el_teniente = "teniente", el_salvador = "salvador")

  for (division in divisiones) {
    archivo_shp <- file.path(dir_shapefiles, shapefiles_division[division])
    if (!file.exists(archivo_shp)) {
      if (verbose) message("Shapefile no encontrado, omito ", division, ": ", archivo_shp)
      next
    }
    shp <- st_read(archivo_shp, quiet = TRUE)
    div <- div_archivo[division]
    if (verbose) message("--- ", division, " ---")

    for (var in c("pp", "temp", "q")) {
      cal <- calidad_por_variable[var]
      if (is.na(cal)) next

      arch_est <- resolver_ruta_estaciones(dir_estaciones, var, div, cal, ano_inicio, ano_fin)
      if (is.null(arch_est)) {
        if (verbose) message("  ", var, ": no hay CSV de estaciones. Omitido.")
        next
      }

      estaciones <- read_csv_safe(arch_est)
      codigos_dentro <- filtrar_estaciones_por_shapefile(estaciones, shp)
      if (verbose) {
        message("  ", var, ": ", length(codigos_dentro), " / ", nrow(estaciones), " estaciones en envolvente")
      }

      if (generar_mapas_diagnostico) {
        arch_map <- file.path(
          dir_mapas,
          paste0("mapa_filtrado_", division, "_", var, "_", ano_inicio, "_", ano_fin, ".png")
        )
        guardar_mapa_diagnostico_division(
          estaciones, shp, codigos_dentro, division, arch_map,
          variable_codigo = var, etiqueta_shapefile = basename(archivo_shp),
          chile_sf = chile_sf, margen_frac = margen_mapa, verbose = verbose
        )
      }

      if (actualizar_estaciones && length(codigos_dentro) > 0) {
        estaciones_filtradas <- estaciones[as.character(estaciones$codigo_nacional) %in% as.character(codigos_dentro), , drop = FALSE]
        bn_out <- sub("^estaciones_", "estaciones_filtradas_", basename(arch_est))
        arch_est_filtrado <- file.path(dir_estaciones_salida, bn_out)
        write_csv_utf8_via_temp(estaciones_filtradas, arch_est_filtrado)
        if (verbose) message("  CSV estaciones (nuevo): ", bn_out)
      }

      if (var == "pp") {
        bn <- paste0("series_pp_el_", div, "_", cal, "_", ano_inicio, "_", ano_fin, "_rellena_missForest.csv")
        filtrar_una_serie_por_codigos(
          file.path(dir_rellenas, bn), codigos_dentro,
          file.path(dir_rellenas_filtradas, bn), verbose
        )
      } else if (var == "temp") {
        for (serie in c("t_max", "t_min")) {
          bn <- paste0("series_", serie, "_el_", div, "_", cal, "_", ano_inicio, "_", ano_fin, "_rellena_missForest.csv")
          filtrar_una_serie_por_codigos(
            file.path(dir_rellenas, bn), codigos_dentro,
            file.path(dir_rellenas_filtradas, bn), verbose
          )
        }
      } else {
        bn <- paste0("series_q_el_", div, "_", cal, "_", ano_inicio, "_", ano_fin, "_rellena_missForest.csv")
        filtrar_una_serie_por_codigos(
          file.path(dir_rellenas, bn), codigos_dentro,
          file.path(dir_rellenas_filtradas, bn), verbose
        )
      }
    }
  }

  if (verbose) {
    message("\nListo. Recortadas en: ", dir_rellenas_filtradas, "\n==========================================\n")
  }
  invisible(dir_rellenas_filtradas)
}
