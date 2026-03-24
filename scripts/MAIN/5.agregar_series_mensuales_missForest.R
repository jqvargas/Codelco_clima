# =============================================================================
# Script: 5.agregar_series_mensuales_missForest.R
# Lee las series rellenas con missForest desde series/rellenas y las agrega
# a escala mensual y anual. Salida en series/mensual y series/anual.
#
# Reglas de agregación (mensual y anual):
#   - Temperatura (t_max, t_min): media
#   - Precipitación: acumulado (suma)
#   - Caudal: promedio (media)
#
# Solo procesa archivos *_rellena_missForest.csv.
# Mensual: fecha (primer día del mes), year, month, estaciones...
# Anual:   fecha (primer día del año), year, estaciones...
#
# Uso: agregar_series_mensuales_missForest(dir_rellenas, dir_mensual)
#      agregar_series_anuales_missForest(dir_rellenas, dir_anual)
#      exportar_promedio_historico_csv(...)  → CSV por serie + Excel consolidado (opcional)
#      graficar_serie_mensual(estacion, variable, division, dir_mensual, ...)
# =============================================================================

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# Inferir tipo de variable desde el nombre del archivo
# series_pp_*, series_t_max_*, series_t_min_* -> "pp" o "temp"
# series_q_* -> "q"
# -----------------------------------------------------------------------------
inferir_variable_archivo <- function(nombre_archivo) {
  bn <- basename(nombre_archivo)
  if (grepl("^series_pp_", bn)) return("pp")
  if (grepl("^series_t_max_|^series_t_min_", bn)) return("temp")
  if (grepl("^series_q_", bn)) return("q")
  NA_character_
}

# -----------------------------------------------------------------------------
# Cargar CSV de serie rellena y normalizar columnas de fecha
# -----------------------------------------------------------------------------
cargar_serie_rellena <- function(archivo) {
  if (!file.exists(archivo)) stop("No existe: ", archivo)
  df <- read_csv(archivo, show_col_types = FALSE)
  df <- as.data.frame(df)
  nms <- names(df)
  for (par in list(c("Fecha", "fecha"), c("Year", "year"), c("Month", "month"), c("Day", "day"))) {
    if (par[1] %in% nms && !(par[2] %in% nms)) names(df)[names(df) == par[1]] <- par[2]
  }
  if (!"fecha" %in% names(df)) stop("Se requiere columna 'fecha' o 'Fecha': ", archivo)
  df$fecha <- as.Date(df$fecha)
  if (!"year" %in% names(df))  df$year  <- as.integer(year(df$fecha))
  if (!"month" %in% names(df)) df$month <- as.integer(month(df$fecha))
  df
}

# -----------------------------------------------------------------------------
# Columnas de estaciones (numéricas, excluyendo fecha/year/month/day)
# -----------------------------------------------------------------------------
columnas_estaciones <- function(df) {
  cols_fecha <- c("fecha", "year", "month", "day")
  cols_fecha <- intersect(cols_fecha, names(df))
  candidatas <- setdiff(names(df), cols_fecha)
  candidatas[vapply(candidatas, function(c) is.numeric(df[[c]]), logical(1))]
}

# -----------------------------------------------------------------------------
# Agregar una serie diaria a mensual
# variable: "temp" -> media; "pp" -> suma; "q" -> media
# Devuelve data.frame con fecha (primer día del mes), year, month y columnas de estaciones
# -----------------------------------------------------------------------------
agregar_mensual <- function(df, variable = c("temp", "pp", "q")) {
  variable <- match.arg(variable)
  estaciones <- columnas_estaciones(df)
  if (length(estaciones) == 0) stop("No hay columnas de estaciones numéricas")

  if (variable == "pp") {
    # Precipitación: acumulado mensual
    agg <- df %>%
      group_by(year, month) %>%
      summarise(across(all_of(estaciones), ~ sum(., na.rm = TRUE)), .groups = "drop")
  } else {
    # Temperatura y caudal: media mensual
    agg <- df %>%
      group_by(year, month) %>%
      summarise(across(all_of(estaciones), ~ mean(., na.rm = TRUE)), .groups = "drop")
  }

  # fecha = primer día del mes (sin columna day)
  agg$fecha <- as.Date(paste(agg$year, agg$month, "01", sep = "-"))
  agg <- agg %>% relocate(fecha, .before = year)
  agg
}

# -----------------------------------------------------------------------------
# Agregar una serie diaria a anual
# variable: "temp" -> media; "pp" -> suma; "q" -> media (misma lógica que mensual)
# Devuelve data.frame con year, fecha (primer día del año) y columnas de estaciones
# -----------------------------------------------------------------------------
agregar_anual <- function(df, variable = c("temp", "pp", "q")) {
  variable <- match.arg(variable)
  estaciones <- columnas_estaciones(df)
  if (length(estaciones) == 0) stop("No hay columnas de estaciones numéricas")

  if (variable == "pp") {
    # Precipitación: acumulado anual
    agg <- df %>%
      group_by(year) %>%
      summarise(across(all_of(estaciones), ~ sum(., na.rm = TRUE)), .groups = "drop")
  } else {
    # Temperatura y caudal: media anual
    agg <- df %>%
      group_by(year) %>%
      summarise(across(all_of(estaciones), ~ mean(., na.rm = TRUE)), .groups = "drop")
  }

  # fecha = primer día del año
  agg$fecha <- as.Date(paste(agg$year, "01", "01", sep = "-"))
  agg <- agg %>% relocate(fecha, .before = year)
  agg
}

# -----------------------------------------------------------------------------
# Procesar un archivo *_rellena_missForest.csv y guardar serie mensual
# -----------------------------------------------------------------------------
procesar_una_serie_mensual <- function(archivo_entrada,
                                      dir_mensual,
                                      verbose = TRUE) {
  bn <- basename(archivo_entrada)
  if (!grepl("_rellena_missForest\\.csv$", bn)) {
    if (verbose) message("  Omitido (no es *_rellena_missForest.csv): ", bn)
    return(invisible(NULL))
  }

  variable <- inferir_variable_archivo(archivo_entrada)
  if (is.na(variable)) {
    if (verbose) message("  Omitido (tipo de variable no reconocido): ", bn)
    return(invisible(NULL))
  }

  if (verbose) message("  Procesando ", bn, " (variable: ", variable, ")")

  df <- cargar_serie_rellena(archivo_entrada)
  mensual <- agregar_mensual(df, variable = variable)

  # Crear directorio de salida si no existe
  if (!dir.exists(dir_mensual)) dir.create(dir_mensual, recursive = TRUE)
  # Nombre de salida: quitar _rellena_missForest, añadir _mensual.csv
  nombre_salida <- gsub("_rellena_missForest\\.csv$", "_mensual.csv", bn)
  archivo_salida <- file.path(dir_mensual, nombre_salida)
  write_csv(mensual, archivo_salida)
  if (verbose) message("    -> ", nombre_salida)

  invisible(mensual)
}

# -----------------------------------------------------------------------------
# Agregar a mensual todas las series rellenas con missForest
# dir_rellenas: carpeta con *_rellena_missForest.csv (ej. output/.../series/rellenas)
# dir_mensual: carpeta de salida (ej. output/.../series/mensual)
# -----------------------------------------------------------------------------
agregar_series_mensuales_missForest <- function(dir_rellenas = "output/output_seleccion_estaciones/series/rellenas",
                                                dir_mensual = "output/output_seleccion_estaciones/series/mensual",
                                                verbose = TRUE) {
  if (!dir.exists(dir_rellenas)) {
    stop("No existe la carpeta de series rellenas: ", dir_rellenas)
  }
  if (!dir.exists(dir_mensual)) {
    dir.create(dir_mensual, recursive = TRUE)
    if (verbose) message("Carpeta creada: ", dir_mensual)
  }

  archivos <- list.files(dir_rellenas, pattern = "_rellena_missForest\\.csv$", full.names = TRUE)
  if (length(archivos) == 0) {
    message("No se encontraron archivos *_rellena_missForest.csv en ", dir_rellenas)
    return(invisible(list()))
  }

  if (verbose) message("Agregando ", length(archivos), " series a escala mensual (missForest)...")

  resultados <- list()
  for (i in seq_along(archivos)) {
    resultados[[basename(archivos[i])]] <- procesar_una_serie_mensual(
      archivos[i], dir_mensual, verbose = verbose
    )
  }

  if (verbose) message("Series mensuales guardadas en: ", dir_mensual)
  invisible(resultados)
}

# -----------------------------------------------------------------------------
# Procesar un archivo *_rellena_missForest.csv y guardar serie anual
# -----------------------------------------------------------------------------
procesar_una_serie_anual <- function(archivo_entrada,
                                    dir_anual,
                                    verbose = TRUE) {
  bn <- basename(archivo_entrada)
  if (!grepl("_rellena_missForest\\.csv$", bn)) {
    if (verbose) message("  Omitido (no es *_rellena_missForest.csv): ", bn)
    return(invisible(NULL))
  }

  variable <- inferir_variable_archivo(archivo_entrada)
  if (is.na(variable)) {
    if (verbose) message("  Omitido (tipo de variable no reconocido): ", bn)
    return(invisible(NULL))
  }

  if (verbose) message("  Procesando ", bn, " (variable: ", variable, ") -> anual")

  df <- cargar_serie_rellena(archivo_entrada)
  anual <- agregar_anual(df, variable = variable)

  # Crear directorio de salida si no existe
  if (!dir.exists(dir_anual)) dir.create(dir_anual, recursive = TRUE)
  # Nombre de salida: quitar _rellena_missForest, añadir _anual.csv
  nombre_salida <- gsub("_rellena_missForest\\.csv$", "_anual.csv", bn)
  archivo_salida <- file.path(dir_anual, nombre_salida)
  write_csv(anual, archivo_salida)
  if (verbose) message("    -> ", nombre_salida)

  invisible(anual)
}

# -----------------------------------------------------------------------------
# Agregar a escala anual todas las series rellenas con missForest
# Misma lógica que mensual: PP = suma, Temp y Caudal = media
# dir_rellenas: carpeta con *_rellena_missForest.csv
# dir_anual: carpeta de salida (output/.../series/anual)
# -----------------------------------------------------------------------------
agregar_series_anuales_missForest <- function(dir_rellenas = "output/output_seleccion_estaciones/series/rellenas",
                                              dir_anual = "output/output_seleccion_estaciones/series/anual",
                                              verbose = TRUE) {
  if (!dir.exists(dir_rellenas)) {
    stop("No existe la carpeta de series rellenas: ", dir_rellenas)
  }
  if (!dir.exists(dir_anual)) {
    dir.create(dir_anual, recursive = TRUE)
    if (verbose) message("Carpeta creada: ", dir_anual)
  }

  archivos <- list.files(dir_rellenas, pattern = "_rellena_missForest\\.csv$", full.names = TRUE)
  if (length(archivos) == 0) {
    message("No se encontraron archivos *_rellena_missForest.csv en ", dir_rellenas)
    return(invisible(list()))
  }

  if (verbose) message("Agregando ", length(archivos), " series a escala anual (missForest)...")

  resultados <- list()
  for (i in seq_along(archivos)) {
    resultados[[basename(archivos[i])]] <- procesar_una_serie_anual(
      archivos[i], dir_anual, verbose = verbose
    )
  }

  if (verbose) message("Series anuales guardadas en: ", dir_anual)
  invisible(resultados)
}

# -----------------------------------------------------------------------------
# Resumen de todo el periodo: una fila, una columna por estación.
# PP: para cada año, acumulado diario (suma); luego promedio de esos totales anuales (mm/año).
# Temperatura y caudal: promedio aritmético de los valores diarios.
# -----------------------------------------------------------------------------
agregar_periodo_total_una_fila <- function(df, variable = c("temp", "pp", "q")) {
  variable <- match.arg(variable)
  estaciones <- columnas_estaciones(df)
  if (length(estaciones) == 0) stop("No hay columnas de estaciones numéricas")

  if (variable == "pp") {
    agg <- df %>%
      group_by(year) %>%
      summarise(across(all_of(estaciones), ~ sum(., na.rm = TRUE)), .groups = "drop") %>%
      summarise(across(all_of(estaciones), ~ mean(., na.rm = TRUE)), .groups = "drop")
    estadistico <- "promedio_acumulado_anual_mm"
  } else {
    agg <- df %>%
      summarise(across(all_of(estaciones), ~ mean(., na.rm = TRUE)), .groups = "drop")
    estadistico <- "promedio_diario_periodo"
  }
  list(tabla = agg, estadistico = estadistico)
}

# -----------------------------------------------------------------------------
# Construye una fila (periodo completo × estaciones) a partir de un *_rellena_missForest.csv
# -----------------------------------------------------------------------------
procesar_un_archivo_promedio_historico <- function(archivo_entrada, verbose = TRUE) {
  bn <- basename(archivo_entrada)
  if (!grepl("_rellena_missForest\\.csv$", bn)) {
    if (verbose) message("  Omitido (no es *_rellena_missForest.csv): ", bn)
    return(NULL)
  }
  variable <- inferir_variable_archivo(archivo_entrada)
  if (is.na(variable)) {
    if (verbose) message("  Omitido (tipo de variable no reconocido): ", bn)
    return(NULL)
  }
  if (verbose) message("  Resumen periodo total: ", bn, " (", variable, ")")

  df <- cargar_serie_rellena(archivo_entrada)
  res <- agregar_periodo_total_una_fila(df, variable = variable)
  fecha_min <- min(df$fecha, na.rm = TRUE)
  fecha_max <- max(df$fecha, na.rm = TRUE)

  meta <- data.frame(
    archivo_fuente = bn,
    fecha_inicio_periodo = fecha_min,
    fecha_fin_periodo = fecha_max,
    variable = variable,
    estadistico = res$estadistico,
    stringsAsFactors = FALSE
  )
  dplyr::bind_cols(meta, res$tabla)
}

# Ruta de escritura con carpeta absoluta; en Windows intenta nombre corto 8.3 (MAX_PATH / OneDrive).
ruta_escritura_segura <- function(path_out) {
  ddir <- dirname(path_out)
  if (!dir.exists(ddir)) dir.create(ddir, recursive = TRUE)
  d_abs <- normalizePath(ddir, winslash = "/", mustWork = TRUE)
  dest_full <- file.path(d_abs, basename(path_out))
  if (.Platform$OS.type == "windows") {
    d_short <- tryCatch(utils::shortPathName(d_abs), error = function(e) NULL)
    if (!is.null(d_short) && nzchar(d_short)) {
      dest_full <- file.path(d_short, basename(path_out))
    }
  }
  dest_full
}

# readr::write_csv a rutas largas / OneDrive en Windows a veces falla; escribir en
# tempdir() y copiar.
write_csv_destino_seguro <- function(df, path_out) {
  dest_full <- ruta_escritura_segura(path_out)
  tmp <- tempfile(pattern = "phcsv_", tmpdir = tempdir(), fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(df, tmp)
  ok <- file.copy(from = tmp, to = dest_full, overwrite = TRUE)
  if (!ok || !file.exists(dest_full)) {
    stop("No se pudo escribir el CSV en: ", path_out)
  }
  invisible(dest_full)
}

# writexl a destino largo: generar en tempdir() y copiar.
write_xlsx_destino_seguro <- function(lista_hojas, path_out) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Instale writexl: install.packages(\"writexl\")")
  }
  dest_full <- ruta_escritura_segura(path_out)
  tmp <- tempfile(pattern = "phxlsx_", tmpdir = tempdir(), fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  writexl::write_xlsx(lista_hojas, tmp)
  ok <- file.copy(from = tmp, to = dest_full, overwrite = TRUE)
  if (!ok || !file.exists(dest_full)) {
    stop("No se pudo escribir el Excel en: ", path_out)
  }
  invisible(dest_full)
}

# -----------------------------------------------------------------------------
# Un CSV por cada *_rellena_missForest.csv (misma convención que _anual.csv:
# series_pp_el_teniente_70_1990_2024_historico.csv, etc.).
# Cada archivo: una fila + columnas de metadato + una columna por estación.
# Opcional: un Excel con una sola hoja y las mismas filas apiladas (todas las series).
# -----------------------------------------------------------------------------
exportar_promedio_historico_csv <- function(
  dir_rellenas = "output/output_seleccion_estaciones/series/rellenas",
  dir_salida = "output/output_seleccion_estaciones/series/historico",
  excel_consolidado = TRUE,
  nombre_excel_consolidado = "resumen_historico_consolidado.xlsx",
  nombre_hoja_consolidado = "todas_las_series",
  verbose = TRUE
) {
  if (!dir.exists(dir_rellenas)) {
    stop("No existe la carpeta de series rellenas: ", dir_rellenas)
  }
  if (!dir.exists(dir_salida)) {
    dir.create(dir_salida, recursive = TRUE)
    if (verbose) message("Carpeta creada: ", dir_salida)
  }

  archivos <- list.files(dir_rellenas, pattern = "_rellena_missForest\\.csv$", full.names = TRUE)
  if (length(archivos) == 0) {
    message("No se encontraron archivos *_rellena_missForest.csv en ", dir_rellenas)
    return(invisible(character(0)))
  }

  if (verbose) {
    message("Exportando resumen de periodo total: un CSV por serie → ", dir_salida)
  }

  rutas <- character(0)
  tablas_consolidar <- list()
  for (arch in archivos) {
    salida <- procesar_un_archivo_promedio_historico(arch, verbose = verbose)
    if (is.null(salida)) next
    bn <- basename(arch)
    nombre_out <- gsub("_rellena_missForest\\.csv$", "_historico.csv", bn)
    path_out <- file.path(dir_salida, nombre_out)
    write_csv_destino_seguro(salida, path_out)
    rutas <- c(rutas, path_out)
    tablas_consolidar[[length(tablas_consolidar) + 1L]] <- salida
    if (verbose) message("    -> ", nombre_out)
  }

  if (length(rutas) == 0) {
    message("No se escribió ningún CSV de promedio histórico.")
    return(invisible(character(0)))
  }

  if (verbose) message(length(rutas), " archivos CSV guardados.")

  if (excel_consolidado && length(tablas_consolidar) > 0) {
    consolidado <- dplyr::bind_rows(tablas_consolidar)
    path_xlsx <- file.path(dir_salida, nombre_excel_consolidado)
    nm_hoja <- nombre_hoja_consolidado
    if (nchar(nm_hoja) > 31) nm_hoja <- substr(nm_hoja, 1, 31)
    hojas <- list()
    hojas[[nm_hoja]] <- consolidado
    write_xlsx_destino_seguro(hojas, path_xlsx)
    if (verbose) {
      message(
        "Excel consolidado (", nrow(consolidado), " filas, 1 hoja): ",
        basename(path_xlsx)
      )
    }
    rutas <- c(rutas, path_xlsx)
  }

  invisible(rutas)
}

# -----------------------------------------------------------------------------
# Ruta del archivo mensual según variable y división
# variable: "temp", "pp", "q"
# division: "el_salvador" o "el_teniente"
# tipo_temp: "t_max" o "t_min" (solo si variable == "temp")
# -----------------------------------------------------------------------------
ruta_serie_mensual <- function(variable = c("temp", "pp", "q"),
                               division = c("el_teniente", "el_salvador"),
                               calidad = NULL,
                               ano_inicio = 1990, # valor canónico en MAIN.R; ajustar si se usa de forma independiente
                               ano_fin = 2024,    # valor canónico en MAIN.R; ajustar si se usa de forma independiente
                               tipo_temp = c("t_max", "t_min"),
                               dir_mensual = "output/output_seleccion_estaciones/series/mensual") {
  variable   <- match.arg(variable)
  division   <- match.arg(division)
  tipo_temp  <- match.arg(tipo_temp)
  div_archivo <- if (division == "el_teniente") "teniente" else "salvador"
  if (is.null(calidad)) {
    calidad <- switch(variable, temp = 70, pp = 60, q = 50)
  }
  if (variable == "pp") {
    base <- paste0("series_pp_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin)
  } else if (variable == "temp") {
    base <- paste0("series_", tipo_temp, "_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin)
  } else {
    base <- paste0("series_q_el_", div_archivo, "_", calidad, "_", ano_inicio, "_", ano_fin)
  }
  file.path(dir_mensual, paste0(base, "_mensual.csv"))
}

# -----------------------------------------------------------------------------
# Nombre de columna de la estación en el data frame (puede ser "X" + código)
# -----------------------------------------------------------------------------
nombre_columna_estacion <- function(df, estacion) {
  est <- as.character(estacion)
  if (est %in% names(df)) return(est)
  xest <- paste0("X", est)
  if (xest %in% names(df)) return(xest)
  NULL
}

# -----------------------------------------------------------------------------
# Graficar serie mensual para una estación
# -----------------------------------------------------------------------------
graficar_serie_mensual <- function(estacion,
                                   variable = c("temp", "pp", "q"),
                                   division = c("el_teniente", "el_salvador"),
                                   dir_mensual = "output/output_seleccion_estaciones/series/mensual",
                                   calidad = NULL,
                                   ano_inicio = 1990, # valor canónico en MAIN.R; ajustar si se usa de forma independiente
                                   ano_fin = 2024,    # valor canónico en MAIN.R; ajustar si se usa de forma independiente
                                   tipo_temp = c("t_max", "t_min"),
                                   titulo = NULL) {
  variable  <- match.arg(variable)
  division  <- match.arg(division)
  tipo_temp <- match.arg(tipo_temp)

  archivo <- ruta_serie_mensual(
    variable = variable, division = division, calidad = calidad,
    ano_inicio = ano_inicio, ano_fin = ano_fin, tipo_temp = tipo_temp, dir_mensual = dir_mensual
  )
  if (!file.exists(archivo)) {
    stop("No existe el archivo de serie mensual: ", archivo)
  }

  df <- read_csv(archivo, show_col_types = FALSE)
  df <- as.data.frame(df)
  if (!"fecha" %in% names(df)) {
    if ("Fecha" %in% names(df)) names(df)[names(df) == "Fecha"] <- "fecha"
    else stop("El archivo debe tener columna 'fecha' o 'Fecha'.")
  }
  df$fecha <- as.Date(df$fecha)

  col_est <- nombre_columna_estacion(df, estacion)
  if (is.null(col_est)) {
    stop("Estación '", estacion, "' no encontrada en el archivo. Columnas de estaciones: ",
         paste(setdiff(names(df), c("fecha", "year", "month")), collapse = ", "))
  }

  if (variable == "temp") {
    unidad <- if (tipo_temp == "t_max") "T máx (°C)" else "T mín (°C)"
  } else if (variable == "pp") {
    unidad <- "Precipitación (mm)"
  } else {
    unidad <- "Caudal"
  }

  if (is.null(titulo)) {
    div_label <- if (division == "el_teniente") "El Teniente" else "El Salvador"
    titulo <- paste0("Serie mensual ", variable, " - Estación ", estacion, " (", div_label, ")")
  }

  p <- ggplot(df, aes(x = fecha, y = .data[[col_est]])) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    labs(title = titulo, x = "Fecha", y = unidad) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  p
}

# -----------------------------------------------------------------------------
# Ejemplo de uso (setwd en caracterizacion_historica)
# -----------------------------------------------------------------------------
#
# source("scripts/MAIN/4.agregar_series_mensuales_missForest.R")
#
# agregar_series_mensuales_missForest(
#   dir_rellenas = "output/output_seleccion_estaciones/series/rellenas",
#   dir_mensual  = "output/output_seleccion_estaciones/series/mensual"
# )
#
# graficar_serie_mensual("330088", variable = "temp", division = "el_teniente")
# graficar_serie_mensual("05110002-6", variable = "pp", division = "el_teniente")
# graficar_serie_mensual("02103014-7", variable = "q", division = "el_salvador")
