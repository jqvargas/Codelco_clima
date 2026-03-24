# ============================================================================
# Script para generar gráficos de distribución de calidad de estaciones
# Autor: Claude
# Fecha: 2026-02-13
# ============================================================================

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(readr)

# ============================================================================
# FUNCIÓN PRINCIPAL PARA GENERAR EL GRÁFICO
# ============================================================================

generar_grafico_calidad <- function(archivo_csv,
                                    columna_calidad,
                                    titulo_variable,
                                    archivo_salida,
                                    color_principal = "#2E86AB",
                                    periodo_subtitulo = NULL) {
  
  # Leer el archivo CSV
  df <- read_csv(archivo_csv, show_col_types = FALSE)
  
  # Verificar que la columna existe
  if (!columna_calidad %in% colnames(df)) {
    stop(paste("La columna", columna_calidad, "no existe en el archivo"))
  }
  
  # Estadísticas básicas
  cat("\n============================================================\n")
  cat(paste("Archivo:", archivo_csv, "\n"))
  cat("============================================================\n")
  cat(paste("Total de estaciones:", nrow(df), "\n"))
  cat("\nEstadísticas de calidad:\n")
  print(summary(df[[columna_calidad]]))
  
  # Crear rango de umbrales de calidad
  umbrales <- seq(0, 100, by = 1)
  
  # Calcular cuántas estaciones cumplen con cada umbral
  estaciones_por_umbral <- sapply(umbrales, function(umbral) {
    sum(df[[columna_calidad]] >= umbral, na.rm = TRUE)
  })
  
  # Crear data frame para el gráfico
  df_plot <- data.frame(
    umbral = umbrales,
    n_estaciones = estaciones_por_umbral
  )
  
  # Umbrales de referencia
  umbrales_ref <- c(50, 60, 70, 80, 90, 95)
  colores_ref <- c("#E63946", "#F77F00", "#06A77D", "#4361EE", "#7209B7", "#560BAD")
  
  # Calcular estaciones para cada umbral de referencia
  df_umbrales_ref <- data.frame(
    umbral = umbrales_ref,
    color = colores_ref,
    n_est = sapply(umbrales_ref, function(u) sum(df[[columna_calidad]] >= u, na.rm = TRUE))
  )
  
  # Crear el gráfico
  p <- ggplot(df_plot, aes(x = umbral, y = n_estaciones)) +
    # Área rellena
    geom_area(fill = color_principal, alpha = 0.3) +
    # Línea principal
    geom_line(color = color_principal, linewidth = 1.2) +
    # Líneas verticales de referencia
    geom_vline(data = df_umbrales_ref, 
               aes(xintercept = umbral, color = color), 
               linetype = "dashed", 
               alpha = 0.7, 
               linewidth = 0.8,
               show.legend = FALSE) +
    # Líneas horizontales de referencia
    geom_hline(data = df_umbrales_ref, 
               aes(yintercept = n_est, color = color), 
               linetype = "dashed", 
               alpha = 0.5, 
               linewidth = 0.5,
               show.legend = FALSE) +
    # Etiquetas de umbrales
    geom_label(data = df_umbrales_ref,
               aes(x = umbral, y = max(df_plot$n_estaciones) * 0.98, 
                   label = paste0(umbral, "%\n(", n_est, " est.)"),
                   fill = color),
               color = "white",
               fontface = "bold",
               size = 3,
               show.legend = FALSE) +
    # Escalas de color personalizadas
    scale_color_identity() +
    scale_fill_identity() +
    # Etiquetas de ejes y título
    labs(
      x = "Umbral de Calidad (%)",
      y = "Número de Estaciones",
      title = paste0("Distribución Acumulada de Estaciones por Umbral de Calidad"),
      subtitle = paste0(titulo_variable, if (!is.null(periodo_subtitulo)) paste0(" (", periodo_subtitulo, ")") else " (1990-2025)")
    ) +
    # Tema y personalización
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    # Límites de ejes
    coord_cartesian(xlim = c(0, 100), ylim = c(0, max(estaciones_por_umbral) * 1.05)) +
    # Anotación con información adicional
    annotate("label", 
             x = 5, 
             y = max(estaciones_por_umbral) * 0.95,
             label = paste0("Total: ", nrow(df), " estaciones\n",
                           "Calidad media: ", round(mean(df[[columna_calidad]], na.rm = TRUE), 1), "%"),
             hjust = 0,
             vjust = 1,
             fill = "white",
             alpha = 0.8,
             size = 3.5)
  
  # Crear directorio de salida si no existe
  dir_salida <- dirname(archivo_salida)
  if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)
  # Guardar el gráfico
  ggsave(archivo_salida, 
         plot = p, 
         width = 12, 
         height = 7, 
         dpi = 300,
         bg = "white")
  
  cat("\n✓ Gráfico guardado en:", archivo_salida, "\n")
  
  # Imprimir tabla resumen
  cat("\n============================================================\n")
  cat("TABLA RESUMEN: Estaciones por Umbral de Calidad\n")
  cat("============================================================\n")
  cat(sprintf("%-12s %-20s %-15s\n", "Umbral", "N° Estaciones", "% del Total"))
  cat("------------------------------------------------------------\n")
  
  umbrales_tabla <- c(0, 50, 60, 70, 75, 80, 85, 90, 95, 98, 99)
  for (umbral in umbrales_tabla) {
    n_est <- sum(df[[columna_calidad]] >= umbral, na.rm = TRUE)
    porcentaje <- (n_est / nrow(df)) * 100
    cat(sprintf("≥ %3d%%      %3d estaciones       %5.1f%%\n", umbral, n_est, porcentaje))
  }
  cat("============================================================\n")
  
  # Ejemplos de estaciones por rango
  cat("\n============================================================\n")
  cat("EJEMPLO DE ESTACIONES EN DIFERENTES RANGOS\n")
  cat("============================================================\n")
  
  rangos <- list(
    c(99, 100.1),
    c(95, 99),
    c(90, 95),
    c(80, 90),
    c(50, 80)
  )
  
  for (rango in rangos) {
    estaciones_rango <- df %>%
      filter(!!sym(columna_calidad) >= rango[1] & !!sym(columna_calidad) < rango[2])
    
    cat(sprintf("\nCalidad %.0f%% - %.0f%%: %d estaciones\n", 
                rango[1], rango[2], nrow(estaciones_rango)))
    
    if (nrow(estaciones_rango) > 0) {
      cat(sprintf("  Ejemplo: %s (%.2f%%)\n", 
                  estaciones_rango$nombre[1], 
                  estaciones_rango[[columna_calidad]][1]))
    }
  }
  
  return(p)
}

# ============================================================================
# FUNCIÓN: Generar gráficos de calidad para todos los CSV con calidad_0
# ============================================================================
# Lee los archivos estaciones_*_0_*_*.csv en dir_estaciones, genera un gráfico
# por cada uno y los guarda en dir_graficos (El Teniente / El Salvador; pp, temp, q).

generar_graficos_calidad_estaciones_0 <- function(dir_estaciones,
                                                 dir_graficos,
                                                 ano_inicio = NULL,
                                                 ano_fin = NULL) {

  # Patrón: estaciones_{variable}_{division}_0_{ano_inicio}_{ano_fin}.csv
  archivos <- list.files(dir_estaciones, pattern = "estaciones_.*_0_.*\\.csv$", full.names = TRUE)

  if (length(archivos) == 0) {
    message("No se encontraron archivos estaciones_*_0_*_*.csv en: ", dir_estaciones)
    return(invisible(NULL))
  }

  # Filtrar por año si se especifica
  if (!is.null(ano_inicio) && !is.null(ano_fin)) {
    patron_ano <- paste0("_", ano_inicio, "_", ano_fin, "\\.csv$")
    archivos <- archivos[grepl(patron_ano, archivos, ignore.case = TRUE)]
  }

  if (length(archivos) == 0) {
    message("No hay archivos de calidad_0 para el periodo ", ano_inicio, "-", ano_fin)
    return(invisible(NULL))
  }

  if (!dir.exists(dir_graficos)) {
    dir.create(dir_graficos, recursive = TRUE)
    message("Carpeta creada: ", dir_graficos)
  }

  # Configuración por variable: columna de calidad, colores
  config <- list(
    pp = list(columna = "calidad_pp", color_teniente = "#2E86AB", color_salvador = "#06A77D"),
    temp = list(columna = "calidad_t_max", color_teniente = "#7209B7", color_salvador = "#F77F00"),
    caudal = list(columna = "calidad_q", color_teniente = "#2E86AB", color_salvador = "#D62828")
  )

  # Nombres amigables para títulos
  nombres_division <- c(el_teniente = "El Teniente", el_salvador = "El Salvador")
  nombres_variable <- c(pp = "Precipitación", temp = "Temperatura", caudal = "Caudal")

  resultados <- list()
  for (ruta in archivos) {
    nombre <- basename(ruta)
    # Parsear: estaciones_pp_el_teniente_0_1990_2024.csv
    partes <- strsplit(sub("\\.csv$", "", nombre, ignore.case = TRUE), "_")[[1]]
    if (length(partes) < 7) next
    variable <- partes[2]
    division <- paste(partes[3], partes[4], sep = "_")  # "el_teniente" o "el_salvador"
    ano_ini <- partes[6]
    ano_fin <- partes[7]  # variable local: año final extraído del nombre del archivo CSV; NO sobreescribe el global ano_fin
    periodo_sub <- paste(ano_ini, ano_fin, sep = "-")
    if (!variable %in% names(config)) next

    columna_calidad <- config[[variable]]$columna
    titulo <- paste(nombres_division[division], "-", nombres_variable[variable])
    color <- if (division == "el_teniente") config[[variable]]$color_teniente else config[[variable]]$color_salvador
    nombre_salida <- paste0("distribucion_calidad_", division, "_", variable, ".png")
    archivo_salida <- file.path(dir_graficos, nombre_salida)

    cat("\n\n████████████████████████████████████████████████████████████\n")
    cat("PROCESANDO:", toupper(titulo), "\n")
    cat("████████████████████████████████████████████████████████████\n")

    tryCatch({
      p <- generar_grafico_calidad(
        archivo_csv = ruta,
        columna_calidad = columna_calidad,
        titulo_variable = titulo,
        archivo_salida = archivo_salida,
        color_principal = color,
        periodo_subtitulo = periodo_sub
      )
      resultados[[nombre_salida]] <- p
    }, error = function(e) {
      message("Error al procesar ", nombre, ": ", conditionMessage(e))
    })
  }

  cat("\n\n════════════════════════════════════════════════════════════\n")
  cat("✓ Gráficos de calidad (calidad_0) guardados en:", dir_graficos, "\n")
  cat("════════════════════════════════════════════════════════════\n")

  invisible(resultados)
}

# ============================================================================
# INSTRUCCIONES DE USO
# ============================================================================

# Desde MAIN.R (o con setwd en caracterizacion_historica):
#   source("scripts/MAIN/0.analisis_calidad_estaciones.R")
#   generar_graficos_calidad_estaciones_0(
#     dir_estaciones = "output/output_seleccion_estaciones/estaciones",
#     dir_graficos   = "output/output_seleccion_estaciones/graficos_calidad"
#   )

# Gráfico individual:
# p <- generar_grafico_calidad(
#   archivo_csv = "ruta/estaciones_pp_el_salvador_0_1990_2024.csv",
#   columna_calidad = "calidad_pp",
#   titulo_variable = "El Salvador - Precipitación",
#   archivo_salida = "distribucion_calidad_el_salvador_pp.png",
#   color_principal = "#06A77D"
# )

# ============================================================================
# NOTAS
# ============================================================================
# - Librerías: ggplot2, dplyr, readr
# - Solo se procesan archivos con patrón estaciones_*_0_*_*.csv (calidad_0)
