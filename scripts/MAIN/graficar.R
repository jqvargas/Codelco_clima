# Script para generar gráficos de tendencias decadales (Salvador y Teniente)
# Genera gráficos de resultados finales y los guarda en output/tendencias_historica/graficos

library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# Función única: graficar resultados finales de tendencias para Salvador y Teniente
# -----------------------------------------------------------------------------
graficar_tendencias_finales <- function(dir_tendencias = file.path("output", "tendencias_historica"),
                                        dir_graficos = NULL,
                                        verbose = TRUE) {
  if (is.null(dir_graficos)) {
    dir_graficos <- file.path(dir_tendencias, "graficos")
  }
  if (!dir.exists(dir_graficos)) {
    dir.create(dir_graficos, recursive = TRUE)
    if (verbose) message("Carpeta creada: ", dir_graficos)
  }

  ruta_salvador <- file.path(dir_tendencias, "salvador_Tendencias_decadales.csv")
  ruta_teniente <- file.path(dir_tendencias, "teniente_Tendencias_decadales.csv")

  if (!file.exists(ruta_salvador)) stop("No se encontró: ", ruta_salvador)
  if (!file.exists(ruta_teniente)) stop("No se encontró: ", ruta_teniente)

  salvador <- read.csv(ruta_salvador, stringsAsFactors = FALSE)
  teniente <- read.csv(ruta_teniente, stringsAsFactors = FALSE)

  salvador$division <- "Salvador"
  teniente$division <- "Teniente"
  datos <- rbind(salvador, teniente)

  # Normalizar nombres de variable (Tn/Tx o t_min/t_max -> tmin/tmax para incluir temperatura)
  datos <- datos %>%
    mutate(var_norm = case_when(
      var %in% c("Tn", "t_min", "tmin") ~ "tmin",
      var %in% c("Tx", "t_max", "tmax") ~ "tmax",
      TRUE ~ as.character(var)
    ))

  datos_anuales <- datos %>%
    filter(mes == "Anual") %>%
    filter(var_norm %in% c("pp", "tmin", "tmax", "q"))

  if (nrow(datos_anuales) == 0) {
    warning("No hay filas con mes=='Anual' y var en pp/tmin/tmax/q. Revisar archivos CSV.")
    return(invisible(NULL))
  }

  datos_anuales <- datos_anuales %>%
    mutate(
      variable = case_when(
        var_norm == "pp"   ~ "Precipitación\n(mm/década)",
        var_norm == "tmin" ~ "Temperatura Mínima\n(°C/década)",
        var_norm == "tmax" ~ "Temperatura Máxima\n(°C/década)",
        var_norm == "q"    ~ "Caudal\n(m³/s/década)",
        TRUE ~ as.character(var_norm)
      ),
      significativo = ifelse(p_valor < 0.05, "Significativo (p<0.05)", "No significativo")
    )

  resumen <- datos_anuales %>%
    group_by(division, variable) %>%
    summarise(
      tendencia_promedio = mean(pend_decada, na.rm = TRUE),
      tendencia_mediana = median(pend_decada, na.rm = TRUE),
      n_estaciones = n(),
      n_significativas = sum(p_valor < 0.05, na.rm = TRUE),
      .groups = "drop"
    )

  if (verbose) {
    message("\nResumen de tendencias por división y variable:")
    print(resumen)
  }

  # ---------- Gráfico 1: Tendencias decadales por división (violin + box + puntos) ----------
  p1 <- ggplot(datos_anuales, aes(x = division, y = pend_decada, fill = division)) +
    geom_violin(alpha = 0.4, trim = FALSE, scale = "width") +
    geom_boxplot(width = 0.15, alpha = 0.3, outlier.shape = NA,
                 position = position_dodge(width = 0)) +
    geom_jitter(aes(color = significativo, shape = significativo),
                width = 0.1, size = 2.5, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.7) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = c("Salvador" = "#E69F00", "Teniente" = "#56B4E9")) +
    scale_color_manual(values = c("Significativo (p<0.05)" = "#D55E00", "No significativo" = "gray60")) +
    scale_shape_manual(values = c("Significativo (p<0.05)" = 16, "No significativo" = 1)) +
    labs(
      title = "Tendencias Decadales por División",
      subtitle = "Valores anuales - Cada punto representa una estación de monitoreo",
      x = "División", y = "Tendencia (cambio por década)",
      fill = "División", color = "Significancia estadística", shape = "Significancia estadística"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "gray95", color = NA),
      legend.position = "bottom", legend.box = "vertical",
      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.spacing = unit(1, "lines")
    ) +
    guides(
      fill = guide_legend(order = 1, nrow = 1),
      color = guide_legend(order = 2, nrow = 1, override.aes = list(size = 3)),
      shape = guide_legend(order = 2, nrow = 1)
    )

  f1 <- file.path(dir_graficos, "tendencias_decadales.png")
  ggsave(f1, p1, width = 12, height = 10, dpi = 300, bg = "white")
  if (verbose) message("Gráfico 1 guardado: ", f1)

  # ---------- Gráfico 2: Tendencia promedio por división y variable (barras) ----------
  resumen_plot <- resumen %>%
    mutate(
      variable_simple = gsub("\n.*", "", variable),
      label_text = sprintf("n=%d\n(%d sig.)", n_estaciones, n_significativas)
    )

  p2 <- ggplot(resumen_plot, aes(x = variable_simple, y = tendencia_promedio, fill = division)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7,
             color = "black", linewidth = 0.3) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.5) +
    geom_text(aes(label = label_text,
                  y = ifelse(tendencia_promedio >= 0,
                             tendencia_promedio + max(abs(tendencia_promedio)) * 0.05,
                             tendencia_promedio - max(abs(tendencia_promedio)) * 0.05)),
              position = position_dodge(width = 0.8),
              vjust = ifelse(resumen_plot$tendencia_promedio >= 0, -0.2, 1.2),
              size = 3, color = "gray20") +
    scale_fill_manual(values = c("Salvador" = "#E69F00", "Teniente" = "#56B4E9")) +
    labs(
      title = "Tendencia Promedio por División y Variable",
      subtitle = "Barras: promedio de todas las estaciones | n = estaciones | sig. = significativas (p<0.05)",
      x = "Variable", y = "Tendencia promedio (cambio por década)", fill = "División"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
      legend.position = "bottom", panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.key.size = unit(1, "cm")
    )

  f2 <- file.path(dir_graficos, "tendencias_promedio.png")
  ggsave(f2, p2, width = 10, height = 7, dpi = 300, bg = "white")
  if (verbose) message("Gráfico 2 guardado: ", f2)

  # ---------- Resumen en consola ----------
  if (verbose) {
    message("\n============================================================")
    message("RESUMEN DE ANÁLISIS")
    message("============================================================\n")
    for (div in unique(resumen$division)) {
      message("DIVISIÓN: ", div)
      message("------------------------------------------------------------")
      resumen_div <- resumen %>% filter(division == div)
      for (i in seq_len(nrow(resumen_div))) {
        message("  ", gsub("\n", " ", resumen_div$variable[i]), ":")
        message("    - Tendencia promedio: ", round(resumen_div$tendencia_promedio[i], 2))
        message("    - Estaciones analizadas: ", resumen_div$n_estaciones[i])
        message("    - Estaciones significativas: ", resumen_div$n_significativas[i], " (",
                round(100 * resumen_div$n_significativas[i] / resumen_div$n_estaciones[i], 1), "%)")
        message("")
      }
      message("")
    }
    message("============================================================")
    message("Gráficos guardados en: ", dir_graficos)
    message("============================================================\n")
  }

  invisible(list(resumen = resumen, p1 = p1, p2 = p2, dir_graficos = dir_graficos))
}

# -----------------------------------------------------------------------------
# Ejecución al correr el script
# Nota: las rutas por defecto son relativas al directorio de trabajo (setwd).
# Conviene fijar setwd() en la raíz del proyecto (caracterizacion_historica).
# Para solo definir la función sin generar gráficos, comenta el bloque siguiente.
# -----------------------------------------------------------------------------
if (identical(parent.frame(), globalenv())) {
  graficar_tendencias_finales(
    dir_tendencias = file.path("output", "tendencias_historica"),
    dir_graficos   = file.path("output", "tendencias_historica", "graficos"),
    verbose       = TRUE
  )
}
