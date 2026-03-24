library(tidyverse)

tendencias <- read.csv("output/tendencias/EFE_Tendencias_decadales.csv")
tendencias <- tendencias %>% arrange(pend_decada)

#Estación Las Trancas tiene un -600 mm /década de precipitación, eso está raro. 08130004-6

series_brutas_pp <- read.csv("output/output_seleccion_estaciones/series/brutas/series_pp_1992_2021_bruta_EFE.csv")
series_rellenas_pp <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_pp_1992_2021_rellena_100_EFE.csv")
series_brutas_pp <- series_brutas_pp %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = 'pp'
    )
head(series_brutas_pp)
series_las_trancas <- series_brutas_pp %>% filter (codigo_nacional == 'X08130004.6' )
head(series_las_trancas)
series_las_trancas <- series_las_trancas %>%
  mutate(fecha = as.Date(fecha))

series_las_trancas_anual <- series_las_trancas %>% 
    group_by(year)%>%
        summarise(pp_year = sum(pp, na.rm = TRUE)) %>%
        ungroup()

series_las_trancas_anual
p <- ggplot(series_las_trancas_anual, aes(x = year, y = pp_year)) + 
    geom_col(fill = 'lightblue', color = "blue")+ 
    geom_smooth(method = "lm", se = FALSE, color = 'red') +
    labs(title = "Precipitación anual - Las Trancas - Serie Bruta",
        x = 'Año', y = 'Precipitación total (mm)')+
    ylim(0, 3100)+
    theme_classic()

ggsave( 'grafico_pp_anual_las_trancas.png',plot = p)




series_rellenas_pp <- series_rellenas_pp %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = 'pp'
    )
head(series_rellenas_pp)
series_las_trancas_rellenas <- series_rellenas_pp %>% filter (codigo_nacional == 'X08130004.6' )
head(series_las_trancas_rellenas)
series_las_trancas_rellenas$
series_las_trancas_rellenas <- series_las_trancas_rellenas %>%
  mutate(fecha = as.Date(fecha))

series_las_trancas_rellenas_anual <- series_las_trancas_rellenas %>% 
    group_by(Year)%>%
        summarise(pp_year = sum(pp, na.rm = TRUE)) %>%
        ungroup()

p <- ggplot(series_las_trancas_rellenas_anual, aes(x = Year, y = pp_year)) + 
    geom_col(fill = 'lightblue', color = "blue")+ 
    geom_smooth(method = "lm", se = FALSE, color = 'red') +
    labs(title = "Precipitación anual - Las Trancas - Serie Rellena",
        x = 'Año', y = 'Precipitación total (mm)')+
    ylim(0, 3100)+
    theme_classic()

ggsave( 'grafico_pp_anual_las_trancas_rellenas.png',plot = p)


#En precipitación, Serie rellena se ve casi igual a la bruta. Al parecer, dado los datos descargados que tenemos, la precipitacion muy influcenciada por la Mega Sequía. 
#Dado que en periodo 2022 la precipitación vuelve a aumentar. Quizás no nos conviene usar periodos históricos hasta 2020? -> urgente completar


#2. Temperatura mínima
# Laguna Malleco tiene pendiente de -1.0676 °C por década, con p_valor de 0.0005. X08350002.6
series_brutas_t_min <- read.csv("output/output_seleccion_estaciones/series/brutas/series_t_min_1992_2021_bruta_EFE.csv")
series_rellenas_t_min <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_t_min_1992_2021_rellena_100_EFE.csv")
series_brutas_t_min <- series_brutas_t_min %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = 't_min'
    )
series_rellenas_t_min <- series_rellenas_t_min %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = 't_min'
    )

series_malleco <- series_brutas_t_min %>% filter (codigo_nacional == 'X08350002.6')
series_malleco_mensual <- series_malleco %>% 
    group_by(year, month)%>%
        summarise(t_min = mean(t_min, na.rm = TRUE)) %>%
        ungroup()

# Crear columna tipo Date al primer día de cada mes
series_malleco_mensual <- series_malleco_mensual %>%
  mutate(fecha = as.Date(paste(year, month, "01", sep = "-")))

p <- ggplot(series_malleco_mensual, aes(x = fecha, y = t_min)) + 
    geom_line(color = "blue")+ 
    geom_smooth(method = "lm", se = FALSE, color = 'red') +
    labs(title = "Temperatura mínima mensual- Laguna Malleco - Serie Bruta",
        x = 'Año', y = '°C')+
    ylim(-10,10) +
    theme_classic()



ggsave( 'grafico_t_min_mensual_laguna_malleco.png',plot = p)

series_malleco_rellenas <- series_rellenas_t_min %>% filter (codigo_nacional == 'X08350002.6')
series_malleco_rellenas_mensual <- series_malleco_rellenas %>% 
    group_by(Year, Month)%>%
        summarise(t_min = mean(t_min, na.rm = TRUE)) %>%
        ungroup()

# Crear columna tipo Date al primer día de cada mes
series_malleco_rellenas_mensual <- series_malleco_rellenas_mensual %>%
  mutate(fecha = as.Date(paste(Year, Month, "01", sep = "-")))

p <- ggplot(series_malleco_rellenas_mensual, aes(x = fecha, y = t_min)) + 
    geom_line(color = "blue")+ 
    geom_smooth(method = "lm", se = FALSE, color = 'red') +
    labs(title = "Temperatura mínima mensual- Laguna Malleco - Serie Rellena",
        x = 'Año', y = '°C')+ 
    ylim(-10,10) +
    theme_classic()




ggsave( 'grafico_t_min_mensual_laguna_malleco_rellena.png',plot = p)

