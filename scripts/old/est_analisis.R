# Este script es para calcular los promedios históricos de cada variable. de PP, t_min, t_max, q_medio (4). 
# En el informe vamos a presentar: 
# 1. estaciones de analisis: cual es la magnitud actual de cada variable en el periodo histórico (4 mapas).
# 3. tendencias indicadores extremos: cuales son las tendencias de los indicadores extremos en el periodo histórico, es decir, como han cambiado. (6 mapas. Uno por indicador extremo. Entrego todo lo otro en un anexo digital. Lo mismo para las estaciones. Yo lo armo)
# 2. tendencias de las variables: como han cambiando las variables en los últimos años. 
# este codigo es para resolver 1, dado que 2 y 3 ya está resuelto (4 mapas).

source("backend/librerias.R")

series_pp <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_pp_1992_2021_rellena_100_EFE.csv")
estaciones_pp <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_pp_EFE.csv", sep = ";")
estaciones_pp$lat <- gsub(",", ".", estaciones_pp$lat)
estaciones_pp$lat <- as.numeric(estaciones_pp$lat)
estaciones_pp$lon <- gsub(",", ".", estaciones_pp$lon)
estaciones_pp$lon <- as.numeric(estaciones_pp$lon)

series_pp <- series_pp %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = sprintf("pp_day")
    )
series_pp$codigo_nacional <- gsub("X", "", series_pp$codigo_nacional)
series_pp$codigo_nacional <- gsub("\\.", "-", series_pp$codigo_nacional)

pp_anual <- series_pp %>%
    group_by(Year, codigo_nacional) %>%
    summarise(pp_year = sum(pp_day)) %>% 
    ungroup()

pp_promedio <- pp_anual %>%
    group_by(codigo_nacional) %>%
    summarise(pp_promedio = mean(pp_year)) %>%
    ungroup()

pp_promedio <- pp_promedio %>% left_join(estaciones_pp, by = c("codigo_nacional")) %>% select(nombre, lat, lon, codigo_nacional,altura, pp_promedio)
write.csv(pp_promedio, "output/tendencias/promedios/pp_promedio_historico_1992_2021_EFE.csv", row.names = FALSE)





#tmax
series_t_max <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_t_max_1992_2021_rellena_100_EFE.csv")
estaciones_t_max <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_temp_EFE.csv", sep = ";")
estaciones_t_max$lat <- gsub(",", ".", estaciones_t_max$lat)
estaciones_t_max$lat <- as.numeric(estaciones_t_max$lat)
estaciones_t_max$lon <- gsub(",", ".", estaciones_t_max$lon)
estaciones_t_max$lon <- as.numeric(estaciones_t_max$lon)

series_t_max <- series_t_max %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = sprintf("t_max_day")
    )
series_t_max$codigo_nacional <- gsub("X", "", series_t_max$codigo_nacional)
series_t_max$codigo_nacional <- gsub("\\.", "-", series_t_max$codigo_nacional)

t_max_anual <- series_t_max %>%
    group_by(Year, codigo_nacional) %>%
    summarise(t_max_year = mean(t_max_day)) %>% 
    ungroup()

t_max_promedio <- t_max_anual %>%
    group_by(codigo_nacional) %>%
    summarise(t_max_promedio = mean(t_max_year)) %>%
    ungroup()

t_max_promedio <- t_max_promedio %>% left_join(estaciones_t_max, by = c("codigo_nacional")) %>% select(nombre, lat, lon, codigo_nacional,altura, t_max_promedio)
write.csv(t_max_promedio, "output/tendencias/promedios/t_max_promedio_historico_1992_2021_EFE.csv", row.names = FALSE)



#t_min
series_t_min <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_t_min_1992_2021_rellena_100_EFE.csv")
estaciones_t_min <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_temp_EFE.csv", sep = ";")
estaciones_t_min$lat <- gsub(",", ".", estaciones_t_min$lat)
estaciones_t_min$lat <- as.numeric(estaciones_t_min$lat)
estaciones_t_min$lon <- gsub(",", ".", estaciones_t_min$lon)
estaciones_t_min$lon <- as.numeric(estaciones_t_min$lon)

series_t_min <- series_t_min %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = sprintf("t_min_day")
    )
series_t_min$codigo_nacional <- gsub("X", "", series_t_min$codigo_nacional)
series_t_min$codigo_nacional <- gsub("\\.", "-", series_t_min$codigo_nacional)

t_min_anual <- series_t_min %>%
    group_by(Year, codigo_nacional) %>%
    summarise(t_min_year = mean(t_min_day)) %>% 
    ungroup()

t_min_promedio <- t_min_anual %>%
    group_by(codigo_nacional) %>%
    summarise(t_min_promedio = mean(t_min_year)) %>%
    ungroup()

t_min_promedio <- t_min_promedio %>% left_join(estaciones_t_min, by = c("codigo_nacional")) %>% select(nombre, lat, lon, codigo_nacional,altura, t_min_promedio)
write.csv(t_min_promedio, "output/tendencias/promedios/t_min_promedio_historico_1992_2021_EFE.csv", row.names = FALSE)

temp_promedio <- t_max_promedio %>% left_join(t_min_promedio, by = c("nombre", "lat", "lon", "codigo_nacional", "altura"))
write.csv(temp_promedio, "output/tendencias/promedios/temp_promedio_historico_1992_2021_EFE.csv", row.names = FALSE)


#q
series_q <- read.csv("output/output_seleccion_estaciones/series/rellenas/series_q_1992_2021_rellena_100_EFE.csv")
estaciones_q <- read.csv("output/output_seleccion_estaciones/estaciones/estaciones_caudal_EFE.csv", sep = ";")
estaciones_q$lat <- gsub(",", ".", estaciones_q$lat)
estaciones_q$lat <- as.numeric(estaciones_q$lat)
estaciones_q$lon <- gsub(",", ".", estaciones_q$lon)
estaciones_q$lon <- as.numeric(estaciones_q$lon)

series_q <- series_q %>%
    pivot_longer(
        cols = starts_with("X"),
        names_to = "codigo_nacional",
        values_to = sprintf("q_day")
    )
series_q$codigo_nacional <- gsub("X", "", series_q$codigo_nacional)
series_q$codigo_nacional <- gsub("\\.", "-", series_q$codigo_nacional)

q_anual <- series_q %>%
    group_by(Year, codigo_nacional) %>%
    summarise(q_year = mean(q_day)) %>% 
    ungroup()

q_promedio <- q_anual %>%
    group_by(codigo_nacional) %>%
    summarise(q_promedio = mean(q_year)) %>%
    ungroup()

q_promedio <- q_promedio %>% left_join(estaciones_q, by = c("codigo_nacional")) %>% select(nombre, lat, lon, codigo_nacional,altura, q_promedio)
write.csv(q_promedio, "output/tendencias/promedios/q_promedio_historico_1992_2021_EFE.csv", row.names = FALSE)
