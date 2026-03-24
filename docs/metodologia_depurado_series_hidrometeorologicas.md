# Metodología de depuración de series hidrometeorológicas

## Objetivo

La depuración de series tiene por objeto **identificar y marcar como faltantes (NA)** aquellos valores que se consideran outliers o errores groseros en las series diarias de precipitación, temperatura (máxima y mínima) y caudal, **antes** de aplicar cualquier método de rellenado o imputación. De este modo se evita que valores anómalos (por error de medición, digitación o transmisión) contaminen las series rellenadas y los análisis posteriores.

La depuración se aplica por **estación de monitoreo** y por **variable**, de forma automática sobre las series brutas descargadas, y genera series depuradas que son la entrada al paso de rellenado.

---

## Criterios de depuración

Se utilizan tres criterios en secuencia. Todo valor que cumpla al menos uno de ellos se reemplaza por NA. Los criterios son los siguientes.

### 1. Límites físicos o de rango posible

Se eliminan valores que están fuera del rango físicamente plausible para cada variable:

- **Precipitación (mm/día):** se rechazan valores negativos y valores superiores a un límite superior configurable (por defecto 150 mm/día). En zonas áridas o semiáridas, valores diarios muy por encima de la precipitación media anual son altamente sospechosos; el límite superior puede ajustarse según la región (por ejemplo 150 mm/día para el norte árido de Chile).
- **Temperatura máxima y mínima (°C):** se rechazan valores fuera del intervalo [-50, 55] °C, coherente con rangos observados a nivel mundial.
- **Caudal (m³/s o unidades de la serie):** se rechazan valores negativos.

Este criterio permite eliminar errores evidentes (negativos en PP o caudal, temperaturas imposibles) y valores de precipitación diaria extremadamente altos que en el contexto regional se consideran implausibles.

### 2. Método del rango intercuartílico (IQR) y fallback para IQR nulo

Para cada estación (columna) se calculan el primer y tercer cuartil (Q1 y Q3) y el rango intercuartílico IQR = Q3 − Q1. Los valores que quedan fuera del intervalo  
**[Q1 − k·IQR, Q3 + k·IQR]**  
se marcan como NA. El factor *k* es configurable (por defecto *k* = 1.5, estándar en diagramas de caja).

**Caso IQR = 0:** En muchas series de precipitación en zonas áridas, la mediana y gran parte de los valores son cero, de modo que Q1 = Q3 y IQR = 0. En ese caso el método IQR no define un límite superior. Como **método alternativo** se utiliza el **percentil 99.5** de la serie de la estación: todo valor estrictamente mayor que ese percentil se marca como NA. Así se detectan los valores diarios extremos (posibles errores) incluso cuando la distribución es muy concentrada en cero.

### 3. Umbral estadístico (solo precipitación)

Este criterio se aplica únicamente a precipitación y permite capturar valores anómalos respecto a la distribución de cada estación:

- **Estaciones con precipitación media diaria muy baja (media &lt; 2 mm):** se considera sospechoso todo valor **mayor a 100 mm en un día**. Ese umbral absoluto es configurable y resulta útil en regiones áridas donde la media es del orden de 0,2–0,6 mm/día y valores de cientos de mm son prácticamente incompatibles con el clima local.
- **Resto de estaciones:** se calcula la media y la desviación estándar de la serie (por estación) y se marca como NA todo valor mayor que **media + *factor* × desviación estándar**, con un *factor* configurable (por defecto 10). Así se identifican valores que se alejan mucho de la distribución habitual de esa estación.

El umbral estadístico complementa al límite físico y al IQR, sobre todo en series con muchas observaciones nulas o cercanas a cero donde el IQR puede ser cero o muy pequeño.

---

## Orden de aplicación y salida

Los criterios se aplican **en secuencia** sobre la misma serie: primero límites físicos, luego IQR (o percentil 99.5 si IQR = 0) y por último el umbral estadístico. Un valor marcado como NA en un paso permanece como NA en los siguientes. La salida son series en el mismo formato que las brutas (fecha y columnas por estación), con los outliers reemplazados por NA, listas para el rellenado.

Se genera además un **reporte** (en consola y opcionalmente en CSV) que indica, por archivo y por estación, cuántos valores fueron marcados como NA por cada criterio (límites físicos, IQR y umbral estadístico), de modo que el usuario pueda revisar qué datos se consideraron outliers y por qué criterio.

---

## Parámetros configurables

La metodología permite ajustar los criterios al contexto de la región y al objetivo del estudio:

- **Límite superior de precipitación:** valor máximo aceptable en mm/día (por defecto 150).
- **Factor *k* del IQR:** mayor *k* (por ejemplo 3) hace el criterio IQR más conservador (menos valores marcados como NA).
- **Umbral absoluto para PP en estaciones de media muy baja:** por defecto 100 mm/día.
- **Factor de desviaciones estándar** en el umbral estadístico: por defecto 10.

Con estos parámetros se puede endurecer o suavizar la depuración según se trate de zonas más húmedas o más áridas y según la confianza en la calidad de los datos brutos.
