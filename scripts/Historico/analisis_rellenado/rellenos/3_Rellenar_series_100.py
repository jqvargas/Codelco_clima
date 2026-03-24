# -*- coding: utf-8 -*-
'''
Created on Thu Jul  3 09:06:23 2025

@author: Usuario
'''

# Paso 1: Estación tiene más del 70% de los datos para cada mes-año de estaciones en las regiones de Antofagasta hacia el sur (60% hacia el norte)
# Paso 2: Si hay <70% o 60% segpun corresponda para el mex del año y hacer RL con estaciones cercanas.
# Paso 3: Si no se puede hacer RL, rellenar con el promedio del mes x del año siguiente (y+1) y el año anterior (y-1)
# Paso 4: Si no es posible rellenar con e promdio, rellenar con los daos del mes x del año anterior (y-1)
# Paso 5: Si no hay datos para el mes x del año anterior, rellenar con CR2Met
# Paso 6: Si el paso anterior no es posible, rellenar con la mediana de todos los meses x (P50)

import pandas as pd
import numpy as np
from pyproj import Transformer
from sklearn.linear_model import LinearRegression


dir_in = 'C:/Users/Usuario/Universidad Católica de Chile/uc365_EFE Adaptación - Documents/General/Desarrollo/2_Análisis de amenaza y vulnerabilidad/1_Analisis de amenazas/'
dir_out = 'C:/Users/Usuario/Universidad Católica de Chile/uc365_EFE Adaptación - Documents/General/Desarrollo/2_Análisis de amenaza y vulnerabilidad/1_Analisis de amenazas/series/rellenas/'

# Cargar series brutas de precipitación
# df_pp = pd.read_csv(dir_in +'series/brutas/series_pp_1992_2021_bruta_EFE.csv')
# df_pp = pd.read_csv(dir_in +'series/brutas/series_t_max_1992_2021_bruta_EFE.csv')
df_pp = pd.read_csv(dir_in +'series/brutas/series_t_min_1992_2021_bruta_EFE.csv')
# df_pp = pd.read_csv(dir_in +'series/brutas/series_q_1992_2021_bruta_EFE.csv')

                    # , index_col = 'fecha', parse_dates= True)
df_pp.index = pd.to_datetime(df_pp['fecha'])
 
# Cargar metadata de las estaciones de precipitación
# df_meta_est = pd.read_csv(dir_in +'estaciones/estaciones_pp_EFE.csv', delimiter = ';', decimal = ',' )
df_meta_est = pd.read_csv(dir_in +'estaciones/estaciones_temp_EFE.csv', delimiter = ';', decimal = ',' )
# df_meta_est = pd.read_csv(dir_in +'estaciones/estaciones_caudal_EFE.csv', delimiter = ';', decimal = ',' )
transformer = Transformer.from_crs("EPSG:4326", "EPSG:32719")
x,y = transformer.transform(df_meta_est['lon'], df_meta_est['lat'])   
df_meta_est['xcoord'] = x.tolist()
df_meta_est['ycoord'] = y.tolist()

#%%

# =============================================================================
# Paso 1: Identificar las estaciones que tienen más del 70% de los datos para cada mes-año 
# de estaciones en las regiones de Antofagasta hacia el sur (60% hacia el norte)
# =============================================================================

# Crear lista con las regiones del norte 
# Metadata de estaciones de pp no tiene regiones de Tarapaca, ni Antofagasta
# Modo de chequeo:
# li_reg = df_meta_est['nom_reg'].drop_duplicates()
# No se incluyen regiones de Tarapaca y Antofagasta, porque no se sabe cómo se 
# escribirían esas estaciones si estuvieran en la metadata
li_norte = ['Arica y Parinacota']
#### ------------- ####

# Calcular el porcentaje de datos que tiene cada estación para mes-año 
li_est_1 = df_pp.keys().tolist()[4:]
li_meta_est = df_meta_est['codigo_nacional'].drop_duplicates().tolist()
df_count = df_pp.resample('ME').agg('count')

di_bool = {}
df_bool_day = pd.DataFrame()
for est in li_est_1:
    region =  df_meta_est['nom_reg'][df_meta_est['codigo_nacional'] == est].iloc[0]
    if region in li_norte:
        di_bool[est] = (df_count[est]/df_count['day'])>0.6
    else:
        di_bool[est] = (df_count[est]/df_count['day'])>0.7      
    
df_bool = pd.DataFrame(di_bool)
df_bool = df_bool.replace({True: 1, False: np.nan})
df_bool.insert(loc = 0, column='month', value= df_bool.index.month)
df_bool.insert(loc = 0, column='year', value= df_bool.index.year)
# df_bool.loc[df_bool.index[0]-pd.Timedelta("31 day")] = [0]*df_bool.shape[1]
# df_bool = df_bool.resample('D').bfill()
# df_bool = df_bool.drop([df_bool.index[0]],axis=0)

#%%

# =============================================================================
# Paso 2: Si hay <70% o 60% según corresponda para el mes-año, hacer RL
# con estaciones cercanas.
# =============================================================================

#### Paso 2.a: Identificar si hay estaciones que cumplen el criterio de distancia para realizar una RL ####
# Cantidad máxima en metros para que la estaciones se consideren para la RL
umbral = 30000
# Número de estaciones mínima necesaria para que se realice la RL
numero_estaciones = 2

li_estacion_asignada = []
li_estacion_asignada_dist = []

for i in range(len(df_meta_est)):
    # Extrae la coordenadas y las asigna a variables
    xcoord, ycoord, codigo = df_meta_est.iloc[i][['xcoord','ycoord', 'codigo_nacional']]
    estaciones = df_meta_est.copy()
    # Calcula la distancia (minimo cuadrado) de la estacion i  a cada una de las otras estaciones
    estaciones['Distancia'] = np.sqrt((xcoord-estaciones['xcoord'])**2 + (ycoord-estaciones['ycoord'])**2)
    # Crea un dataframe con todas las estaciones que se encuentra a una distancia menor o igual al umbral definido
    estaciones_filtrada = estaciones[(estaciones['Distancia']<=umbral)]
    estaciones_filtrada = estaciones_filtrada.query('codigo_nacional != @codigo')
    # Ordena las estaciones filtradas según la distancia a la que se encuentran de la est
    # De menor a mayor distancia
    estaciones_filtrada.sort_values(by=['Distancia'], inplace=True, ascending=True)
    
    if len(estaciones_filtrada)!=0:
        # En caso de que existan estaciones que cumplen con el umbral de distancia
        # Se re-define el dataframe con la información de las 3 estaciones más cercanas a la estacion
        # En caso de no haber 3 estaciones el dataframe queda con las dimensiones 
        # de las estaciones que existen
        estaciones_filtrada = estaciones_filtrada.iloc[:numero_estaciones]
        # Lista con el código de las estaciones seleccionadas, en caso de que no existan
        # el número de estaciones definidas se rellena la lista con espacios hasta completar 
        # la cantidad de estaciones máxima. Se agrega un espacio para la estación auxiliar
        li_estacion_temp = estaciones_filtrada['codigo_nacional'].tolist() + ['']*((numero_estaciones + 1)-len(estaciones_filtrada))
        # Lista con el la distancia de las estaciones seleccionadas, el orden corresponde
        # al de las estaciones de la lista anterior
        li_distancia_temp = estaciones_filtrada['Distancia'].tolist() + ['']*((numero_estaciones + 1) -len(estaciones_filtrada))

    else:
        # En caso de que no hubiera estaciones que cumplen con el umbral se busca la más cercana
        # Entrega el indice de la estación con la distancia minima, que será la estación auxiliar
        argmin_distancia = estaciones['Distancia'].argmin()
        # Se re-define el dataframe con la información de la estación auxiliar
        estaciones_filtrada = estaciones.iloc[[argmin_distancia]]
        # Se crea una lista con la cantidad de espacios del número de estaciones definidas y en último lugar
        # se añade el código de la estación auxiliar
        li_estacion_temp = ['']*numero_estaciones+estaciones_filtrada['codigo_nacional'].tolist()
        # Se crea una lista con la cantidad de espacios del número de estaciones definidas y en último lugar
        # se añade la distancia de la estación auxiliar
        li_distancia_temp = ['']*numero_estaciones+estaciones_filtrada['Distancia'].tolist()
    
    li_estacion_asignada.append(li_estacion_temp )
    li_estacion_asignada_dist.append(li_distancia_temp)
        
# Las listas creadas se asignan a un dataframa 
df_meta_est[['Estacion_'+str(i+1) for i in range(numero_estaciones)] + ['Estacion_aux']] = li_estacion_asignada
df_meta_est[['Distancia_'+str(i+1) for i in range(numero_estaciones)] +['Distancia_aux']] = li_estacion_asignada_dist

#### Paso 2.b: Identificar si las estaciones que cumplen el criterio de distancia también ####
# cumplen con el criterio de porcentaje de datos para el mes-año #
df_pp_fill_paso2 = df_pp.copy()
li_registro = []
df_bool_aux = df_bool.replace({1: np.nan})

di_pendientes_2 = {}
for est in li_est_1:
    li_pendientes_2 = []
    df_bool_est = df_bool_aux[est] 
    li_est_candidatas = df_meta_est.query('codigo_nacional == @est')[['Estacion_'+str(i+1) for i in range(numero_estaciones)]].iloc[0].tolist()
    
    # Extrae las fechas para las cuales la estación no cumple con el criterio de % de datos
    fecha_rellenar_2 = df_bool_est[df_bool_est.isna()].index
    # Chequeo para determinar si existe el número de estaciones necesarias para la RL
    if '' in li_est_candidatas:
        li_registro.append(f'{est}_No hay suficientes estaciones candidatas')
        di_pendientes_2[est] = [str(i.year) +'-' + str(i.month) for i in fecha_rellenar_2]
        print('No hay suficientes estaciones candidatas')
        pass
    else:
        # Recorre una a una las fechas que deben ser rellenadas
        for fecha in fecha_rellenar_2:
            year = fecha.year
            mes = fecha.month
            est_cumple = 0
            fechas_relleno = []
            for est_candidata in li_est_candidatas:
                # Extrae el valor de la estación candidata que indica si cumple (1) 
                # o no (nan) con el % de datos
                value_est_candidata = df_bool[est_candidata].loc[fecha]
                est_cumple = + value_est_candidata
            
            # Determina si todas las estaciones candidatas cumplen con el criterio de %
            if est_cumple == numero_estaciones:
                li_series_reg = []
                # Extrae la serie de observaciones para el mes-año en que se necesita rellenar
                li_series_reg.append(df_pp.loc[str(fecha.year)+'-'+str(fecha.month),est])
                for est_reg in li_est_candidatas:
                    # Extrae y agrega la serie de observaciones para el mes-año correspondientes
                    # de las estaciones con las que se va a rellenar la serie de la estación incompleta
                    li_series_reg.append(df_pp.loc[str(fecha.year)+'-'+str(fecha.month),est_reg])
                
                # Crea dataframe en que cada columna es la serie de una estación
                df_series_reg = pd.DataFrame(li_series_reg,index=['Y']+['X'+str(i+1) for i in range(numero_estaciones)]).T
                
                # Elimina todas las filas en las que algún valor es nan
                # Necesito que todas las estaciones (la que quiero rellenar y con las que voy a rellenar)
                # tengan valores para poder realizar la RL y obtener los parametros
                df_fit = df_series_reg.dropna(how='any')
                # Extrae los valores de la estación a rellenar en formato vector
                y = df_fit[['Y']].to_numpy()
                # Extrae los valores de las estaciones de relleno en formato vector
                x = df_fit[['X'+str(i+1) for i in range(numero_estaciones)]].to_numpy()
                
                if len(x) >0:
                    # Función para calcular la regresión linear
                    reg = LinearRegression().fit(x,y)
                    
                    # Extrae las filas en que al menos uno de los valores de las 
                    # estaciones de relleno es nan
                    df_x_predict = df_series_reg.loc[df_series_reg[['X'+str(i+1) for i in range(numero_estaciones)]].dropna(how='any').index]
                    # Extrae las filas donde el valor de X (estación original que se quiere rellenar)
                    # es nan
                    df_x_predict = df_x_predict.loc[df_x_predict['Y'].isna()]
                    n_rellenados = len(df_x_predict)
                    # Extrae los valores a los que se aplicaran los parametros de la RL
                    # para obtener los datos de relleno
                    x_predict = df_x_predict[['X'+str(i+1) for i in range(numero_estaciones)]].to_numpy()
                    # Dataframe con las series rellenadas aplicando los parametros obtenidos de la RL
                    df_pp_fill_paso2.loc[df_x_predict.index,est] = reg.predict(x_predict)
                
              
                    li_registro.append(f'{est}_{year}-{mes}_{n_rellenados}')
                else:
                    li_registro.append(f'{est}_No hay suficientes datos para_{year}-{mes}')
                    li_pendientes_2.append(f'{year}-{mes}')
                    print('No hay suficientes datos para una RL')
                di_pendientes_2[est] = li_pendientes_2
            else:
                li_registro.append(f'{est}_Las estaciones candidatas no cumplen para_{year}-{mes}')
                li_pendientes_2.append(f'{year}-{mes}')
                print('Las estaciones candidatas no cumplen')
            di_pendientes_2[est] = li_pendientes_2


#%%

# =============================================================================
# Paso 3: Si no se puede hacer RL, rellenar con el promedio del mes x del año siguiente (y+1) y el año anterior (y-1)
# =============================================================================

df_pp_fill_paso3 = df_pp_fill_paso2.copy()
li_est_3 = di_pendientes_2.keys()
di_pendientes_3 = {}

for est in li_est_3:
    li_fecha_est = di_pendientes_2[est]
    li_pendientes_3 = []
    for fecha in li_fecha_est:
        year = int(fecha.split('-')[0])
        mes = int(fecha.split('-')[1])
        if year == 1992 or year== 2021:
            li_pendientes_3.append(f'{year}-{mes}')
            print('P3: No tiene suficientes datos')
        else:
            year_prev = year-1
            year_fut = year + 1
            bool_prev= df_bool[est].loc[str(year_prev)+'-'+str(mes)].iloc[0]
            bool_fut= df_bool[est].loc[str(year_fut)+'-'+str(mes)].iloc[0]
            chequeo = bool_prev + bool_fut
            if chequeo == 2:
                df_prev = df_pp.loc[str(year_prev)+'-'+str(mes),est]
                df_fut = df_pp.loc[str(year_fut)+'-'+str(mes),est]
                df_original = df_pp.loc[str(year)+'-'+str(mes),est]
                if mes ==2:
                    df_prev = df_prev.iloc[:28]
                    df_fut = df_fut.iloc[:28]
                    df_original = df_original.iloc[:28]
                else:
                    pass
                
                df_prev.index = df_original.index
                df_fut.index = df_original.index
                df_mean = (df_prev + df_fut)/2
                fechas_rellenar_3 = df_original.loc[df_original.isna()].index
                est_cumple_bool = (df_original.count()+sum(~df_mean.loc[fechas_rellenar_3].isna()))/len(df_original) >0.7 
                              
                if est_cumple_bool:
                    df_pp_fill_paso3.loc[fechas_rellenar_3,est] = df_mean.loc[fechas_rellenar_3]
                else:
                    li_pendientes_3.append(f'{year}-{mes}')  
                    print('P3: El relleno no tiene suficientes datos')
            else:
                li_pendientes_3.append(f'{year}-{mes}')
                print('P3: No tiene suficientes datos')
    di_pendientes_3[est] = li_pendientes_3
                         
#%%

# =============================================================================
# Paso 4: Si no es posible rellenar con el promedio, rellenar con los datos del mes x del año anterior (y-1)
# =============================================================================

df_pp_fill_paso4 = df_pp_fill_paso3.copy()
li_est_4 = di_pendientes_3.keys()
di_pendientes_4 = {}

for est in li_est_4:
    li_fecha_est = di_pendientes_3[est]
    li_pendientes_4 = []
    for fecha in li_fecha_est:
        year = int(fecha.split('-')[0])
        mes = int(fecha.split('-')[1])
        if year == 1992 or year== 2021:
            li_pendientes_4.append(f'{year}-{mes}')
            print('P4: No tiene suficientes datos')
        else:
            year_prev = year-1
            bool_prev= df_bool[est].loc[str(year_prev)+'-'+str(mes)].iloc[0]    
            if bool_prev == 1:
                df_prev = df_pp.loc[str(year_prev)+'-'+str(mes),est]
                df_original = df_pp.loc[str(year)+'-'+str(mes),est]
                if mes ==2:
                    df_prev = df_prev.iloc[:28]
                    df_original = df_original.iloc[:28]
                else:
                    pass
                
                df_prev.index = df_original.index
                df_relleno = df_prev
                fechas_rellenar_4 = df_original.loc[df_original.isna()].index
                est_cumple_bool = (df_original.count()+sum(~df_relleno.loc[fechas_rellenar_4].isna()))/len(df_original) >0.7 
                              
                if est_cumple_bool:
                    df_pp_fill_paso4.loc[fechas_rellenar_4,est] = df_relleno.loc[fechas_rellenar_4]
                else:
                    li_pendientes_4.append(f'{year}-{mes}')  
                    print('P4: El relleno no tiene suficientes datos')
            else:
                li_pendientes_4.append(f'{year}-{mes}')
                print('P4: No tiene suficientes datos')
    di_pendientes_4[est] = li_pendientes_4

#%%
# =============================================================================
# Paso 5: Si no hay datos para el mes x del año anterior, rellenar con CR2Met
# =============================================================================

dir_in2 = 'C:/Users/Usuario/Universidad Católica de Chile/uc365_BBDD - General/Desarrollo/Datos/CR2_v25_interpolado_estaciones/'

# Cargar series CR2Met v2.5 diarias de precipitación de las estaciones
# df_pp_cr2met = pd.read_csv(dir_in2 +'pr_CR2MET_v25_day_1980_2020_grilla.csv', index_col = 'Fecha', parse_dates= True, dayfirst=True)
# df_pp_cr2met = pd.read_csv(dir_in2 +'tmax_CR2MET_v25_day_1980_2020_grilla.csv', index_col = 'Fecha', parse_dates= True, dayfirst=True)
df_pp_cr2met = pd.read_csv(dir_in2 +'tmin_CR2MET_v25_day_1980_2020_grilla.csv', index_col = 'Fecha', parse_dates= True, dayfirst=True)
# Lista de las estaciones que tienen serie cr2met (id_ccg)
li_cr2met_est = df_pp_cr2met.keys()[3:].tolist()

# Cargar metadata de las estaciones con id_ccg y codigo_nacional
df_meta_est_CCG = pd.read_csv(dir_in2 +'metadatos_actualizados.csv')

df_pp_fill_paso5 = df_pp_fill_paso4.copy()
li_est_5 = list(di_pendientes_4.keys())
di_pendientes_5 = {}

for est in li_est_5:
    li_fecha_est_5 = di_pendientes_4[est]
    li_pendientes_5 = []
    # Busca el codigo id_ccg equivalente de la estación estación (codigo_nacinal)
    est_id_ccg = str(df_meta_est_CCG['id'][df_meta_est_CCG['codigo_nacional'] == est].iloc[0])
    for fecha in li_fecha_est_5:
        year = int(fecha.split('-')[0])
        mes = int(fecha.split('-')[1])
        
        if  year== 2021:
            li_pendientes_5.append(f'{year}-{mes}')
            print('P5: No tiene suficientes datos') 
        else:
            if est_id_ccg in li_cr2met_est:
                # Crea un dataframe con las series de cr2met de la estación para el año-mes 
                df_cr2met = df_pp_cr2met.loc[str(year)+'-'+str(mes),est_id_ccg]
                # Crea un dataframe con las series originales de la estación para el año-mes 
                df_original = df_pp.loc[str(year)+'-'+str(mes),est]
                fechas_rellenar_5 = df_original.loc[df_original.isna()].index
                est_cumple_bool = (df_original.count()+sum(~df_cr2met.loc[fechas_rellenar_5].isna()))/len(df_original) >0.7 
                              
                if est_cumple_bool:
                    df_pp_fill_paso5.loc[fechas_rellenar_5,est] = df_cr2met.loc[fechas_rellenar_5]
                else:
                    li_pendientes_5.append(f'{year}-{mes}')  
                    print('P5: Serie cr2met no tiene suficientes datos')
            else:
                li_pendientes_5.append(f'{year}-{mes}')
                print('P5: Estacion no tiene serie cr2met')
    di_pendientes_5[est] = li_pendientes_5
        
df_pp_fill_paso5 = df_pp_fill_paso5.rename(columns={'year':'Year', 'month':'Month', 'day':'Day'})

# df_pp_fill_paso5.to_csv(dir_out + 'series_pp_1992_2021_rellena_100_EFE.csv', index=False)
    
#%%
# =============================================================================
# # Paso 6: Si el paso anterior no es posible, rellenar con la mediana de todos los meses x (P50)
# =============================================================================

df_pp_fill_paso6 = df_pp_fill_paso5.copy()
li_est_6 = di_pendientes_5.keys()

df_bool_day = df_bool.copy()

df_bool_day.loc[df_bool_day.index[0]-pd.Timedelta("31 day")] = [0]*df_bool_day.shape[1]
df_bool_day = df_bool_day.resample('D').bfill()
df_bool_day = df_bool_day.drop([df_bool_day.index[0]],axis=0)


df_pp_original_filtrado = (df_pp*df_bool_day).drop(['fecha','year','month','day'],axis=1)
mediana_diaria_original = df_pp_original_filtrado.groupby(df_pp_original_filtrado.index.dayofyear).median()

for est in li_est_6:
    df_fill_est = df_pp_fill_paso5[est]
    fechas_rellenar_6 = df_fill_est[df_fill_est.isna()].index
    
    df_pp_fill_paso6.loc[fechas_rellenar_6,est] = mediana_diaria_original.loc[fechas_rellenar_6.dayofyear,est].tolist()
    

df_pp_fill_paso6.to_csv(dir_out + 'series_t_min_1992_2021_rellena_100_EFE.csv', index=False)
    
    