# ==============================================================================
# Procesamiento: Ventas por actividad economica
# DNPRMF
# Fabian Villarreal
#
# Script que procesa la informacion de las ventas por actividad economica 
# (CICN) del Formulario 104 del SRI para uso en previsiones macroeconomicas
# ==============================================================================

rm(list = ls())

# Librerias
pacman::p_load(tidyverse, readxl, openxlsx, dplyr, ggplot2, ggfortify, foreign, 
               timeDate, forecast, xts, urca, tseries, lubridate, stringi, 
               stringr, reshape2, expsmooth, seasonal, Metrics)

# Colores
paleta <- c('#1E4976', '#5FA3BF', '#BBBE64', '#EFCB68', '#E5825E', '#D05022')

# Directorio y archivos de resultados
wd <- 'D:/DNPRMF/Programacion Sector Real/Previsiones/Previsiones 2023 - P2/Formulario 104 SRI'
setwd(wd)

archivo_excel <- 'Resultados/ventas_2023_05.xlsx'
mes_fin <- 5 # Mes hasta el que se cuenta con informacion (Validar con DNSM)



### Bases ----------------------------------------------------------------------

# Base: Formulario 104 SRI
df_sri <- data.frame(read_excel('Datos/Formulario 104/2023-07-07 tabla resumen CNT F104.xlsx',
                                sheet = 'Sheet 1'))

# Base: indp (2007 = 100) # Solicitar a Emanuel
df_indp <- data.frame(read_excel(paste0('D:/DNPRMF/Programacion Sector Real/Precios/Ind Precios CICN/',
                                        'Resultados/ind_precios_cicn.xlsx'),
                                sheet = 'Ind Precios'))



### Procesamiento --------------------------------------------------------------


### Base -----------------------------------------------------------------------

# Tratamiento
df_base <- df_sri

df_base <- df_base[!is.na(df_base$COD_CICN_CAB2007),] # Eliminar sin CICN
df_base <- df_base[df_base$UNIDAD_LEGAL_TIPO_BCE %in% c('SCD'),] # Sociedades
df_base <- df_base[df_base$TIPO_PERIOCIDAD %in% c('MENSUAL'),] # Frecuencia de declaraciones

df_base <- df_base[, c('ANIO_FISCAL', 'MES_FISCAL', 'COD_CICN_CAB2007', 'TOTAL_VENTAS', 'TOTAL_VENTAS.R')]
colnames(df_base) <- c('year', 'month', 'cicn', 'vt', 'vti')

# Codigo CICN
df_base$cod_cicn <- substr(df_base$cicn, 0, 3)

# Agregar anio, mes
mes <- as.yearmon(as.Date(paste(df_base$year, df_base$month, 01), '%Y %m %d'))
df_base <- cbind(mes, df_base)
colnames(df_base)[1] <- 'mes'

inf_disp <- max(df_base$mes) - (1/12) # Eliminar meses con informacion incompleta
df_base <- df_base[df_base$mes <= inf_disp,] 

aux_pos <- 4 # Orden columnas, indice en el cual se desea cambiar
df_base <- df_base[, c(1:aux_pos, length(df_base), (aux_pos+1):(length(df_base)-1))] 

df_base <- df_base[order(df_base$cicn, df_base$mes),] # Ordenar por fecha



### Ventas corrientes ----------------------------------------------------------

# Base en valores nominales
df_Ventas_C <- df_base

# Industrias a descartar
drop_f1 <- c('000') # Industrias no clasificadas
drop_f2 <- c('003', '006', '009', '010', '046') # Industrias que no poseen indice de precios

df_Ventas_C <- df_Ventas_C[!df_Ventas_C$cod_cicn %in% drop_f1,]
df_Ventas_C <- df_Ventas_C[!df_Ventas_C$cod_cicn %in% drop_f2,]

# Agregar informacion a nivel de CICN
df_Ventas_C <- df_Ventas_C %>% 
  group_by(year, month, cod_cicn) %>% 
  summarise(vt = sum(vt), vti = sum(vti))

# Agregar anio, mes 
mes <- as.yearmon(as.Date(paste(df_Ventas_C$year, df_Ventas_C$month, 01), '%Y %m %d'))
df_Ventas_C <- cbind(mes, df_Ventas_C)
colnames(df_Ventas_C)[1] <- 'mes'

df_Ventas_C <- df_Ventas_C[order(df_Ventas_C$cod_cicn, df_Ventas_C$mes),] # Ordenar por fecha



### Indice de precios ----------------------------------------------------------

# Tratamiento
df_indp$mes <- as.yearmon(df_indp$mes)



### Ventas reales --------------------------------------------------------------

# Base con ventas en valores reales de acuerdo con el indp
df_Ventas_K <- merge(df_Ventas_C, df_indp, by = c('cod_cicn', 'mes'), all.x = TRUE)
df_Ventas_K$vtr <- df_Ventas_K$vt / df_Ventas_K$indp * 100
df_Ventas_K$vtir <- df_Ventas_K$vti / df_Ventas_K$indp * 100

col_order <- c('mes', 'year', 'month', 'cod_cicn', 'vt', 'vti', 'indp', 'vtr', 'vtir')
df_Ventas_K <- df_Ventas_K[, col_order]

df_Ventas_K <- df_Ventas_K[order(df_Ventas_K$cod_cicn, df_Ventas_K$mes),] # Ordenar



### Resultados -----------------------------------------------------------------

# Base con resultados de ventas reales y constantes
df_Resultados_K <- df_Ventas_K
df_Resultados_C <- df_Ventas_C

# Reshape

# Considerar si se quiere ventas imputadas (vtir) o no imputadas (vtr)
df_Resultados_K$cod_cicn <- paste0('cicn_', df_Resultados_K$cod_cicn)
df_Resultados_K <- df_Resultados_K %>%
  pivot_wider(id_cols = 'mes', names_from = 'cod_cicn', values_from = 'vtr') 

# Considerar si se quiere ventas imputadas (vti) o no imputadas (vt)
df_Resultados_C$cod_cicn <- paste0('cicn_', df_Resultados_C$cod_cicn)
df_Resultados_C <- df_Resultados_C %>% 
  pivot_wider(id_cols = 'mes', names_from = 'cod_cicn', values_from = 'vt')

# Ventas totales
df_Resultados_K$Total <- rowSums(df_Resultados_K[, -1], na.rm = TRUE)
df_Resultados_K <- df_Resultados_K[df_Resultados_K$Total != 0,]

df_Resultados_K <- df_Resultados_K[order(df_Resultados_K$mes),] # Ordenar

df_Resultados_C$Total <- rowSums(df_Resultados_C[, -1], na.rm = TRUE)
df_Resultados_C <- df_Resultados_C[df_Resultados_C$Total != 0,]

df_Resultados_C <- df_Resultados_C[order(df_Resultados_C$mes),] # Ordenar



### Indices (Base 2015m1 = 100) ------------------------------------------------
df_Indices <- df_Resultados_K
df_Indices[, -1] <- data.frame(lapply(df_Indices[, -1], function(x) x / x[1]))



### Variacion T/T-1 ------------------------------------------------------------

# Variacion mensual
df_var_t1 <- df_Indices
df_var_t1[, -1] <- data.frame(lapply(df_var_t1[, -1], function(x) x / lag(x)))



### Variacion T/T-12 -----------------------------------------------------------

# Variacion interanual
df_var_t12 <- df_Indices
df_var_t12[, -1] <- data.frame(lapply(df_var_t12[, -1], function(x) x / lag(x, n = 12)))



### Variacion trimestral interanual --------------------------------------------

# Obtener media movil de 3 meses del indice
df_trimestral_ma <- df_Indices
df_trimestral_ma[, -1] <- rollmean(df_trimestral_ma[, -1], 
                                   k = 3, 
                                   fill = NA, 
                                   align = 'right')

# Obtener variacion interanual del promedio trimestral del indice
df_trimestral <- df_trimestral_ma
df_trimestral[, -1] <- data.frame(lapply(df_trimestral[, -1], function(x) x / lag(x, n = 12)))



### Variacion semestral interanual ---------------------------------------------

# Obtener media movil de 3 meses del indice encadenado
df_semestral_ma <- df_Indices
df_semestral_ma[, -1] <- rollmean(df_semestral_ma[, -1], 
                                  k = 6, 
                                  fill = NA, 
                                  align = 'right')

# Obtener variacion interanual del promedio trimestral del indice
df_semestral <- df_semestral_ma
df_semestral[, -1] <- data.frame(lapply(df_semestral[, -1], function(x) x / lag(x, n = 12)))



### Exportacion de resultados --------------------------------------------------

# Dataframes a guardar
dfs <- list(df_base, df_Ventas_C, df_Ventas_K, df_Resultados_C, df_Resultados_K, 
            df_Indices, df_var_t1, df_var_t12, df_trimestral, df_semestral)
dfn <- c('Base', 'Ventas C', 'Ventas K', 'Resultados C', 'Resultados K', 
         'Indices', 'Var T_T-1', 'Var T_T-12', 'Var Trimestral', 'Var Semestral')

# Formato de mes
for (i in 1:length(dfs)) {
  dfs[[i]]$mes <- as.Date(dfs[[i]]$mes)
}

# Workbook
wb <- createWorkbook()

# Crear hojas y guardar dataframes
for (i in 1:length(dfs)) {
  addWorksheet(wb, dfn[i])
  writeData(wb, dfn[i], dfs[[i]])
}

# Formato de celdas
bold_text <- createStyle(textDecoration = 'bold')
mesf_text <- createStyle(numFmt = 'mmm-yyyy')
usdf_text <- createStyle(numFmt = '#,##0')
indf_text <- createStyle(numFmt = '0.000')

for (i in 1:length(dfs)) {
  addStyle(wb, sheet = dfn[i], bold_text, 
           rows = 1, cols = 1:ncol(dfs[[i]]), gridExpand = TRUE) # Bold en columnas
  addStyle(wb, sheet = dfn[i], mesf_text, 
           rows = 2:(nrow(dfs[[i]]) + 1), cols = 1, gridExpand = TRUE) # Formato mes-anio
}

# Formato USD
addStyle(wb, sheet = dfn[1], usdf_text, 
         rows = 2:(nrow(dfs[[1]]) + 1), cols = 6:11, gridExpand = TRUE)

addStyle(wb, sheet = dfn[2], usdf_text, 
         rows = 2:(nrow(dfs[[2]]) + 1), cols = 5:6, gridExpand = TRUE)

addStyle(wb, sheet = dfn[3], usdf_text, 
         rows = 2:(nrow(dfs[[3]]) + 1), cols = c(5:6, 8:9), gridExpand = TRUE)
addStyle(wb, sheet = dfn[3], indf_text, 
         rows = 2:(nrow(dfs[[3]]) + 1), cols = 7, gridExpand = TRUE)

addStyle(wb, sheet = dfn[4], usdf_text, 
         rows = 2:(nrow(dfs[[4]]) + 1), cols = 2:ncol(dfs[[4]]), gridExpand = TRUE)

addStyle(wb, sheet = dfn[5], usdf_text, 
         rows = 2:(nrow(dfs[[5]]) + 1), cols = 2:ncol(dfs[[5]]), gridExpand = TRUE)

# Formato indices
for (i in 6:length(dfs)) {
  addStyle(wb, sheet = dfn[i], indf_text,
           rows = 2:(nrow(dfs[[i]]) + 1), cols = 2:ncol(dfs[[i]]), gridExpand = TRUE)
}

# Resultados
saveWorkbook(wb, file = archivo_excel, overwrite = TRUE)
