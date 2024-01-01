# ==============================================================================
# Procesamiento: Ventas por actividad economica
# DNPRMF
# Fabian Villarreal
# 
# Forecast con ARIMA y Holt-Winters
# ==============================================================================

# Librerias
pacman::p_load(tidyverse, readxl, openxlsx, dplyr, ggplot2, ggfortify, foreign, 
               timeDate, forecast, xts, urca, tseries, lubridate, stringi, 
               stringr, reshape2, expsmooth, seasonal, Metrics)

# Colores
paleta <- c('#1E4976', '#5FA3BF', '#BBBE64', '#EFCB68', '#E5825E', '#D05022')

# Directorio
setwd(wd)



# Base -------------------------------------------------------------------------
df_ts <- readRDS('Resultados/base_modelo_VentasK.rds')

# Numero de predicciones hasta fin de anio
num_predicciones <- 12 - month(max(index(df_ts)))
if (num_predicciones == 0) {
  num_predicciones <- 12
}

df_ts$cicn_047 <- NULL # Se descarta industria 047 por presencia de NAs en la serie



# Auto ARIMA -------------------------------------------------------------------

# Algoritmo Hyndman-Khandakar
arima_opt <- list() # Lista para almacenar los mejores modelos por cada industria en funcion del AIC

varlist <- colnames(df_ts)
for (var in varlist) {
  
  # Estimacion
  print(var)
  arima_fit <- list()
  
  arima1 <- auto.arima(df_ts[, var], seasonal = FALSE, stepwise = TRUE, d = 0, max.p = 12, max.q = 12)
  arima_fit[1] <- list(arima1)
  
  arima2 <- auto.arima(df_ts[, var], seasonal = FALSE, stepwise = FALSE, d = 0, max.p = 12, max.q = 12)
  arima_fit[2] <- list(arima2)
  
  arima3 <- auto.arima(df_ts[, var], seasonal = TRUE, stepwise = TRUE, d = 0, max.p = 12, max.q = 12)
  arima_fit[3] <- list(arima3)
  
  arima4 <- auto.arima(df_ts[, var], seasonal = TRUE, stepwise = FALSE, d = 0, max.p = 12, max.q = 12)
  arima_fit[4] <- list(arima4)
  
  # AIC
  arima_aic <- rep(0, 4)
  arima_aic[1] <- arima1$aic
  arima_aic[2] <- arima2$aic
  arima_aic[3] <- arima3$aic
  arima_aic[4] <- arima4$aic
  
  # Modelo con el menor AIC
  modelo_opt <- min(which(arima_aic == min(arima_aic)))
  
  # Almacenar mejor modelo
  arima_opt[var] <- arima_fit[modelo_opt]
}



# Holt-Winters -----------------------------------------------------------------
holt_winters <- list() # Lista para almacenar los modelos Holt-Winters

varlist <- colnames(df_ts)
for (var in varlist) {
  print(var)
  holt_winters[var] <- list(HoltWinters(na.omit(df_ts[, var])))
}



# Resultados -------------------------------------------------------------------
save(df_ts, arima_opt, holt_winters, num_predicciones,
     file = 'Resultados/lista_modelos_VentasK.RData')

load('Resultados/lista_modelos_VentasK.RData')
varlist <- colnames(df_ts)



# Forecast ---------------------------------------------------------------------

# RMSE de modelos para combinacion de forecast
m_rmse <- matrix(NA, nrow = length(varlist), ncol = 2) # Matriz de RMSE
rownames(m_rmse) <- varlist
colnames(m_rmse) <- c('auto_arima', 'holt_winters')

for (var in varlist) {
  m_rmse[var, 'auto_arima'] <- rmse(arima_opt[[var]]$x, arima_opt[[var]]$fitted)
  m_rmse[var, 'holt_winters'] <- rmse(holt_winters[[var]]$x, holt_winters[[var]]$fitted)
}

# Pesos para los forecast con base en RMSE
# https://documentation.sas.com/doc/en/hpfug/14.2/hpfug_hpfcbm_sect012.htm
m_weights <- matrix(NA, nrow = length(varlist), ncol = 3) # Matriz de pesos RMSE
rownames(m_weights) <- varlist
colnames(m_weights) <- c('W', 'auto_arima', 'holt_winters')

for (var in varlist) {
  m_weights[, 'W'] <- rowSums(1 / m_rmse)
  m_weights[var, 'auto_arima'] <- (1 / m_rmse[var, 'auto_arima']) / m_weights[var, 'W']
  m_weights[var, 'holt_winters'] <- (1 / m_rmse[var, 'holt_winters']) / m_weights[var, 'W']
}

# Forecast combinado: Auto Arima y Holt-Winters
m_forecast <- matrix(NA, nrow = num_predicciones, ncol = length(varlist))
colnames(m_forecast) <- varlist

for (var in varlist) {
  arima <- as.vector(forecast::forecast(arima_opt[[var]], h = num_predicciones)$mean + 1)
  hwinters <- as.vector(forecast::forecast(holt_winters[[var]], h = num_predicciones)$mean + 1)
  
  m_forecast[, var] <- 
    m_weights[var, 'auto_arima'] * arima + m_weights[var, 'holt_winters'] * hwinters
}

# Dataframe con forecast
df_ts_fcast <- data.frame(m_forecast)

mes <- as.yearmon(seq(as.Date(max(index(df_ts)) + 1/12), 
                      by = 'month', 
                      length.out = num_predicciones))

df_ts_fcast <- cbind(mes, df_ts_fcast)

df_ts_fcast <- xts(df_ts_fcast[, -1], order.by = df_ts_fcast$mes, frequency = 12)



# Resultados de datos observados y forecast ------------------------------------
df_obs <- df_ts

df_obs <- df_obs + 1

df_res <- rbind(df_obs, df_ts_fcast) # Combinacion

df_res <- df_res[year(index(df_res)) == max(year(index(df_res)))] # Quedarse con ultimo anio

# Promedio geometrico de indices observados y de forecast

# Promedio de valores positivos
df_res_positivos <- df_res
for (i in colnames(df_res)) {
  df_res_positivos[, i] <- replace(df_res_positivos[, i], which(df_res_positivos[, i] <= 0), NA)
}

df_prom <- data.frame(matrix(nrow = 1, ncol = ncol(df_res)))
colnames(df_prom) <- colnames(df_res)

for (i in colnames(df_prom)) {
  df_prom[, i] <- prod(df_res_positivos[, i], na.rm = TRUE)
  
  v <- sum(!is.na(df_res_positivos[, i]))
  df_prom[, i] <- df_prom[, i]**(1/v)
}

df_res_xlsx <- cbind(mes = as.Date(index(df_res)), data.frame(df_res))

df_prom_xlsx <- cbind(Promedio = 'Promedio geometrico', df_prom)



# Exportacion de resultados ----------------------------------------------------
wb <- loadWorkbook(archivo_excel)

# Crear hoja y guardar dataframe
worksheet = 'Forecast VentasK'
addWorksheet(wb, worksheet, tabColour = paleta[3])
writeData(wb, worksheet, df_res_xlsx)
writeData(wb, worksheet, df_prom_xlsx, startRow = nrow(df_res_xlsx) + 3)

# Formato de celdas
bold_text <- createStyle(textDecoration = 'bold')
mesf_text <- createStyle(numFmt = 'mmm-yyyy')
indf_text <- createStyle(numFmt = '0.000',halign = 'center', valign = 'center')
cell_text <- createStyle(wrapText = TRUE, halign = 'left')

addStyle(wb, sheet = worksheet, bold_text,
         rows = c(1, nrow(df_res_xlsx) + 3), 
         cols = 1:ncol(df_res_xlsx), gridExpand = TRUE) # Bold en columnas

addStyle(wb, sheet = worksheet, mesf_text,  
         rows = 2:(nrow(df_res_xlsx) + 1), cols = 1, gridExpand = TRUE) # Formato mes-anio

addStyle(wb, sheet = worksheet, indf_text, 
         rows = c(2:(nrow(df_res_xlsx) + 1), nrow(df_res_xlsx) + 4), 
         cols = 2:ncol(df_res_xlsx), gridExpand = TRUE)

addStyle(wb, sheet = worksheet, cell_text,
         rows = nrow(df_res_xlsx) + 4, cols = 1, gridExpand = TRUE) 

# Resultados
saveWorkbook(wb, file = archivo_excel, overwrite = TRUE)
