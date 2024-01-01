# ==============================================================================
# DNPRMF
# Script para procesar datos: Empleo registrado por rama de actividad - IESS
# Fabian Villarreal
# 
# Registro de Empleo en la Seguridad Social
# https://www.ecuadorencifras.gob.ec/registro-empleo-seguridad-social/
# ==============================================================================

# Librerias
pacman::p_load(tidyverse, readxl, openxlsx, dplyr, ggplot2, ggfortify, foreign, 
               timeDate, forecast, xts, urca, tseries, lubridate, stringi, 
               stringr, reshape2, expsmooth, seasonal, Metrics)

# Colores
my_pal <- c('#1E4976', '#5FA3BF', '#BBBE64', '#EFCB68', '#E5825E', '#D05022')

# Directorio
setwd('D:/fvillarreal/Programacion Sector Real/Otros indicadores/Empleo IESS')



# Datos: Empleo registrado por rama de actividad
df <- read_excel('Datos/Indicadores Laborales_Empleo_06_22.xlsx',
                 sheet = '1_1_1',
                 skip = 5)

df <- data.frame(df[1:24,])

# Labels de industrias
df[22, 1] <- 'Z0'; df[23, 1] <- 'Z1'; df[24, 1] <- 'Z2'

df[22, 2] <- 'No clasificado'
df[23, 2] <- 'Doméstico'
df[24, 2] <- 'Campesino'

# Nombre de columnas
mes_range <- as.character(seq(as.Date('2009-01-01'),
                            by = 'month',
                            length.out = ncol(df)-2))

colnames(df) <- c('ciiu', 'labels', mes_range)

# Labels
labels <- df[1:21, 1:2]
labels$labels <- substring(labels$labels, 1, nchar(labels$labels)-1)
saveRDS(labels, file = 'Resultados/labels.rds')

df <- df[!names(df) %in% c('labels')]

# Reshape
df <- melt(df, id.vars = 'ciiu', 
           variable.name = 'anio_mes',
           value.name = 'empleo')

df['empleo'][is.na(df['empleo'])] <- 0
df$anio_mes <- as.yearmon(df$anio_mes)

# Anio y mes
df$anio = year(df$anio_mes)
df$mes = month(df$anio_mes)

# Orden
df <- df[, c('ciiu', 'anio_mes', 'anio', 'mes', 'empleo')]


# Resultados
wb <- createWorkbook()

addWorksheet(wb, 'empleo')
writeData(wb, 'empleo', df)

addWorksheet(wb, 'labels')
writeData(wb, 'labels', labels)

saveWorkbook(wb, file = 'Resultados/empleo_iess_2022_06.xlsx', overwrite = TRUE)
