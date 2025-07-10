library(readr)
library(sf)
library(dplyr)
library(terra)
library(ggplot2)

pm_in <- "E:/ES_Demand_Supply/data/xiamen/pm2.5/PM2.5/monthly/pm_25_monthly"

## Precipitaion
pm25_monthly <- list.files(path = pm_in, 
                   pattern='\\.tif$', 
                   all.files= T, 
                   full.names= T)

pm25_monthly

pm25_stack <- rast(pm25_monthly)
names(pm25_stack) <- paste0("month_", 1:12)

plot(pm25_stack)

pm25_annual <- app(pm25_stack, fun=median)

plot(pm25_annual)
