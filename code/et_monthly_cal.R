## Calculate Evapotranspiration based on 'modified Hargreaves' equation
## (Droogers and Allen, 2002)
## ET0 = 0.0013 * 0.408 * RA * (TAv + 17) * (TD - 0.0123P)^0.76

## TAv  = average of the mean daily maximum and mean daily minimum temperatures for each month (Tavg in degrees Celsius)
## TD   = difference between mean daily maximum and mean daily minimums for each month (TD)
## RA   = extraterrestrial radiation in MJm^-2d^-1
## P    = precipitation in mm per month

## RA based on FAO Irrigation Drainage Paper 56 (Allan (1998)) for Xiamen (Lat ~ 24)

library(terra)

## Xiamen boundary
border <- vect("E:/Bird/data/xiamen/extent/border.shp")
plot(border)

lcz <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/xm_lcz.tif")
lcz_xm <- crop(lcz, border, mask=TRUE)
plot(lcz_xm)

writeRaster(lcz_xm$xm_lcz_1, 
            "E:/ES_Demand_Supply/data/UrbanCooling/data/xm_lcz_1.tif",
            datatype="INT8U")

## WorldClim

## Precipitaion
prec <- list.files(path = "E:/ES_Demand_Supply/data/UrbanCooling/data/ET/wc2.1_cruts4.06_2.5m_prec_2020-2021/y2021", 
                       pattern='\\.tif$', 
                       all.files= T, 
                       full.names= T)

prec

prec_stack <- rast(prec)
names(prec_stack) <- paste0("month_", 1:12)
prec_stack_xm <- crop(prec_stack, border, mask=TRUE)

plot(prec_stack_xm)

## Temperature (Max)
temp_max <- list.files(
  path = "E:/ES_Demand_Supply/data/UrbanCooling/data/ET/wc2.1_cruts4.06_2.5m_tmax_2020-2021/y2021",
  pattern = '.tif$',
  all.files = TRUE,
  full.names = TRUE
)

temp_max <- rast(temp_max)
names(temp_max) <- paste0("month_", 1:12)
temp_max_xm <- crop(temp_max, border, mask=TRUE)
plot(temp_max_xm)

## Temperature (Min)
temp_min <- list.files(
  path = "E:/ES_Demand_Supply/data/UrbanCooling/data/ET/wc2.1_cruts4.06_2.5m_tmin_2020-2021/2021",
  pattern = '.tif$',
  all.files = TRUE,
  full.names = TRUE
)

temp_min <- rast(temp_min)
names(temp_min) <- paste0("month_", 1:12)

temp_min_xm <- crop(temp_min, border, mask=TRUE)
plot(temp_min_xm)


## Calculate TD

td <- temp_max_xm - temp_min_xm
plot(td)

## ET0 calculation
ra <- read.csv("E:/ES_Demand_Supply/data/UrbanCooling/data/ET/ra_xiamen_fao.csv")
ra_monthly <- c(ra$ra)
ra_monthly

# Tav
temp_list <- list(temp_max_xm, temp_min_xm)
temp_list

s <- sds(temp_list)
tva <- app(s, mean)
tva
plot(tva)

# temp_max_mean <- global(temp_max_xm, "mean", na.rm=TRUE)
# temp_max_mean
# 
# temp_min_mean <- global(temp_min_xm, "mean", na.rm=TRUE)
# temp_min_mean


## Calculate ET0

et0 <- 0.0013 * 0.408 * ra_monthly * (tva + 17) * (td - 0.0123 * prec_stack_xm)^0.76
names(et0) <- paste0("et0_", 1:12)
plot(et0)
et0
# Write to file by month
out_dir <- "E:/ES_Demand_Supply/data/UrbanCooling/data/ET/et_monthly"

writeRaster(et0, filename=paste0(names(et0), ".tif"), overwrite=TRUE)
