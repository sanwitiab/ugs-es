library(readr)
library(sf)
library(dplyr)
library(terra)
library(ggplot2)

source("code/helper.R") # call file and function for analysis

# File paths
## ras data yearly
t_in <- "E:/ES_Demand_Supply/data/xiamen/pm2.5/Tile"

## raw data monthly
pm_in <- "E:/ES_Demand_Supply/data/xiamen/pm2.5/PM2.5/monthly/dec"

## save result file
pm_out <- "E:/ES_Demand_Supply/data/xiamen/pm2.5/xiamen_pm25/pm25_xm.tif"

## Read tile ------------------------------------------------------------------
tile <- list.files(t_in,
                   pattern = '*.csv',
                   full.names = TRUE)

# Read and Combine to one files
df.all = do.call(rbind, lapply(tile, 
                               function(x) read.csv(x)))

# Convert to SF
df_sf <- st_as_sf(df.all,
                  coords = c("Longitude", "Latitude"),
                  crs = 4326,
                  agr = "constant")
#plot(df_sf)

## Read PM2.5 values ----------------------------------------------------------
pm_25 <- list.files(pm_in,
                    pattern = '*.csv',
                    full.names = TRUE)

pm.all = do.call(rbind, lapply(pm_25,
                               function(x) read.csv(x)))

# join table
pm2.5_sf <- merge(x = df_sf, y = pm.all, by = "GridID")
#head(pm2.5_sf)
#plot(pm2.5_sf)

# Rasterize
pm2.5_sf_proj <- st_transform(pm2.5_sf, 32650) # transform to epsg:3395

# create raster template 
r_template <- rast(ext(pm2.5_sf_proj),
                   resolution = 1000,
                   crs = crs(pm2.5_sf_proj))

# rasterize pm2.5
pm2.5_r <- rasterize(pm2.5_sf_proj,
                     r_template,
                     field = "PM2.5")
# plot(pm2.5_r)

pm2.5_xiamen <- crop(pm2.5_r, city_32650, mask=TRUE)

pm_xiamen_re <- project(
  pm2.5_xiamen,
  "epsg:32650",
  res=1000
)

names(pm_xiamen_re) <- paste0("pm25")
  
# plot(pm2.5_xiamen, type="interval")
# plot(border, add=TRUE)

## Write to file
writeRaster(pm_xiamen_re,
            pm_out,
            overwrite = TRUE)
