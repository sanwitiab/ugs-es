library(readr)
library(sf)
library(dplyr)
library(terra)
library(ggplot2)

source("code/helper.R") # call function for normalization

# Read tile
tile <- list.files("E:/ES_Demand_Supply/data/xiamen/pm2.5/Tile", pattern = '*.csv',
				full.names = TRUE)
tile

# Read and Combine to one files
df.all = do.call(rbind, lapply(tile, 
                               function(x) read.csv(x)))
# print(df.all)
# str(df.all)

# Convert to SF
df_sf <- st_as_sf(df.all, coords = c("Longitude", "Latitude"),
			crs = 4326, agr = "constant")
# df_sf
#plot(df_sf)

# Read PM2.5 values
pm2.5 <- list.files(
                    "E:/ES_Demand_Supply/data/xiamen/pm2.5/PM2.5",
                    pattern = '*.csv',
                    full.names = TRUE
                    )
#pm2.5

pm.all = do.call(rbind, lapply(pm2.5,
					function(x) read.csv(x)))
head(pm.all)
summary(pm.all)

# join table
pm2.5_sf <- merge(x = df_sf, y = pm.all, by = "GridID")
#head(pm2.5_sf)
#plot(pm2.5_sf)

# Rasterize
pm2.5_sf_32650 <- st_transform(pm2.5_sf, 3395)

r_template <- rast(ext(pm2.5_sf_32650), 
                   resolution = 1000,
                   crs = crs(pm2.5_sf_32650)
                   )

pm2.5_r <- rasterize(pm2.5_sf_32650, 
                     r_template,
                     field = "PM2.5")

names(pm2.5_r) <- paste0("pm25")

#plot(pm2.5_r)

# Xiamen areas
# border <- vect("E:/ES_Demand_Supply/data/xiamen/extent/xiamen_county_fillH.shp")
#border

# border <- project(border, "EPSG:32650")

pm2.5_xiamen <- crop(pm2.5_r, city, mask=TRUE)
plot(pm2.5_xiamen, type="interval")
plot(city, add=TRUE)


# write to file
writeRaster(
  pm2.5_xiamen,
  "E:/ES_Demand_Supply/data/xiamen/pm2.5/xiamen_pm25/pm25_xm.tif",
  datatype="INT8U",
  overwrite = TRUE          
            )


d <- classify(pm2.5_xiamen, c(10,15,20,25))
plot(d, type="classes")

# Extract PM2.5 mean for each county
# border_pm2.5 <- extract(pm2.5_xiamen, border, median, na.rm = TRUE)
built_pm <- zonal(pm2.5_xiamen, built_area, "mean", na.rm = TRUE, as.polygon = TRUE)
#border_pm2.5

plot(built_pm, "pm25", type="interval")

## Plot PM2.5 base on Land use types
# lu <- vect("E:/ES_Demand_Supply/data/xiamen/landuse/lu_2020_valid.shp")
# lu <- project(lu, "EPSG:32650")
# lu_pm2.5 <- zonal(pm2.5_xiamen, lu, "median", na.rm = TRUE, as.polygons = TRUE)
# plot(lu_pm2.5, "last", type = "interval")


# Normalize PM2.5 median
#define Min-Max normalization function
min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

names(border_hmi)
#apply Min-Max normalization to PM2.5 column
border_hmi$pm25_n <- lapply(border_hmi[,12], min_max_norm)
#border_pm2.5

#plot(border_pm2.5, "pm25_n", type="interval")

# Z-Score
#standardize pm2.5 columns of dataset
#standardize Sepal.Width

border_hmi$pm25_z <- (border_hmi$pm25 - mean(border_hmi$pm25)) / sd(border_hmi$pm25)
#border_pm2.5

plot(border_hmi, "pm25_z", type="interval")

# Calculate Demand for PM2.5
border_hmi$pm25_dem <- (border_hmi$pm25_n + border_hmi$人密)/2

names(border_hmi)
border_hmi$pm25_dem_n <- lapply(border_hmi[,15], min_max_norm)
plot(border_hmi, "pm25_dem_n", type="interval")
#border_pm2.5


ggplot(data = st_as_sf(border_hmi)) +
	geom_sf(aes(fill = pm25_dem_n)) +
	scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Calculate Supply (Tree areas)
veg <- rast("E:/Bird/data/xiamen/result/CUGIC_Result/2_Vegetation_Height_Class/vegetation_height_class.tif")

trees <- veg

trees_utm <- project(trees, "EPSG:32650")
#trees_utm

trees_utm[values(trees_utm) < 2] = NA
trees_utm[values(trees_utm) > 2] = NA

#plot(trees_utm)
# calculate area of trees in HA for each sub-district
area_ha <- cellSize(trees_utm, unit="ha", names="ha")

t_area <- length(area_ha)*median(area_ha)
#print(t_area)

trees_pixels <- freq(trees_utm, digits = 0, zones = border)

trees_pixels <- trees_pixels |> 
                  mutate(t_area_ha = count * 0.007754767,
                         pm_removal = t_area_ha * 0.19)

head(trees_pixels)

#summary(trees_pixels)

border$zone <- seq.int(nrow(border)) # add row id
xm_pm_ds <- merge(border, trees_pixels, all.x=TRUE, by.x="zone", by.y="zone")
xm_pm_ds
values(xm_pm_ds)
names(xm_pm_ds)

xm_pm_ds$pm25_sup_n <- lapply(xm_pm_ds[,12], min_max_norm)
#calculate pm2.5 removal by tree
xm_pm_ds$pm_remove_tree <- (xm_pm_ds$pm_removal * xm_pm_ds$pm25)
head(xm_pm_ds)

#apply Min-Max normalization to PM2.5 column
xm_pm_ds$pm25_sup_n <- lapply(xm_pm_ds[,23], min_max_norm)

plot(xm_pm_ds, "pm25_dem_n", type = "interval")
plot(xm_pm_ds, "pm25_sup_n", type = "interval")

ggplot(data = st_as_sf(xm_pm_ds)) +
  geom_sf(aes(fill = pm25_dem_n)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = st_as_sf(xm_pm_ds)) +
  geom_sf(aes(fill = pm25_sup_n)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

par(mfrow=c(1,2))
m <- c(3.1, 3.1, 2.1, 2.1)
plot(xm_pm_ds, "pm25_sup_n", col=c("grey", "darkgrey","lightgreen","green","darkgreen"),
     mar=m,
     plg=list(x="topright"),
     pax=list(las=1),
     main="PM2.5 removal supply")
plot(xm_pm_ds, "pm25_dem_n", col=c("grey", "darkgrey","orange","red","darkred"),
     mar=m,
     plg=list(x="topright", cex=.75),
     pax=list(las=1),
     main="PM2.5 Concentration")
dev.off()

par(mfrow=c(1,2))
m <- c(3.1, 3.1, 2.1, 2.1)
plot(xm_pm_ds, "pm25_sup_n", col=map.pal("greens", 100),
     type="interval", breaks=5, mar=c(3.1, 3.1, 2.1, 3.1),
     plg=list(x="topright"), main="PM2.5 removal supply")
plot(xm_pm_ds, "pm25_dem_n", 
     col=map.pal("reds", 100),
     type="interval", breaks=5, mar=c(3.1, 3.1, 2.1, 3.1),
     plg=list(x="topright"), main="PM2.5 Concentration")
dev.off()

# mismatch
s_max <- max(xm_pm_ds$pm25_sup_n, na.rm = T)
d_max <- max(xm_pm_ds$pm25_dem_n, na.rm = T)

xm_pm_ds$pm25_mm <- (xm_pm_ds$pm25_sup_n - xm_pm_ds$pm25_dem_n) / ((s_max + d_max) / 2)

values(xm_pm_ds)

plot(xm_pm_ds, "pm25_mm", col=c("darkred","red", "orange", "yellow", "lightgreen", "green", "darkgreen"), 
     type="interval", breaks=7, plg=list(x="bottomright", cex=.75),
     pax=list(las=1),
     main="PM2.5 Missmatch")

border_mm <- xm_pm_ds
