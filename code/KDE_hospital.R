library(readr)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)
library(ggplot2)
library(tmap)
library(spatstat) # Spatial Point Pattern Analysis, Model-Fitting, Simulation, Tests
library(SpatialKDE)
library(raster)

source("E:/ES_Demand_Supply/code/helper.R")


## hospital density
hospital <- st_read("E:/ES_Demand_Supply/data/xiamen/hospital/hospital.shp")

hospital <- st_transform(hospital, "epsg:32650")

hospital <- hospital |> 
                #filter(types == "hospital") |>
		        mutate(
			        utm_e = st_coordinates(geometry)[,1],
			        utm_n = st_coordinates(geometry)[,2]
		        )

summary(hospital)

## Create a point pattern object (ppp)
x <- as.ppp(hospital)
x
plot(x)

### create window from polygon
x$window <-  st_as_sf(built_32650)|>
			st_union() |>
			as.owin()

### Calculate KDE
bb <- st_bbox(st_as_sf(built_32650)) 			# create bounding box
cellsize <- 100 					# raster cell size
##### specify size of raster
height <- (bb$ymax - bb$ymin) / cellsize	
width <- (bb$xmax - bb$xmin) / cellsize

kde <- density(x, sigma = 500, dimyx = c(height, width)) |>
  rast() 
crs(kde) = st_crs(hospital)$wkt  # a ppp has no CRS information so add it

plot(kde)

writeRaster(kde,
		"E:/ES_Demand_Supply/data/xiamen/landuse/hospital_kde.tif"
		)



tm_shape(kde) +
	tm_raster() 

## Kernel density map from points (SpatialKDE)
cell_size = 100
band_width = 150


### Vector
hospital_grid = hospital |>
				create_grid_hexagonal(
					cell_size = cell_size,
					side_offset = band_width
				)

r_kde = hospital |>
		kde(
			band_width = band_width,
			kernel = "quartic",
			grid = hospital_grid
		)

tm_shape(r_kde) +
  tm_polygons(col = "kde_value", palette = "viridis", title = "hospital KDE Estimate") +
  tm_shape(hospital) +
  tm_dots(size = 0.1, col = "red")

### Raster KDE
raster_r <- hospital |>
			create_raster(
				cell_size = cell_size,
				side_offset = band_width
			)

r_kde <- hospital |>
		kde(
			band_width = band_width,
			kernel = "triweight",
			grid = raster_r
		)

r_kde_t <- rast(r_kde)
tm_shape(r_kde) +
	tm_raster(
		col.scale = tm_scale_continuous(
			values = "brewer.rd_yl_gn"
			)
		)
