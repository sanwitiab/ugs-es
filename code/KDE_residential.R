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


## Residential density
residential <- st_read("E:/ES_Demand_Supply/data/xiamen/landuse/resident_cen.shp")

residential <- residential |> 
                #filter(types == "residential") |>
		    mutate(
			utm_e = st_coordinates(geometry)[,1],
			utm_n = st_coordinates(geometry)[,2]
		    )

## Create a point pattern object (ppp)
x <- as.ppp(residential)
x
plot(x)

### create window from polygon
x$window <-  st_as_sf(city_utm)|>
			st_union() |>
			as.owin()

### Calculate KDE
bb <- st_bbox(st_as_sf(city_utm)) 			# create bounding box
cellsize <- 100 					# raster cell size
##### specify size of raster
height <- (bb$ymax - bb$ymin) / cellsize	
width <- (bb$xmax - bb$xmin) / cellsize

kde <- density(x, sigma = 500, dimyx = c(height, width)) |>
  rast() 
crs(kde) = st_crs(residential)$wkt  # a ppp has no CRS information so add it

plot(kde)

writeRaster(kde,
		"E:/ES_Demand_Supply/data/xiamen/landuse/residential_kde.tif"
		)



tm_shape(kde) +
	tm_raster() 

## Kernel density map from points (SpatialKDE)
cell_size = 100
band_width = 150


### Vector
residential_grid = residential |>
				create_grid_hexagonal(
					cell_size = cell_size,
					side_offset = band_width
				)

r_kde = residential |>
		kde(
			band_width = band_width,
			kernel = "quartic",
			grid = residential_grid
		)

tm_shape(r_kde) +
  tm_polygons(col = "kde_value", palette = "viridis", title = "Residential KDE Estimate") +
  tm_shape(residential) +
  tm_dots(size = 0.1, col = "red")

### Raster KDE
raster_r <- residential |>
			create_raster(
				cell_size = cell_size,
				side_offset = band_width
			)

r_kde <- residential |>
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
