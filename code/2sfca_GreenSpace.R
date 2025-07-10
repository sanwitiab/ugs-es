## source: https://crd230.github.io/lab5a.html#Floating_Catchment_Area

library(sf)
library(tmap)
library(cppRouting)
library(sp)
library(tidyverse)

library(osmdata)
library(matrixStats)
library(SpatialAcc)

# Read data and pre-processing -----------------------------------------------
county <- st_read("E:/ES_Demand_Supply/data/xiamen/extent/county_population_valid.shp")
plot(county)
glimpse(county)
 
# LULC select residential for origin
# lulc <- st_read("E:/ES_Demand_Supply/data/xiamen/landuse/lu_2020_valid.shp")
# glimpse(lulc)
# colnames(lulc)
# 
# unique(lulc$lu_en)

# UGS
ugs <- st_read("E:/ES_Demand_Supply/data/xiamen/urban_green_space/ugs_xiamen.shp")
plot(st_geometry(ugs))

unique(ugs$leisure)

# plot(lulc[lulc$lu_en == "residential",], max.plot = 1)

# check CRS of two layers
st_crs(county) == st_crs(ugs)

st_crs(county)$proj4string
st_crs(ugs)$proj4string

# transform CRS
# county_prj <- st_transform(county, 4527)
# 
# st_crs(county_prj)$proj4string
# 
# st_crs(county_prj) == st_crs(lulc)

# Create centroid of residential area
# residential <- lulc |> filter(lu_en == "residential")
# plot(residential$geometry)

# residential_cen <- st_centroid(residential)

# creat centroid of ugs
ugs_centroid <- st_centroid(ugs)

ugs_centroid_xm <- st_crop(ugs_centroid, county)

plot(st_geometry(county))
plot(st_geometry(ugs_centroid_xm), add=TRUE)


# plot(county_prj$geometry)
# plot(residential_cen$geometry, add=TRUE)

tm_shape(county_prj) +
  tm_polygons() +
  tm_shape(residential_cen) +
  tm_dots(fill = "red")

# Creat centroid of green areas
green <- lulc |> filter(lu_en == "grassland" | lu_en == "woodland")
# plot(green$geometry)
green_centroid <- st_centroid(green)

# plot(green_centroid)
tm_shape(county_prj) +
  tm_polygons() +
  tm_shape(green_centroid) +
  tm_dots(fill = "green")

# plot grass land and residential centroid
tm_shape(county_prj) +
  tm_polygons() +
  tm_shape(residential_cen) +
  tm_dots(fill = "red") +
  tm_shape(green_centroid) +
  tm_dots(fill = "green")



# Points in polygon # ------------------------------------------------
# create unique ID for each residential
green <- green_centroid |>
		mutate(ID = row_number())

# sum up number of green space within each county
green_agg <- aggregate(green["ID"], county_prj, FUN = "length")

# any county with an NA has 0 resd
green_agg <- green_agg |>
			mutate(ID = replace_na(ID, 0))

# save number of green space within a county to main data object
county_prj <- county_prj |>
			mutate(green = green_agg$ID)

county_prj <- county_prj |>
			mutate(greenperpop = (green/常人口)*10000, na.rm = TRUE)

summary(county_prj$greenperpop)

# create map
tm_shape(county_prj, unit = "m") +
	tm_polygons(col = "greenperpop", style = "jenks", palette = "green",
			border.alpha = 0, title = "Green per\n10k population") +
	tm_scale_bar(position = c("right", "bottom")) +
		tm_layout(main.title = "Green space accessibility in Xiamen City",
				main.title.size = 0.95, frame = FALSE,
				legend.outside = TRUE, legend.outside.position = "right")

# Create county centroid
county.centroids <- st_centroid(county_prj)

tm_shape(county_prj) +
	tm_polygons(col = "blue") +
	tm_shape(county.centroids) +
	tm_dots(col = "red")


# create buffer
county.buff <- st_buffer(county_prj, dist = 4000)
county.buff

# extract 1 county
ex1 <- filter(county.buff, Name == "侨英街道")
ex2 <- filter(county_prj, Name == "侨英街道")

tmap_mode("view")

tm_shape(county_prj) +
		tm_polygons() +
	tm_shape(county.centroids) +
		tm_dots(size = 0.01) +
		tm_shape(ex1) +
		tm_borders(col="red") +
	tm_shape(ex2) +
		tm_dots(col = "red")

# sum up number of residential within buffer
buff.green_agg <- aggregate(green["ID"], county.buff, FUN = "length")

# any county with an NA has 0 resd
buff.green_agg <- buff.green_agg |>
			mutate(ID = replace_na(ID, 0))

# save number of residential within a county to main data object
county.buff <- county.buff |>
			mutate(green1m = buff.green_agg$ID) |>
			dplyr::select(Name, green1m)

county.buff <- county.buff |>
			st_drop_geometry()

county_prj <- county_prj |>
			left_join(county.buff, by = "Name") |>
			mutate(greenbuff1m = (green1m/常人口)*10000)

county_prj |>
	summarize(Mean = mean(green1m, na.rm=TRUE)) |>
	st_drop_geometry()

county_prj |>
	ggplot() +
	geom_histogram(mapping = aes(x=green1m), na.rm=TRUE) +
	xlab("Green space in Xiamen county")

tmap_mode("plot")

tm_shape(county_prj, unit = "m") +
	tm_polygons(col = "greenbuff1m", style = "jenks", palette = "Reds",
			border.alpha = 0, title = "Green per\n10k population") +
	tm_scale_bar(position = c("left", "bottom")) +
		tm_layout(main.title = "Green spatial accessibility in Xiamen City",
				main.title.size = 0.95, frame = FALSE,
				legend.outside = TRUE, legend.outside.position = "right")

# Distance to nearest green # ------------------------------------------------
green.dist <- st_distance(county.centroids, green)

# number of county
dim(county.centroids)

# number of green
dim(green)

# number of county by number of green
dim(green.dist)

county_prj <- county_prj |>
			mutate(greenmin = rowMins(green.dist))

county_prj |>
	summarize("Mean min" = mean(greenmin, na.rm=TRUE),
			"Median Min" = median(greenmin, na.rm=TRUE)) |>
	st_drop_geometry()


county_prj |>
	ggplot() +
	geom_histogram(mapping = aes(x=greenmin), na.rm = TRUE) +
	xlab("Nearest distance to nearest green space")

# Create choropleth map of nearest green distance
tm_shape(county_prj, unit = "m") +
	tm_polygons(col = "greenmin", style = "jenks", palette = "Reds",
			border.alpha = 0, title = "Distance to nearest \ngreen (m)") +
	tm_scale_bar(position = c("right", "bottom")) +
		tm_layout(main.title = "Green spatial accessibility in Xiamen City",
				main.title.size = 0.95, frame = FALSE,
				legend.outside = TRUE, legend.outside.position = "right")

# Floating Catchment Area ------------------------------------------------------
# recreate county_buff to make it amn sf object
county.buff <- st_buffer(county.centroids, dist = 1500) |>
			dplyr::select(Name)

buff.county <- county.centroids |>
			dplyr::select(常人口) |>
			st_join(county.buff) |>
			group_by(Name) |>
			summarize(buffpop = sum(常人口)) |>
			ungroup()

# drop geometry so we join this dataframe bact to county_prj
buff.county <- st_drop_geometry(buff.county)

# create fca varaible
county_prj <- county_prj |>
			left_join(buff.county, by = "Name") |>
			mutate(buffpop = replace_na(buffpop, 0)) |>
			mutate(fca = (green1m/buffpop)*10000)

# Create choropleth map of FCA
tm_shape(county_prj, unit = "m") +
	tm_polygons(col = "fca", style = "jenks", palette = "Reds",
			border.alpha = 0, title = "FCA") +
	tm_scale_bar(position = c("right", "bottom")) +
		tm_layout(main.title = "Green spatial accessibility in Xiamen City",
				main.title.size = 0.95, frame = FALSE,
				legend.outside = TRUE, legend.outside.position = "right")

# Two-step Floating Catchment Area #####------------------------------------
class(green.dist)

# Get coordinates
centroids.coords <- st_coordinates(county.centroids)

green.coords <- st_coordinates(green)

# Calculate Euclidean distance
dist.matrix <- distance(centroids.coords, green.coords, type = "euclidean")
class(dist.matrix)

# Calculate 2SFCA with ac() function
TSFCA <- ac(p = county_prj$常人口,
		n = green$Shape_Leng,
		D = dist.matrix, d0 = 400, family = "2SFCA")

county_prj <- county_prj |>
			mutate(TSFCA = TSFCA)

# Create choropleth map of 2FCA
tm_shape(county_prj, unit = "m") +
	tm_polygons(col = "TSFCA", style = "jenks", palette = "brewer.reds",
			border.alpha = 0, title = "TSFCA") +
	tm_scale_bar(position = c("right", "bottom")) +
  tm_title("Green spatial accessibility in Xiamen City") +
		tm_layout(frame = FALSE,
				legend.outside = TRUE, legend.outside.position = "right")


# Road network by Open street map
road <- st_read("E:/ES_Demand_Supply/data/xiamen/road_osm/highway_xiamen.shp")
plot(road)
