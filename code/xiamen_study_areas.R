
# Xiamen City Study areas -------------------------------------------------

library(terra)
library(tidyterra)
library(tidyverse)
library(ggplot2)
library(scales) # Additional library for labels
library(grid)
library(gt)
library(ggspatial) # Spatial data plus the power of the ggplot2 framework means easier mapping.

library(cowplot)


in_file <- "F:/Huang_Data/mxd路径阈值法/SHP/"

list.files(in_file, pattern = '\\.shp$')

cn_area <- vect(paste0(in_file, "boundary_cn.shp"))
province <- vect("F:/Huang_Data/mxd路径阈值法/省级边界/CN-sheng-A.shp")
city <- vect("F:/Huang_Data/mxd路径阈值法/地级市/CN-shi-A.shp")
county <- vect(paste0(in_file, "county.shp"))

# transform projection
province <- project(province, cn_area)
city <- project(city, cn_area)

# create centroid of Xiamen
city_centroid <- centroids(city)

xiamen <- city_centroid |> 
  filter(CityNameC == "厦门市")


# plot map
# plot(cn_area)
# lines(province, lwd=0.05)
# points(xiamen, col = "red", cex=0.7, pch=20)

# subplot
subplot <- ggplot(data = cn_area) +
  geom_spatvector(color="black") +
  geom_spatvector(data = province, fill=NA) +
  geom_spatvector(data = xiamen, color="red") +
  theme_void() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    plot.background = element_rect(fill = "grey95")
  )

# subplot

# mainplot
in_main <- "E:/ES_Demand_Supply/data/xiamen/extent/"
in_dem <- "E:/ES_Demand_Supply/data/xiamen/dem/"

# dem
dem <- rast(paste0(in_dem, "dem.tif"))
dem_prj <- project(dem, cn_area)

# district
xiamen_main <- vect(paste0(in_main, "xiamen_district_valid.shp"))
#xiamen_main <- project(xiamen_main, cn_area)

urban_zone <- vect(paste0(in_main, "xiamen_urban_zone.shp"))
urban_zone_agg <- aggregate(urban_zone, "zone")
urban_zone_fill <- fillHoles(urban_zone_agg)
urban_area <- urban_zone_fill |> filter(zone == "urban area")

# pre-processing
#xiamen_dist <- aggregate(xiamen_main, "区县") # dissolve by name
xiamen_dist <- fillHoles(xiamen_main)
# plot(xiamen_dist)

dem_xm <- crop(dem, xiamen_dist, mask=TRUE)



inset <- cn_area |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = province) +
  geom_sf(data = xiamen, color="red") +
  labs(x = NULL, y = NULL) +
  theme_test() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.title=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"))

# inset

# xm <- xiamen_dist |> 
#   ggplot() +
#   geom_raster(data = dem_xm,aes(x = x, y = y, fill = dem), na.rm = TRUE) +
#   scale_fill_viridis_c(option = "D",
# 				na.value = "transparent") +
#   geom_sf(fill=NA) +
#   labs(x = NULL, y = NULL) +
#   geom_sf_text(aes(label=dist_name), color = "white") +
#   theme_test() +
#   theme(panel.background = element_rect(fill = "white"),
# 		legend.position = "bottom") +
#   annotation_north_arrow(location = "tr", which_north = "true") +
#   annotation_scale(location = "bl")
# 
# xm
# 
# # Combining both maps
# print(inset, vp = viewport(0.65, 0.35, width = 0.225, height = 0.225))

xm_study <- ggplot() +
		geom_raster(data = dem_xm, aes(x = x, y = y, fill = dem), na.rm = TRUE) +
		scale_fill_viridis_c(option = "D",
				na.value = "transparent") +
    labs(fill = "DEM (m)") +

		geom_sf(data = xiamen_dist,fill=NA, color="grey", show.legend = T) +
		geom_sf(data = urban_area, mapping = aes(color=zone), alpha = 0, lwd = 0.2) +
    # labs(fill = "NA") +
		geom_sf_text(data= xiamen_dist, aes(label=dist_name), color="white") +
		theme_void() +
		annotation_north_arrow(location = "tr", which_north = "true") +
  		annotation_scale(style = "ticks", location = "bl") +
		theme(legen.position = "bottom") +
		labs(x = NULL, y = NULL)


# print(inset, vp = viewport(0.6, 0.125, width = 0.210, height = 0.210))

xm_study_map <- ggdraw()+
  draw_plot(xm_study)+
  draw_plot(inset,height=0.210,x=0.15,y=0.05)

plot(xm_study_map)

# ggsave(
#   "xm_study_map.png",
#   device = "png",
#   path = "E:/ES_Demand_Supply/map",
#   dpi = 600
# )

# ggplot() +
# 	geom_sf(data = urban_area,
# 			mapping = aes(fill=zone)) +
# 	scale_fill_manual(values = "grey90") +
# 	geom_sf(data = xiamen_dist, fill=NA) +
# 	geom_sf_text(data = xiamen_dist, aes(label=dist_name)) +
# 	labs(x = NULL, y = NULL) +
# 	theme_test() +
# 	annotation_north_arrow(location = "tr", which_north = "true") +
#   	annotation_scale(location = "bl") +
# 	theme(legend.position='inside',
# 		legend.position.inside = c(.825, .1)) # legend.position = "none"
# 				
