# Data manipulation --------------------------------------------------
library(sf)
library(dplyr)

in_dir = "D:/sanwit/data/tianditu/" 			# input folder

out_dir = "D:/sanwit/data/tianditu/processed/"		# output folder

province = st_read(paste0(in_dir, "中国_省.geojson"))

head(province)

data1 = province |>
		filter(name != "境界线")

data2 = province |>
		filter(name == "境界线")

write_sf(data1, paste0(out_dir, "province.shp"),
		layer_options = "encoding=utf-8")


write_sf(data2, paste0(out_dir, "省境线.shp"),
		layer_options = "encoding=utf-8")


# Plot map ---------------------------------------------------------
library(RColorBrewer)
library(ggplot2)

proj = "+proj=lcc +lon_1=25 +lat_2=47 +lon_0=110 +lat_0=0 +ellps=GRS80 +units=m +no_defs"

set.seed(123)
data1$index = rpois(34,5)

map = ggplot(data1) +
		geom_sf(aes(fill = index)) +
		geom_sf(data = data2) +
		theme_minimal()
map
map +
  coord_sf(crs = proj) +
  scale_fill_stepsn(colours = brewer.pal(4, "Reds"))


map =  map +
		scale_fill_stepsn(colours = brewer.pal(4, "Reds"))

map

mainmap = map +
		coord_sf(ylim = c(2000000, NA), crs = proj,
			   xlim = c(NA, 2500000))

mainmap

submap = map +
		coord_sf(ylim = c(NA, 2700000), crs = proj,
			   xlim = c(-500000, 1600000)) +
		labs(caption = "南海诸岛") +
		guides(fill = "none") +
		theme(panel.grid = element_blank(),
			axis.text = element_blank(),
			plot.background = element_rect(colour = "black"),
			plot.margin = margin(),
			axis.title = element_blank(),
			axis.tick.length = unit(0, "pt"),
			plot.caption = element_text(size = 12)
			)

g = ggplotGrob(submap)

mainmap +
  annotation_custom(g, xmin = 1500000, ymax =3500000)

