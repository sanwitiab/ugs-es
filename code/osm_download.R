library(osmdata)
library(dplyr)

head(available_features())
available_features()


q <- opq(bbox = 'Xiamen, Fujian') |>
    add_osm_feature(key = 'highway')

osmdata_xml(q, 'xiamen_highway.osm')

names(sf::st_read('xiamen_highway.osm', layer = 'lines', quiet = TRUE))

names(osmdata_sf (q, 'xiamen_highway.osm')$osm_lines)

highway <- sf::st_read ('xiamen_highway.osm', layer = 'lines', quiet = TRUE)
highway
plot(highway$geometry)


st_write(highway, "E:/ES_Demand_Supply/data/xiamen/road_osm/highway_xiamen.shp")

