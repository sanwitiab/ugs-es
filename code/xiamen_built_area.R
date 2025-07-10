# Xiamen built-up areas boundary--------------------------------------------

library(terra)

shp <- list.files("E:\\ES_Demand_Supply\\data\\xiamen\\extent\\XM_builtup",
		pattern="\\.shp$",
		full.names=TRUE)
shp

v <- vect(lapply(shp[2], vect))

plot(v)

built_area <- vect("E:\\ES_Demand_Supply\\data\\xiamen\\extent\\XM_builtup\\建成区街道.shp")

head(built_area)

plot(built_area, "Gini_ad_GE")
