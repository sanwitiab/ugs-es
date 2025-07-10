library(terra)

abg <- rast("E:/ES_Demand_Supply/data/xiamen/abg/abg_mask.tif")

plot(abg)

abg <- project(abg, "EPSG:32650")

abg_xiamen <- crop(abg, border, mask=TRUE)
plot(abg_xiamen, type="interval")
plot(border, add=TRUE)
