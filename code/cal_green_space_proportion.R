library(terra)

# function
#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

lcz <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/xm_lcz.tif")

lcz
plot(lcz[[1]])

canopy <- rast("E:/ES_Demand_Supply/data/xiamen/eth_canopy_height_10m/xm_canopy.tif")
plot(canopy)
# remove conopy < 2 meters
canopy2m <- canopy
canopy2m[canopy2m < 2] <- NA

plot(canopy2m)

# project and crop raster
lcz_crop <- project(lcz[[1]], canopy2m, method = "mode")
plot(lcz_crop)
#calculate area of regions under 0 m asl
#get sizes of all cells under 0 m
cell_size <- cellSize(canopy2m, unit = "m", transform = TRUE)

canopy2m
lcz_crop

green_prop <- zonal(canopy2m, lcz_crop, fun="mean", na.rm=TRUE)

green_prop_norm <- as.data.frame(lapply(green_prop[2], min_max_norm))
round(green_prop_norm,2)
