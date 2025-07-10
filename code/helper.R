library(terra)

# Data input for analysis
city <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/xiamen_all.shp")
city_4326 <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/xiamen__4326.shp")
city_32650 <- project(city_4326, "epsg:32650")

built_area <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area.shp")
built_32650 <- project(built_area, "epsg:32650")

built_area_district <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area_district.shp")
district <- vect("E:/ES_Demand_Supply/data/xiamen/extent/xiamen_district_valid.shp")

# function to normalize raster data
rast_norm <- function(x) {
  val <- values(x)
  
  values(x) <- (val - min(val, na.rm = TRUE)) / (max(val, na.rm = TRUE) - min(val, na.rm = TRUE))
  
  x
}

# function to normalize vector data
vect_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))

}

## Z-Score
z_norm <- function(x) {
  (x - mean(x))/sd(x)
}

# interpolate function
interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

# calcualte heat index based on air temp and relative humidity
hi <- function(x,rh) {
  (-8.784695) + 1.61139411 * x + 2.338549 * rh - 0.14611605 * x * 
    rh - 1.2308094 * 10^-2 * x^2 - 1.6424828 * 10^-2 *
    rh^2 + 2.211732 * 10^-3 * x^2 * rh + 7.2546 * 10^-4 * x *
    rh^2 - 3.582 * 10^-6 * x^2 * rh^2
}

heat_norm <- function(x) {
  val <- values(x)
  
  values(x) <- (val - 27) / (max(val, na.rm = TRUE) - 27)
  
  x
}

calculate_It <- function(x) {
  val <- values(x)
  
  if (values(x) > 27) {
    values(x) <- (val - 27) / (max(val, na.rm = TRUE) - 27)
  } else {
    values(x) <- 0
  }
  return(x)
}



