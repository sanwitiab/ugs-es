## ESA Land use data pre-processing
## source: https://esa-worldcover.org/en

# helper ------------------------------------------------------------------


cols <- data.frame(value=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100),
                   col=c("#00a000", "#966400", "#ffb400", "#ffff64",
                         "#c31400", "#fff5d7", "#ffffff", "#0046c8",
                         "#00dc82", "#009678", "#ffebaf"),
                   label=c("Tree cover", #10
                           "Shrubland",  #20
                           "Grassland",  #30
                           "Cropland",   #40
                           "Built-up",   #50
                           "Bare / sparse vegetation", #60
                           "Snow and ice", #70
                           "Permanent water bodies", #80
                           "Herbaceous wetland", #90
                           "Mangroves", #95
                           "Moss and lichen") #100
                   )
lu_dir <- "E:/ES_Demand_Supply/data/xiamen/landuse/"

# library -----------------------------------------------------------------


library(terra)
library(tidyterra)
library(dplyr)

source("E:/ES_Demand_Supply/code/helper.R")

# data --------------------------------------------------------------------

## LULC
in_lu <- "D:/sanwit/data/ESA_Landuse/terrascope_download_20250228_165537/WORLDCOVER/ESA_WORLDCOVER_10M_2021_V200/MAP/ESA_WorldCover_10m_2021_v200_N24E117_Map/"

esa_lu_2021_10m <- rast(paste0(in_lu, "ESA_WorldCover_10m_2021_v200_N24E117_Map.tif"))

has.colors(esa_lu_2021_10m) # check color table
esa_col <- coltab(esa_lu_2021_10m) # get color table

lc_xiamen <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_xiamen_32650.tif")

# crop to Xiamen

esa_lc_2021 <- crop(esa_lu_2021_10m, city_4326)
plot(lc_xiamen)
freq(lc_xiamen)
# writeRaster(esa_lc_2021,
#             "E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_2021.tif",
#             datatype="INT8U")

# has.colors(esa_lu_2021_10m)
# tb <- coltab(esa_lu_2021_10m)

# plot(esa_lu_2021_10m)

# transform projection
# lc_xm <- project(esa_lu_2021_10m,
#                   "EPSG:32650",
#                   method="mode",
#                  res=10)


# plot(lc_xm, type="classes")

# Crop to Xiamen city
lc_xiamen <- crop(lc_xm, city_32650, mask=TRUE)

# writeRaster(lc_xiamen,
#             "E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_xiamen_32650.tif",
#             overwrite=TRUE,
#             datatype="INT8U"
#             )

plot(lc_xiamen)
lines(lu_xm_v_built)
plot(built_32650, add=TRUE)

## Populations
pop <- rast("E:/ES_Demand_Supply/data/NatureAccess/data/pop_xm.tif")

pop_32650 <- project(
  pop,
  "epsg:32650",
  method = "bilinear",
  res = 100
)

pop_32650 <- crop(
  pop_32650,
  city_32650,
  mask = TRUE
)

# resample lulc to pop
lulc_pop <- resample(
  lc_xiamen,
  pop,
  method="mode"
)

names(lulc_pop) <- paste0("lulc")


writeRaster(lc_xiamen,
            "E:/ES_Demand_Supply/data/NatureAccess/data/lulc_10.tif",
            overwrite=TRUE,
            datatype="INT1U"
            )

# select green areas
# clamp
lc_green <- clamp(lc_xiamen, 
                  lower=10, 
                  upper=30,
                  values=FALSE) # group by min & max values

plot(lc_green, ext = built_32650)

## Sieve filter
lc_green_park <- sieve(
  lc_green,
  threshold = 40,
  directions = 8
)
plot(lc_green_park, ext = built_32650)

# reclassify
m <- c(
  10, 10, 1,
  30, 30, 3,
  40, 40, 0,
  50, 50, 0,
  60, 60, 0,
  80, 80, 4,
  90, 90, 5
)

rclmat <- matrix(m, ncol = 3, byrow = TRUE)

lc_reclass <- classify(lc_100, 
                       rclmat,
                       include.lowest=TRUE,
                       right=TRUE)

plot(lc_reclass)

lc_clamp <- clamp(
  lc_100,
  10,
  30,
  values=FALSE
)

plot(lc_clamp)

writeRaster(lc_clamp,
            "E:/ES_Demand_Supply/data/NatureAccess/data/lc_clamp.tif",
            overwrite=TRUE,
            datatype="INT1U"
)

# lu xiamen
lu_xm <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/lu_2020.tif")
lu_xm <- project(lu_xm, "EPSG:3395", method="mode")


# landscapemetrics --------------------------------------------------------

library(landscapemetrics)

check_landscape(lc_xiamen)

# calculate total area
lsm_c_ai(lc_xiamen)

# all patch IDs of class 2 with an ENN >2.5
sample_patch <- lc_xiamen |> 
  lsm_p_area() |> 
  dplyr::filter(class == 10 & value > 1) |> 
  dplyr::pull(id)

# show results
sample_patch

# list all available metrics
list_lsm()

# list only aggregation metrics at landscape level and just return function name
list_lsm(level = "landscape",
         type = "aggregation metric",
         simplify = TRUE)

list_lsm(level = "patch",
         type = "core area metric",
         simplify = TRUE)
# combine arguments
list_lsm(level = c("patch", "landscape"),
         type = "core area metric",
         simplify = TRUE)

metrics <- calculate_lsm(lc_xiamen,
                         what = c("patch","class"))
calculate_correlation(metrics, method = "pearson")

# calculate for each land use
lu_xm_v <- vect(paste0(lu_dir,"lu_2020_valid.shp"))
lu_xm_v <- project(lu_xm_v, "EPSG:3395")

lu_xm_v_built <- crop(lu_xm_v, built_xt)

plot(lu_xm_v_built, "lu_en")

# calculate metrics with grids
xm_metrics <- sample_lsm(lc_xiamen,
                         lu_xm_v_built,
                         level = "landscape",
                         metric = "ent")

head(xm_metrics)
summary(xm_metrics)

str(xm_metrics)

xm_metrics |> 
  filter(class >= 10 & class <= 30)

lu_xm_v_built <- bind_cols(lu_xm_v_built, xm_metrics)

# join table
lu_xm_v_built <- merge(x = lu_xm_v_built, y = xm_metrics, 
                  by.x = "OBJECTID",
                  by.y = "plot_id")

glimpse(lu_xm_v_built)

plot(lu_xm_v_built, "value")
