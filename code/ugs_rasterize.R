library(terra)

source("E:/ES_Demand_Supply/code/helper.R")

ugs <- vect("E:/ES_Demand_Supply/result/urban_park_standard.shp")
ugs_prj <- project(
  ugs,
  "epsg:32650"
)

pop <- rast("E:/ES_Demand_Supply/data/NatureAccess/data/pop_xm.tif")
lulc <- rast("E:/ES_Demand_Supply/data/NatureAccess/data/lulc_10.tif")

# Select UGS: Tree, Grass, Schrub
freq(lulc)
lulc_clamp <- clamp(lulc,
                    10,
                    30,
                    values = FALSE)
plot(lulc_clamp)

# replace values
values(lulc_clamp) <- ugs_prj$code
freq(lulc_clamp)
plot(lulc_clamp)

# Distance on a grid
pop_ugs_distance <- gridDist(
  lulc_clamp,
  10,
  scale = 1200
)

plot(pop_ugs_distance)

head(ugs_prj)

up_xm <- rasterize(
  ugs_prj,
  lulc,
  field = "code",
  update = TRUE
)

plot(up_xm)

writeRaster(up_xm,
            "E:/ES_Demand_Supply/data/NatureAccess/data/urban_park.tif",
            overwrite=TRUE,
            datatype="INT1U"
)
