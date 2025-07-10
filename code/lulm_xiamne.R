#### Land use/Land Management of Xiamen ####

library(terra)
library(sf)

lulm <- vect("E:/ES_Demand_Supply/data/xiamen/landuse/lu_single.shp")

lulm
head(lulm)

plot(lulm, "一级地", border = NULL)

#### Data cleaning ####

summary(is.valid(lulm))

## FillHold
lulm_f <- fillHoles(lulm)
lulm_f

summary(is.valid(lulm_agg))
## Aggregate/dissolve
lulm_f
lulm_agg <- aggregate(lulm_f, "一级地")

plot(lulm_agg, "一级地", border = NULL)
