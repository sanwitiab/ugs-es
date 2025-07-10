library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)

source("code/helper.R") # call function for normalization

# Input data ------------------------------------------------------------------

## Xiamen boundary ------------------------------------------------------------
city <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/xiamen_all.shp")

## Xiamen built-up areas ------------------------------------------------------
heat_urban <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area.shp")


## Air temperature ------------------------------------------------------------
t_air <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/intermediate/T_air_xm_jun.tif")

## Calculate heat and thermal index (27)
heat_i <- lapp(t_air, hi, rh= 0.8)
plot(heat_i)

heat_max <- as.vector(global(heat_i, fun=max, na.rm=TRUE)[1,1])

thermal_i <- (heat_i - 27) / (heat_max - 27)

plot(thermal_i)


## Population count -----------------------------------------------------------
pop_dir <- "D:/sanwit/data/worldPop/pop_number_100m/"

pop <- rast(paste0(pop_dir, "chn_ppp_2020_constrained.tif"))

pop_prj <- project(pop, t_air, method = "bilinear", mask = TRUE)

## Calculate Heat exposure ----------------------------------------------------
temp_n <- rast_norm(t_air)
pop_n <- rast_norm(pop_prj)

heat_ex <- (0.5 * pop_n) + (0.5 * thermal_i)
plot(heat_ex)
names(heat_ex) <- paste0("heat_ex")

heat_built <- zonal(heat_ex, heat_urban,
                         fun="mean", as.polygons=TRUE, na.rm=TRUE
                         )
heat_built$h_ex <- lapply(heat_built[,"heat_ex"], vect_norm)

ggplot(data = heat_built) +
  geom_spatvector(data = city, fill = NA) +
  geom_spatvector(aes(fill=h_ex), color="black") +
  scale_fill_whitebox_c(palette = "bl_yl_rd",
                        direction = 1) + #reverse color
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "Heat Exposure"
  )
