library(terra)
library(tidyterra)
library(ggspatial)

source("code/helper.R")

# input ----------------------------------------------------------------------
## built areas
heat_urban <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area.shp")

## Sensitivity group of population
pop_sens <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_sens.tif")

pop_sens_xm <- project(pop_sens, t_air, method = "bilinear", mask = TRUE)
plot(pop_sens_xm)

pop_sens_n <- rast_norm(pop_sens_xm)
plot(pop_sens_n)


heat_built_es <- zonal(pop_sens_n, heat_built,
                    fun="sum", as.polygons=TRUE, na.rm=TRUE
)

heat_built_es$h_sens <- lapply(heat_built_es[,9], vect_norm)

summary(heat_built_es)

ggplot(data = heat_built_es) +
  geom_spatvector(data = xm_prj, fill = NA) +
  geom_spatvector(aes(fill=h_sens), color="black") +
  scale_fill_whitebox_c(palette = "bl_yl_rd",
                        direction = 1) + #reverse color
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "Heat Sensitivity"
  )
