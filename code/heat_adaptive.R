library(terra)
library(tidyterra)
library(ggplot2)
library(gstat)

source("code/helper.R") # call function for normalization

hospital <- vect("E:/ES_Demand_Supply/data/xiamen/hospital/hospital.shp")

plot(hospital, "check")

hospital <- hospital |> 
  rename(
    type = "类型",
    name = "名称",
    construction = "建设规",
    district = "行政区"
  )

hospital <- project(hospital, "EPSG:3395")

heat_v <- heat_built_es |> 
              mutate(hosp_count = length(intersect(heat_built_es, hospital)))

# nb_urban <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area.shp")

# Point in polygon
heat_built_es$hosp_count <- extract(heat_built_es, hospital, count=TRUE)

summary(heat_built_es)

heat_v <- heat_built_es |> 
            mutate(hosp_n = ifelse(is.na(hosp_count), 0, hosp_count))

summary(heat_v)

heat_v$hosp_nm <- lapply(heat_v[,12], vect_norm)

plot(heat_v, "hosp_nm")

heat_v$h_vul <- heat_v$h_ex + heat_v$h_sens - heat_v$hosp_nm

plot(heat_v, "h_vul")

heat_v$h_vul_n <- lapply(heat_v[,14], vect_norm)

plot(heat_v,"h_vul_n")

ggplot(data = heat_v) +
  geom_spatvector(data = xm_prj, fill = NA) +
  geom_spatvector(aes(fill=h_vul_n), color="black") +
  scale_fill_whitebox_c(palette = "bl_yl_rd",
                        direction = -1) + #reverse color
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "Heat Adaptive Capacity"
  )

