library(readr)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)
library(ggplot2)
library(tmap)
library(GGally) # create correlation diagram
## Spatial autocorrelation
library(sf)
library(spdep)
library(sfdep)
library(tidyr)
library(ggspatial)

library(corrplot) # visualization of correlation matrix
library(ggcorrplot)

tmap_options(scale = 0.75)

source("E:/ES_Demand_Supply/code/helper.R") # call file and function for analysis
# Urban green space supply indicators -------------------------------------

## Cooling capacity -------------------------------------------------------
# HMI
hmi <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/hm_xm_jun.tif")

# reproject to epsg:3395
hmi_project <- project(hmi, "EPSG:32650",
                       res=100)

# crop to built area of Xiamen
hmi_urban <- crop(hmi_project, built_32650, mask = TRUE)

# rename with HMI
names(hmi_urban) <- paste0("HMI")

# spatial context analysis

## PM2.5 removal -------------------------------------------------------------

### EVI calculation ----------------------------------------------------------
evi <- rast("E:/ES_Demand_Supply/data/xiamen/lai/EVI.tif")

evi[values(evi) > 0.8] =NA #Remove all values exceeding max threshold.
evi[values(evi) < 0.2] =NA #Remove all values that are assumed to not be vegetation

## PM2.5
pm25 <-  rast("E:/ES_Demand_Supply/data/xiamen/pm2.5/xiamen_pm25/pm25_xm.tif")

## Resample spatial resolution of EVI to match with pm25
evi_resamp <- resample(evi, pm25, method = "average", threads = TRUE)

## Equation to calculate EVI
lai <- (3.618 * evi_resamp - 0.118)
names(lai) <- paste0("lai")

### Calculate pollution removal or flux based on Nowak, 2013 ----------------
# F = Vd * C
# Vd = deposition velocity of the pollutant to leaf surface (mh1)
# C is pollutant concentration (μg m−3)
##  

## source: 1. Nowak, et al. Modeled PM2.5 removal by trees in ten U.S. cities 
## and associated health effects. Environmental Pollution 178, 395–402 (2013).
## cell_area <- cellSize(r_pm, unit="m")
Vd = 0.17 # Deposition velocity (Vd)
Con = pm25  # PM2.5 concentration
s = 0.06  # resuspension rate (%)
t = 365 * 3600 # day of the year multiply by second/day

f_pm = Vd * Con * t * (1-s)
r_pm = (f_pm * lai) / 1000000

plot(r_pm)


## Urban Park Accessibility -----------------------------------------------------

# pai_net <- rast("E:/ES_Demand_Supply/result/park_ai_net.tif")
nature_access <- rast("E:/ES_Demand_Supply/data/NatureAccess/data/output/urban_nature_supply_xm_park.tif")

# plot(pai_net)
plot(nature_access)

## Calculate average of supply per neighborhood -----------------------------
## resample to the same extent
hmi_urban <- resample(hmi_project, nature_access)
pm_urban <- resample(r_pm, nature_access)

## normalized values
# hmi_n <- rast_norm(hmi_urban)
# pm_n <- rast_norm(pm_urban)
# pai_n <- rast_norm(pai_euc)

## merge layer
sup_es <- c(
            hmi_urban, pm_urban, nature_access
            )

names(sup_es) <- paste0(c('hmi', 'pmrem', 'nacc'))

tm_shape(sup_es[[1]]) +
  tm_raster(
    col.scale = tm_scale_continuous(
      values = "brewer.rd_yl_gn",
      midpoint = NA
    ),
    col.legend = tm_legend(
      title = "HMI"
    )
  )

## mask with built-area
sup_es_urban <- mask(sup_es, built_32650)

names(sup_es_urban) <- paste0(c(
  "hmi", "pmr", "nai"
  # "hmi_n", "pmr_n", "spai_n"
))

plot(sup_es_urban, main = names(sup_es_urban))

writeRaster(sup_es_urban,
            "E:/ES_Demand_Supply/result/supply_es.tif",
            overwrite = TRUE)

# Plot to map with ggplot()
#hmi
ggplot() +
  geom_spatraster(data = sup_es[[1]]) +

  scale_fill_viridis_c(option = "D", trans = "sqrt",
                       na.value = "transparent") +
  labs(fill = "HMI",
       title = "(a)") +
  geom_spatvector(data = city_32650,
                  fill = NA,
                  color = "grey70") +
  
  geom_spatvector(data = built_32650,
                  fill = NA,
                  colot = "grey50") +
  
  geom_spatvector(data = built_area_district,
                  fill = NA,
                  color = "black",
                  lwd = 1) +
  annotation_scale(
    location = "br",
    style = 'ticks'
                   ) +
  annotation_north_arrow(
    location = 'tr'
  ) +
  theme_void()

#pm
ggplot() +
  geom_spatraster(data = sup_es_urban[[2]]) +
  
  scale_fill_viridis_c(option = "D", trans = "sqrt",
                       na.value = "transparent") +
  labs(fill = "PM2.5 removal by tree \n(g/m2)") +
  geom_spatvector(data = city_32650,
                  fill = NA,
                  color = "grey70") +
  
  geom_spatvector(data = built_32650,
                  fill = NA,
                  colot = "grey50") +
  
  geom_spatvector(data = built_area_district,
                  fill = NA,
                  color = "black",
                  lwd = 1) +
  theme_void()

#nature access
ggplot() +
  geom_spatraster(data = sup_es_urban[[3]]) +
  
  scale_fill_viridis_c(option = "D", trans = "sqrt",
                       na.value = "transparent") +
  labs(fill = "Nature access \nsquare meters/person") +
  geom_spatvector(data = city_32650,
                  fill = NA,
                  color = "grey70") +
  
  geom_spatvector(data = built_32650,
                  fill = NA,
                  colot = "grey50") +
  
  geom_spatvector(data = built_area_district,
                  fill = NA,
                  color = "black",
                  lwd = 1) +
  theme_void()


# Correlation analysis of Supply --------------------------------------------
### plot corr
hist(sup_es_urban[[2]])

sup_es_n <- lapply(sup_es_urban, rast_norm) |> rast()
pairs(c(sup_es_n[[1]],sup_es_n[[2]], sup_es_n[[3]]))

# compute correlation of all layers
sup_cor <- layerCor(
          sup_es_n,
          "cor",
          w=3,
          use="pairwise.complete.obs"
         )

sup_cor$correlation

corrplot(sup_cor$correlation,
         method = "circle",
         add = F,
         type = "lower",
         order = "AOE",
         hclust.method = "average",
         diag = T,
         col = COL2('RdBu', 10), # RdBu
         tl.srt = 0,
         tl.offset = 1
         )


tm_shape(sup_es_n[[1]]) +
  tm_raster() +
  
tm_shape(built_32650) +
  tm_borders(col = "grey40") +

tm_shape(city_32650) +
  tm_borders(col = "grey80") +

tm_shape(built_area_district) +
  tm_borders(col = "black",
             lwd = 2)


# Supply Sum --------------------------------------------------------------
sup_agg <- app(sup_es_n, fun='sum')
plot(sup_agg)

tm_shape(sup_agg) +
  tm_raster()

### zonal statistics --------------------------------------------------------
built_sup <- zonal(
  sup_es_urban,
  built_32650,
  fun = "mean",
  na.rm = TRUE,
  weights = TRUE,
  as.polygons = TRUE
)

names(built_sup)

plot(built_sup, "hmi")
plot(built_sup, "pmr")
plot(built_sup, "nai")

built_sup <- built_sup |> 
  mutate(
    hmi_n = round(vect_norm(hmi),2),
    pmr_n = round(vect_norm(pmr),2),
    nai_n = round(vect_norm(nai),2),
    sub_total = round(hmi_n + pmr_n + nai_n, 2)
  )

plot(built_sup, "sub_total")

writeVector(
  built_sup,
  "E:/ES_Demand_Supply/result/ugs_supply_index.shp",
  filetype = "ESRI Shapefile",
  overwrite = TRUE
)

# ## Using zonebuilder file
# xm_zone_sup <- zonal(
#   sup_es_urban,
#   vect(xm_zone),
#   fun = "mean",
#   na.rm = TRUE,
#   touches = TRUE,
#   weights = TRUE,
#   as.polygons = TRUE
# )
# 
# plot(xm_zone_sup, "hmi")
# plot(xm_zone_sup, "pmr")
# plot(xm_zone_sup, "spai")

## HMI map --------------------------------------------------------------
hmi_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sup) +
  tm_fill(
    fill = "hmi_n",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      # labels = c("Very low (0.00 - 0.13)",
      #            "Low (0.13 - 0.35)",
      #            "Medium (0.35 - 0.52)",
      #            "High (0.52 - 0.72)",
      #            "Very high (0.72 - 1.00)"
      #           )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
    tm_borders(col="grey60" 
               ,lwd = 1) +
  
  tm_shape(built_area_district) +
    tm_borders(col = "black", lwd = 2) +
  
  tm_title("HMI",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))
  
hmi_map

tmap_save(hmi_map,
          filename = "E:/ES_Demand_Supply/map/hmi_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## PM2.5 removal map --------------------------------------------------------------
pm_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sup) +
  tm_fill(
    fill = "pmr_n",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      labels = c("Very low (0.00 - 0.19)",
                 "Low (0.19 - 0.44)",
                 "Medium (0.44 - 0.61)",
                 "High (0.61 - 0.74)",
                 "Very high (0.74 - 1.00)"
      )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("PM2.5 Removal",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

pm_map

tmap_save(pm_map,
          filename = "E:/ES_Demand_Supply/map/pmr_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## Urban park accessibility ---------------------------------------------------
nai_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sup) +
  tm_fill(
    fill = "nai_n",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      labels = c("Very low (0.00 - 0.07)",
                 "Low (0.07 - 0.17)",
                 "Medium (0.17 - 0.32)",
                 "High (0.32 - 0.62)",
                 "Very high (0.62 - 1.00)"
      )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Urban nature access",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

nai_map

tmap_save(spai_map,
          filename = "E:/ES_Demand_Supply/map/spai_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)


# UGS Demand --------------------------------------------------------------
built_dem <- built_sup

## Heat Exposure-----------------------------------------------------------
t_air <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/intermediate/T_air_xm_jun.tif")

### Calculate heat and thermal index (27)
heat_i <- lapp(t_air, hi, rh= 0.8)
plot(heat_i)

heat_max <- as.vector(global(heat_i, fun=max, na.rm=TRUE)[1,1])

thermal_i <- (heat_i - 27) / (heat_max - 27)

# plot(thermal_i)


### Population count -----------------------------------------------------------
pop_dir <- "E:/ES_Demand_Supply/data/xiamen/world_pop/"

pop <- rast(paste0(pop_dir, "pop_count.tif"))

pop_prj <- project(pop, t_air, method = "bilinear", mask = TRUE,
                   res = 100)

### Calculate Heat exposure ---------------------------------------------------
temp_n <- rast_norm(t_air)
pop_n <- rast_norm(pop_prj)

heat_ex <- (0.5 * pop_n) + (0.5 * thermal_i)
plot(heat_ex)
names(heat_ex) <- paste0("heat_ex")

heat_ex_prj <- project(
  heat_ex,
  "epsg:32650",
  method = "bilinear",
  res = 100
)

### Heat Sensitivity ----------------------------------------------------------

## Pop sensitivity
pop14 <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop10.tif")
pop65 <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop60.tif")
pop_sens <- c(pop14,pop65)

pop_sens <- app(pop_sens, fun=sum)

names(pop_sens) <- paste0("heat_sens")

pop_sens_prj <- project(
  pop_sens,
  heat_ex_prj,
  method = "bilinear",
  res = 100
)

# Heat exposure and Heat sensitivity
# heat_pop <- c(heat_ex_prj, pop_sens_prj)
# plot(heat_pop)

### Heat adaptability ---------------------------------------------------------
hospital <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/hospital_kde.tif")
hospital <- project(
                    hospital, 
                    pop_sens_prj,
                    res = 100
                    )
plot(hospital)
# 
# hospital <- hospital |> 
#   rename(
#     type = "类型",
#     name = "名称",
#     construction = "建设规",
#     district = "行政区"
#   )
# 
# hospital <- project(hospital, "EPSG:32650")

## Count hospital in polygons
# built_dem$heat_adapt <- terra::extract(built_dem,
#                                 hospital,
#                                 count=TRUE)
# 
# built_dem$heat_adapt[is.na(built_dem$heat_adapt)] <- 0

# Heat exposure and Heat sensitivity
heat_vul <- c(heat_ex_prj, pop_sens_prj, hospital)

# rescale all layer with normalized function
heat_vul_n <- lapply(heat_vul, rast_norm) |> rast()
plot(heat_vul_n)

## Heat vulnerability -------------------------------------------------------
h_dem <- heat_vul_n[[1]] + heat_vul_n[[2]] - heat_vul_n[[3]]
names(h_dem) <- paste0("heat_dem")
plot(h_dem)

built_sd <- zonal(
  h_dem,
  built_dem,
  fun = "mean",
  na.rm = TRUE,
  weights = TRUE,
  as.polygons = TRUE
)

head(built_sd)

# built_sd <- built_sd |> 
#   mutate(
#     h_exp = round(vect_norm(heat_ex),2),
#     h_sens = round(vect_norm(heat_sens),2),
#     h_adpt = round(vect_norm(heat_adapt),2),
#     heat_vul = h_exp + h_sens - h_adpt,
#     heat_vul_n = round(vect_norm(heat_vul),2)
#   )
# 
# head(built_sd)

plot(built_sd, "heat_dem")

### Heat vulnerability map ---------------------------------------------------
hvi_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sd) +
  tm_fill(
    fill = "heat_dem",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      labels = c("Very low (0.00 - 0.11)",
                 "Low (0.11 - 0.38)",
                 "Medium (0.38 - 0.53)",
                 "High (0.53 - 0.77)",
                 "Very high (0.77 - 1.00)"
      )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("HVI",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

hvi_map

tmap_save(hvi_map,
          filename = "E:/ES_Demand_Supply/map/hvi_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## PM2.5 exposure ------------------------------------------------------------
pop_32650 <- project(pop,
                    "epsg:32650",
                    method="bilinear",
                    res=1000)

names(pop_32650) <- paste0("pop_count")
# total population
global(
  pop_32650,
  fun = "sum",
  na.rm = TRUE
       )

built_sd$area_ha = expanse(built_sd, unit="ha") # calculate area by unit

## calculate cell areas
areas <- cellSize(pop_32650, unit="km")

areas_minmax <- minmax(areas)
areas_minmax[1]

## calculate population density
popDens <- pop_32650/areas_minmax[1]
plot(popDens)

# PM2.5 concentration
pm25
pm_max <- as.vector(global(pm25, fun=max, na.rm=TRUE)[1,1])
pm_con <- (pm25 - 15)/(pm_max - 15)

pm_con_n <- rast_norm(pm_con)
names(pm_con_n) <- paste0("pm_con")
# calculate PM2.5 exposure
## project to the same extent
pm_con <- project(
  pm_con,
  popDens,
  res=1000
)
## merge layers
pm_pop <- c(popDens, pm_con)

## Standardized
pm_pop <- lapply(pm_pop, rast_norm) |> rast()

pm_exp <- pm_pop[[2]] + pm_pop[[1]]
names(pm_exp) <- paste0("d_pm_exp")
# pop count
built_d_pm <- zonal(
  pm_con_n,
  built_dem,
  fun = "mean",
  na.rm = TRUE,
  weights = TRUE,
  touches = TRUE,
  # exact = TRUE,
  # small = TRUE,
  as.polygons = TRUE
)


summary(built_d_pm)
# 
# built_sd = built_sd |> 
#             mutate(
#               pop_dens = pop_count / area_ha,
#               pm25_n = vect_norm(pm25),
#               pop_dens_n = vect_norm(pop_dens),
#               pm_exp = pm25_n + pop_dens_n,
#               pm_exp_n = round(vect_norm(pm_exp),2)
#               )
# 
# names(built_sd)

plot(built_d_pm, "pm_con")

### PM exposure map ---------------------------------------------------
pmexp_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_d_pm) +
  tm_fill(
    fill = "pm_con",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      labels = c("Very low (0.00 - 0.05)",
                 "Low (0.05 - 0.36)",
                 "Medium (0.36 - 0.53)",
                 "High (0.53 - 0.73)",
                 "Very high (0.73 - 1.00)"
      )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("PM2.5 Exp",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

pmexp_map

tmap_save(pmexp_map,
          filename = "E:/ES_Demand_Supply/map/pmexp_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## Park demand index -----------------------------------------------------------
pdi <- rast("E:/ES_Demand_Supply/result/pdi.tif")
plot(pdi)

built_sd <- zonal(
  pdi,
  built_sd,
  fun = "mean",
  na.rm = TRUE,
  weights = TRUE,
  as.polygons = TRUE
)

names(built_sd)

built_sd = built_sd |> 
            mutate(
              pdi_n = round(vect_norm(pdi),2)
            )

plot(built_sd, "pdi_n")

### UGS per capita ----------------------------------------------------------
ugs_pc <- rast("E:/ES_Demand_Supply/data/NatureAccess/data/output/urban_nature_demand_xm_park.tif")

ugs_pc <- crop(
  ugs_pc,
  built_32650,
  mask=TRUE
)

### PM exposure map ---------------------------------------------------
pdi_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sd) +
  tm_fill(
    fill = "pdi_n",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      labels = c("Very low (0.00 - 0.10)",
                 "Low (0.10 - 0.30)",
                 "Medium (0.30 - 0.47)",
                 "High (0.47 - 0.67)",
                 "Very high (0.67 - 1.00)"
      )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("PDI",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

pdi_map

tmap_save(pdi_map,
          filename = "E:/ES_Demand_Supply/map/pdi_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

head(built_sd)



# Mismatches calculation --------------------------------------------------

built_sd_mm <- built_sd |> 
  select(Neigh_ID, district, name_pinyi,
         hmi_n, pmr_n, spai_n,
         heat_vul_n, pm_exp_n, pdi_n) |> 
  rename(
    s_hmi = hmi_n,
    s_pmr = pmr_n,
    s_spai = spai_n,
    d_hvi = heat_vul_n,
    d_pmexp = pm_exp_n,
    d_pdi = pdi_n
  ) |> 
  
  mutate(
    s_agg = s_hmi + s_pmr + s_spai,
    d_agg = d_hvi + d_pmexp + d_pdi,
    
    m_heat = s_hmi - d_hvi,
    m_pm = s_pmr - d_pmexp,
    m_park = s_spai - d_pdi,
    m_agg = m_heat + m_pm + m_park
  )

head(built_sd_mm)
summary(built_sd_mm)

plot(built_sd_mm, "m_agg")

## Mismatched map -----------------------------------------------------------
mm_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sd_mm) +
  tm_fill(
    fill = "m_agg",
    fill.scale = tm_scale_continuous(
      limits = c(-3,3),
      midpoint = 0,
      values = "brewer.rd_yl_bu",
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Mismatches",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

mm_map

tmap_save(mm_map,
          filename = "E:/ES_Demand_Supply/map/mm_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## Supply map -----------------------------------------------------------
s_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sd_mm) +
  tm_fill(
    fill = "s_agg",
    fill.scale = tm_scale_continuous(
      # midpoint = 0,
      values = "-brewer.rd_yl_bu",
      limits = c(0,3)
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Supply aggregate",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

s_map

tmap_save(s_map,
          filename = "E:/ES_Demand_Supply/map/s_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## Demand map -----------------------------------------------------------
d_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sd_mm) +
  tm_fill(
    fill = "d_agg",
    fill.scale = tm_scale_continuous(
      # midpoint = 0,
      values = "-brewer.rd_yl_bu",
      limits = c(0,3)
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Demand aggregate",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

d_map

tmap_save(d_map,
          filename = "E:/ES_Demand_Supply/map/d_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## Heat mm map -----------------------------------------------------------
hmm_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sd_mm) +
  tm_fill(
    fill = "m_heat",
    fill.scale = tm_scale_continuous(
      # midpoint = 0,
      values = "brewer.rd_yl_bu",
      limits = c(-1,1)
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Temperature mismatches",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

hmm_map

tmap_save(hmm_map,
          filename = "E:/ES_Demand_Supply/map/hmm_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## PM mm map -----------------------------------------------------------
pm_mm_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sd_mm) +
  tm_fill(
    fill = "m_pm",
    fill.scale = tm_scale_continuous(
      # midpoint = 0,
      values = "brewer.rd_yl_bu",
      limits = c(-1,1)
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Air purification mismatches",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

pm_mm_map

tmap_save(hmm_map,
          filename = "E:/ES_Demand_Supply/map/hmm_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

## Statistics ----------------------------------------------------------------
# library(NbClust)
library(tidyrgeoda)

names(built_sd_mm)

ggcorr(
  built_sd_mm[,c(4:9)],
  method = c("pairwise", "spearman"),
  label = TRUE,
  label_alpha = TRUE
)

ggcorr(
  built_sd_mm[,c(12:15)],
  method = c("pairwise", "spearman"),
  label = TRUE,
  label_alpha = TRUE
)

# LISA
built_sd_mm_sf <- st_as_sf(built_sd_mm)

mm_lisa = built_sd_mm_sf |> 
  mutate(
    lisa = st_local_moran(built_sd_mm_sf,
                          'm_agg',
                          wt = st_weights(
                            built_sd_mm_sf,
                            'contiguity',
                            queen = TRUE
                          )
                          
    ),
    gstar = st_local_gstar(built_sd_mm_sf,
                           'm_agg'),
    h_bi = st_local_bimoran(
      built_sd_mm_sf,
      c('s_hmi', 'd_hvi')
    ),
    
    pm_bi = st_local_bimoran(
      built_sd_mm_sf,
      c('s_pmr', 'd_pmexp')
    ),
    
    pdi_bi = st_local_bimoran(
      built_sd_mm_sf,
      c('s_spai', 'd_pdi')
    ),
    
    agg_bi = st_local_bimoran(
      built_sd_mm_sf,
      c('s_agg','d_agg')
    )
  ) |> 
  select(name_pinyi, h_bi, pm_bi, pdi_bi, agg_bi)


ggplot(data = mm_lisa) +
  geom_sf(aes(fill = h_bi),lwd = .1,color = 'grey') +
  scale_fill_lisa()

## Aggregate supply-demand mismatch ----------------------------------------
lisa_mm_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
tm_shape(mm_lisa) +
  tm_fill(
    fill = "agg_bi",
    fill.scale = tm_scale(
      n = 6,
      values = c("grey80", "red", "blue",
                 "lightblue", "pink", "white", "grey10")
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Spatial mismatch of supply-demand",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

lisa_mm_map

tmap_save(lisa_mm_map,
          filename = "E:/ES_Demand_Supply/map/lisa_mm_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.3)


## Temperature regulation map ------------------------------------------------
lisa_h_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(mm_lisa) +
  tm_fill(
    fill = "h_bi",
    fill.scale = tm_scale(
      n = 6,
      values = c("grey80", "red", "blue",
                 "lightblue", "pink", "white", "grey10")
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Spatial mismatch of temperature regulation",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

lisa_h_map

tmap_save(lisa_h_map,
          filename = "E:/ES_Demand_Supply/map/lisa_h_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.3)


## Air quality map ------------------------------------------------
lisa_pm_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(mm_lisa) +
  tm_fill(
    fill = "pm_bi",
    fill.scale = tm_scale(
      n = 6,
      values = c("grey80", "red", "blue",
                 "lightblue", "pink", "white", "grey10")
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Spatial mismatch of air purification",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

lisa_pm_map

tmap_save(lisa_pm_map,
          filename = "E:/ES_Demand_Supply/map/lisa_pm_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.3)


## Urban park map ------------------------------------------------
lisa_park_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(mm_lisa) +
  tm_fill(
    fill = "pdi_bi",
    fill.scale = tm_scale(
      n = 6,
      values = c("grey80", "red", "blue",
                 "lightblue", "pink", "white", "grey10")
    ),
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Spatial mismatch of outdoor recreation",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

lisa_park_map

tmap_save(lisa_park_map,
          filename = "E:/ES_Demand_Supply/map/lisa_park_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.3)
