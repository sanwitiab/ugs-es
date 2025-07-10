# PM2.5 Exposure

library(terra)
library(tidyterra)
library(ggplot2)
library(tmap)
tmap_options(scale = 0.75)
source("code/helper.R") # call function for normalization

# PM2.5 concectration (annual) -----------------------------------------------

## PM2.5
pm25 <- rast("E:/ES_Demand_Supply/data/xiamen/pm2.5/xiamen_pm25/pm25_xm.tif")

plot(pm25)

built_32650 <- project(built_area, "epsg:32650")

border_pm <- zonal(pm25, built_32650, "mean", na.rm = TRUE, as.polygon = TRUE)


# Plot PM2.5 concentration maps ----------------------------------------------
pm_map <- tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
tm_shape(border_pm) +
  tm_polygons(
    fill = "pm25",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      labels = c("very low (14.95 - 15.61)", 
                 "low (15.61 - 16.44)", 
                 "medium (16.44 - 17.00)", 
                 "high (17.00 - 18.07)", 
                 "very high (18.07 - 19.14)")
    ),
    col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.5) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("PM2.5 Concentration (ug/m3)",
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
          filename = "E:/ES_Demand_Supply/map/pm_map.jpg",
          width = 2100,
          dpi = 600,
          scale = 0.35
)


## Population count
pop_dir <- "D:/sanwit/data/worldPop/pop_number_100m/"

pop <- rast(paste0(pop_dir, "chn_ppp_2020_constrained.tif"))
pop
plot(pop)


# Resample the population data -----------------------------------------------
## convert to same coordinate

pop_prj <- project(pop, pm25, method = "bilinear", mask = TRUE)
plot(pop_prj)

# PM2.5 risk categories ------------------------------------------------------
r_who <- c(0, 5, 1,
       5, 10, 2,
       10, 15, 3,
       15, 25, 4,
       25, 35, 5,
       35, 100, 6)

r_chn <- c(0,15, 0,
           15, 25, 1,
           25, 35, 2,
           35, 100, 3)

rclass <- matrix(r_chn, ncol = 3, byrow = TRUE)

pm_risk <- classify(pm25, rclass, include.lowest=TRUE)
plot(pm_risk)

# Calculate air pollution risk categories to population count ----------------
## normalize raster
pm_n <- rast_norm(pm_risk)
pop_n <- rast_norm(pop_prj)

pollution_pop = pm_n * pop_n
plot(pollution_pop)


# Sum to administrative level ------------------------------------------------
urban <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area.shp")
urban

urban_prj <- project(urban, pm25)

urban_prj$area_ha <- expanse(urban_prj, unit = "ha") # calculcate area in ha

# PM2.5
urban_pm <- zonal(pm_n, urban_prj, fun="mean", exact = FALSE,
                    as.polygons=TRUE, na.rm=TRUE)

# PopDensity
urban_pm <- zonal(pop_prj, urban_pm, fun="sum", exact = FALSE,
                  as.polygons=TRUE, na.rm=TRUE)

# calculate population density
urban_pm$popdens_ha <- urban_pm$chn_ppp_2020_constrained / urban_pm$area_ha

# normalized
urban_pm$pm25N <- lapply(urban_pm[,8], vect_norm)
urban_pm$popdens_n <- lapply(urban_pm[,10], vect_norm)

# PM2.5 exposure

urban_pm$pmexp <- urban_pm$pm25N + urban_pm$popdens_n

urban_pm$pmexp_n <- lapply(urban_pm[,13], vect_norm)


plot(urban_pm, "pmexp_n")

ggplot(data = urban_pm) +
  geom_spatvector(data = xm_prj, fill = NA) +
  geom_spatvector(aes(fill = pmexp_n), color = "black") +
  scale_fill_whitebox_c(palette = "bl_yl_rd",
                        direction = 1) + #reverse color
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "PM2.5 Exposure"
  )
