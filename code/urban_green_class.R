# Library
library(sf)         # simple features management
library(tidyverse)  # data science tools
library(terra)      # spatial data analysis
library(tidyterra)  # applied common method from tidyverse to SpatRaster & SpatVector
library(ggspatial)  # spatial data mapping with ggplot2
library(flextable)
library(janitor)
# library(patchwork)
# library(cowplot)

source("E:/ES_Demand_Supply/code/helper.R") # call function for normalization

# Input directory

in_dir <- "E:/ES_Demand_Supply/data/xiamen/urban_green_space/"

ugs <- vect(paste0(in_dir, "ugs_xiamen_cal.shp"))

# classified ugs based on size
# Ref: 

ugs_level <- ugs |> 
  filter(area_ha >= 0.2) |> 
  mutate(
    level = case_when(
      area_ha >= 0.2 & area_ha < 1 ~ "pocket park",
      area_ha >= 1 & area_ha < 5 ~ "neighborhood park",
      area_ha >= 5 & area_ha < 10 ~ "community park",
      area_ha >= 10 & area_ha < 20 ~ "comprehensive park 3",
      area_ha >= 20 & area_ha <= 50 ~ "comprehensive park 2",
      area_ha > 50 ~ "comprehensive park 1"
    ),
    services = case_when(
      level == "comprehensive park 1" ~ 5000,
      level == "comprehensive park 2" ~ 3000,
      level == "comprehensive park 3" ~ 2000,
      level == "community park" ~ 1000,
      level == "neighborhood park" ~ 500,
      level == "pocket park" ~ 300,
    ),
    types = case_match(
      level,c("comprehensive park 1", "comprehensive park 2",
              "comprehensive park 3") ~ "comprehensive park", .default = level,
    )
  )

ugs_level_prj <- project(ugs_level, "epsg:32650")


urban_park_map = ggplot() +
  geom_spatvector(data = city, fill = NA, color = "grey70") +
  geom_spatvector(data = ugs_level_prj, aes(fill=types), color = NA) +
  scale_fill_viridis_d(option = "H", direction = 1) +
  geom_spatvector(data = built_area, fill = NA, color = "grey60", size = 1) +
  geom_spatvector(data = built_area_district, fill = NA, color = "black") +
  geom_spatvector_text(data = built_area_district, aes(label = district),
                       nudge_x = 0) +
  
  # annotation_north_arrow(location = "tr", which_north = "true") +
  # annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    subtitle = "a) urban park",
    fill = "Urban Park Types"
  )

# ESA Land use map ----------------------------------------------------------

lulc_xm <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_xiamen_32650.tif")

lulc_xm <- crop(lulc_xm, built_32650, mask=TRUE)
## plot with ggplot()
lu <- as.factor(lulc_xm)

esa_cols <- c("#00a000", "#966400", "#ffb400", "#ffff64",
              "#c31400", "#fff5d7", "#0046c8",
              "#00dc82", "#009678", "#ffebaf")

lulc_map = ggplot() +
  geom_spatvector(data = city, fill = NA, color="grey70") +
  geom_spatraster(data = lu, na.rm = TRUE) +
  scale_fill_manual(values = esa_cols, na.value = "transparent",
                    drop = FALSE,
                    labels = c("Tree cover", "Shrubland", "Grassland",
                               "Cropland", "Built-up", "Bare / sparse vegetation",
                               "Permanent water bodies",
                               "Herbaceous wetland", "Mangroves")
  ) +
  labs(subtitle = "b) land use/land cover", 
       fill = "LULC") +
  geom_spatvector(data = built_area_district, fill = NA, color = "black") +
  geom_spatvector_text(data = built_area_district, aes(label = district),
                       nudge_x = 0) +
  
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  
  theme_void()


# Combine map with patchwork package ----------------------------------------
# ugs_map = urban_park_map + lulc_map
# 
# plot(ugs_map)

plot(urban_park_map)
plot(lulc_map)
