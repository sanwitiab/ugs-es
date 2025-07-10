library(landscapemetrics)
library(terra)
library(tidyverse)
library(tidyr)
library(ggspatial)
library(tidyterra)
library(flextable)

source("code/helper.R") # call function for normalization

# Data pre-processing ---------------------------------------------------------
  
esa_lc <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_2021.tif")
plot(esa_lc,
     plg=list(
       x="bottom"
     ))

## ESA lu class
lu_class <- c(
  10, 10, 1,  # Tree
  20, 20, 2,  # Shrub
  30, 30, 3,  # Grass
  40, 40, 4,  # Crop
  50, 50, 5,  # Built-up
  60, 60, 6,  # Bare/Sparse Vegetation
  70, 70, 7,  # Snow & ice
  80, 80, 8,  # Permanent water bodies
  90, 90, 9,  # Herbaceous wetland
  95, 95, 10,  # Mangrove
  100, 100, 11 # Moss & Lichen
  
)

## create matrix
esa_matrix <- matrix(lu_class, ncol = 3, byrow = TRUE)

## Reclassify lu class
esa_class <- classify(esa_lc, esa_matrix, include.lowest=FALSE, right = TRUE)

## Transform projection
esa_prj_utm <- project(esa_class, "epsg:3395", method = "mode")


# Crop to Xiamen city
lc_xiamen <- crop(esa_prj_utm, built_area, mask=TRUE)

#coltab(lc_xiamen) <- esa_col

## plot with basic terra
plot(city)
plot(lc_xiamen,
     type="class",
     plg=list(
       legend=c("Tree cover", "Shrubland", "Grassland",
                "Cropland", "Built-up", "Bare / sparse vegetation",
                "Permanent water bodies",
                "Herbaceous wetland", "Mangroves"),
       title="LULC",
       x="topright"
     ),add=TRUE)

plot(built_area, add=TRUE)

## plot with ggplot()
lu <- as.factor(lc_xiamen)

esa_cols <- c("#00a000", "#966400", "#ffb400", "#ffff64",
              "#c31400", "#fff5d7", "#0046c8",
              "#00dc82", "#009678", "#ffebaf")

ggplot() +
  geom_spatvector(data = city, fill = NA, color="grey70") +
  geom_spatraster(data = lu, na.rm = TRUE) +
  scale_fill_manual(values = esa_cols, na.value = "transparent",
                    drop = FALSE,
                    labels = c("Tree cover", "Shrubland", "Grassland",
                               "Cropland", "Built-up", "Bare / sparse vegetation",
                               "Permanent water bodies",
                               "Herbaceous wetland", "Mangroves")) +
  labs(fill = "LULC") +
  geom_spatvector(data = built_area_district, fill = NA, color = "black") +
  geom_spatvector_text(data = built_area_district, aes(label = district),
                       nudge_x = 0) +
  
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  
  theme_void()

# select green areas# select green areascols
# clamp
lc_green <- clamp(lc_xiamen, lower=10, upper=30) # group by min & max values
plot(lc_green)

# reclassify
m <- c(
  10, 10, 1,  # Tree
  20, 20, 2,  # Shrub
  30, 30, 3,  # Grass
  40, 40, 4,  # Crop
  50, 50, 5,  # Built-up
  60, 60, 6,  # Bare/Sparse Vegetation
  70, 70, 7,  # Snow & ice
  80, 80, 8,  # Permanent water bodies
  90, 90, 9,  # Herbaceous wetland
  95, 95, 10,  # Mangrove
  100, 100, 11 # Moss & Lichen
)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)

lc_reclass <- classify(lc_xiamen, rclmat, include.lowest=FALSE)

plot(lc_reclass)




ggplot() +
  geom_spatvector(data = city, fill = NA, color="grey70") +
  geom_spatraster(data = as.factor(lc_reclass), na.rm = TRUE) +
  scale_fill_manual(values = esa_cols, na.value = "transparent",
                    drop = FALSE,
                    labels = c("Tree cover", "Shrubland", "Grassland",
                               "Cropland", "Built-up", "Bare / sparse vegetation",
                               "Permanent water bodies",
                               "Herbaceous wetland", "Mangroves")) +
  labs(fill = "LULC") +
  geom_spatvector(data = built_area_district, fill = NA, color = "black") +
  geom_spatvector_text(data = built_area_district, aes(label = district),
                       nudge_x = 0) +
  
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  
  theme_void()

# Check landscapemetric -------------------------------------------------------

list_lsm(
  level = "landscape",
  simplify = TRUE
)


# Landscape matrics -----------------------------------------------------------
check_landscape(lu)

## calculate lsm for the whole landscape
all_landscape = calculate_lsm(esa_prj_utm,
                              what = c(
                                "lsm_l_ent",
                                "lsm_l_condent",
                                "lsm_l_mutinf",
                                "lsm_l_joinent",
                                "lsm_l_relmutinf"
                              ))

all_landscape

## calculate lsm by sub-district
built_landscapes = sample_lsm(lu, built_area,
                              what = c(
                                "lsm_c_pland"
                              ))

built_landscapes

built_lu <- built_landscapes |> 
  mutate(
    lu = case_when(
      class == 10 ~ "tree",
      class == 20 ~ "shrub",
      class == 30 ~ "grass",
      class == 40 ~ "crop",
      class == 50 ~ "builtup",
      class == 60 ~ "bareland",
      class == 80 ~ "water",
      class == 90 ~ "wetland",
      class == 95 ~ "mangrove"
    )
  ) |> 
  group_by(plot_id, lu) |> 
  summarise(pland = sum(value))

built_lu

## calculate lsm by district
district_landscapes = sample_lsm(lu, built_area_district,
                              what = c(
                                "lsm_c_pland"
                              ))

district_landscapes

district_lu <- district_landscapes |> 
  mutate(
    lu = case_when(
      class == 10 ~ "tree",
      class == 20 ~ "shrub",
      class == 30 ~ "grass",
      class == 40 ~ "crop",
      class == 50 ~ "builtup",
      class == 60 ~ "bareland",
      class == 80 ~ "water",
      class == 90 ~ "wetland",
      class == 95 ~ "mangrove"
    )
  ) |> 
  group_by(plot_id, lu) |> 
  summarise(pland = sum(value))

district_lu

## connect values to polygons

district_lu_w = pivot_wider(district_lu, 
                                 id_cols = plot_id,
                                 names_from = lu, 
                                 values_from = pland)

district_lu_areas = cbind(built_area_district, district_lu_w)

## caluclate lu by district
d_plot_id <- as.data.frame(district_lu_areas) |> 
              select(plot_id, district)

cal_lu_district <- merge(x = district_lu, y = d_plot_id, by = "plot_id", all.x = TRUE)
cal_lu_district

cal_lu_district |> 
  group_by(district, lu) |> 
  summarise(pland = sum(round(pland, 2))) |> 
  flextable()


## connect values to polygons

built_landscapes_w = pivot_wider(built_lu, 
                               id_cols = plot_id,
                               names_from = lu, 
                               values_from = pland)

built_lu_areas = cbind(built_area, built_landscapes_w)

plot(built_lu_areas, "tree")



## group data 
ugs_pland <- built_landscapes |> 
  filter(class <= 30) |> 
  group_by(plot_id) |> 
  summarise(pland = sum(value))

ugs_pland['class'] <- 'UGS' # create column with class name

names(ugs_pland)

ugs_pland_w <- pivot_wider(ugs_pland,
                           id_cols = plot_id,
                           names_from = class,
                           values_from = pland)

built_pland = cbind(built_area, ugs_pland_w)

built_pland

plot(built_pland, "UGS")

ggplot() +
  geom_spatvector(data = city, fill = NA, color="grey70") +
  
  geom_spatvector(data = built_pland, aes(fill=UGS), color="grey60") +
  
  geom_spatvector(data = built_area_district, fill = NA, color = "black") +
  
  geom_spatvector_text(data = built_area_district, aes(label = district),
                       color = "black") +
  scale_fill_whitebox_c(palette = "gn_yl",
                        direction = -1) + #reverse color
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "UGS Proportion (%)"
  )
