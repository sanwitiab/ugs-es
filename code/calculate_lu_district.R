# Library
library(sf)         # simple features management
library(tidyverse)  # data science tools
library(terra)      # spatial data analysis
library(tidyterra)  # applied common method from tidyverse to SpatRaster & SpatVector
library(ggspatial)  # spatial data mapping with ggplot2
library(flextable)
library(janitor)
library(landscapemetrics)
# library(patchwork)
# library(cowplot)

source("E:/ES_Demand_Supply/code/helper.R") # call function for normalization

lulc_xm <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_xiamen_32650.tif")

district_prj <- project(built_area_district, "epsg:32650")

## calculate lsm by sub-district
built_landscapes = sample_lsm(lu, 
                              district_prj,
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

## connect values to polygons

district_lu_w = pivot_wider(built_lu, 
                            id_cols = plot_id,
                            names_from = lu, 
                            values_from = pland)

district_lu_areas = cbind(district_prj, district_lu_w)

lulc_district_sum <- district_lu_areas |> 
  group_by(district) |> 
  summarise(
    across(bareland:shrub, ~sum(.x))
            ) |> 
  as.data.frame() |> 
  mutate_if(
    is.numeric, round, digits=2
  )

write_csv(lulc_district_sum,
          "E:/ES_Demand_Supply/doc/lulc_prop.csv",
          na = 'NA')
