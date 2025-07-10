# Calculate Park Demand Index adopted from 
# 1. Zhou, C.et al. Assessing mini-park installation priority for regreening planning in densely populated cities. Sustainable Cities and Society 67, 102716 (2021).
# Coefficient: 
# Cc = 1.2
# Cy = 1.0
# Ca = 1.0
# Cs = 2.5

library(readr)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)
library(ggplot2)
library(tmap)
library(spatstat) # Spatial Point Pattern Analysis, Model-Fitting, Simulation, Tests
library(SpatialKDE)
library(raster)

source("E:/ES_Demand_Supply/code/helper.R")

# Population count
pop <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_count.tif")
names(pop) <- paste0("pop_count")
pop_utm <- project(pop, "epsg:32650", res=100)

# Pop aged <10
pop_10 <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop10.tif")
names(pop_10) <- paste0("pop_10")
pop10_utm <- project(pop_10, "epsg:32650", res=100)

# Pop aged >=10 - <30
pop_10_30 <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_10_30y.tif")
names(pop_10_30) <- paste0("pop_10_30")
pop_10_30_utm <- project(pop_10_30, "epsg:32650", res=100)

# Pop aged 30 - 60
pop_30_60 <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_30_60y.tif")
names(pop_30_60) <- paste0("pop_30_60")
pop_30_60_utm <- project(pop_30_60, "epsg:32650", res=100)

# Pop age >60
pop_60 <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop60.tif")
names(pop_60) <- paste0("pop_60")
pop_60_utm <- project(pop_60, "epsg:32650", res=100)

# # Population density
# pop_density <- rast("D:/sanwit/data/worldPop/popdensity/chn_pd_2020_1km_UNadj.tif")
# pop_density_crop <- crop(pop_density, city_4326)
# plot(pop_density_crop)

## calculate cell areas
areas <- cellSize(pop_utm, unit="m")

areas_minmax <- minmax(areas)
areas_minmax[1]

## calculate population density
popDens <- pop_utm/areas_minmax[1]
plot(popDens)

## Pop proportion by age
prop_10 <- pop10_utm / pop_utm
prop_10_30 <- pop_10_30_utm / pop_utm
prop_30_60 <- pop_30_60_utm / pop_utm
prop_60 <- pop_60_utm / pop_utm

plot(prop_10, type="interval")

## Residential density
r_kde <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/residential_kde.tif")

r_kde <- resample(r_kde, pop_utm, method = "bilinear")
names(r_kde) <-paste0("pdi")
r_dens <- rast_norm(r_kde)
plot(r_dens)

# Park demand index
# PDI <- Di * (Ce * Pes + Cy * Pyi + Ca * Pai + Cn * Psi)

Cc = 1.2
Cy = 1.0
Ca = 1.0
Cs = 2.5

pdi = r_dens * (Cc * prop_10 + Cy * prop_10_30 + Ca * prop_30_60 + Cs * prop_60)
plot(pdi)

tm_shape(pdi) +
	tm_raster(
		col.scale = tm_scale_continuous(
			values = "viridis",
			midpoint = NA
		)
	) +

tm_shape(city) +
	tm_borders(col = "grey70") +

tm_shape(built_area) +
	tm_borders(col = "black")

writeRaster(pdi,
            "E:/ES_Demand_Supply/result/pdi.tif",
            overwrite=TRUE)

# Extract to neighborhood level ----------------------------------------------------------------------
built_pdi <- zonal(pdi, built_32650, "mean", na.rm = TRUE, as.polygon = TRUE)
built_pdi$pdi_r <- round(built_pdi$pdi, 2)
plot(built_pdi, "pdi_r", type="interval")


# Plot PDI map ---------------------------------------------------------------------------------------
pdi_urban = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
tm_shape(built_pdi) +
  tm_polygons(
    fill = "pdi_r",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 5,
      values = "-brewer.rd_yl_bu",
	labels = c("very low (0.07 - 0.13)",
                 "low (0.13 - 0.25)",
                 "medium (0.25 - 0.34)",
                 "high (0.34 - 0.51)",
                 "very high (0.51 - 0.73)")

    ),
    col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.6) +

tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +

tm_title("Park Demand Index",
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
              
pdi_urban

tmap_save(pdi_urban,
          filename = "E:/ES_Demand_Supply/map/pdi_urban.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)


