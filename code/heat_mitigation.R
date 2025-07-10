library(readr)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)
library(ggplot2)
library(tmap)

tmap_options(scale = 0.75)

source("code/helper.R") # call function for normalization


# HMI
hmi <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/hm_xm_jun.tif")

# reproject to epsg:3395
# hmi_project <- project(hmi, "EPSG:3395")

# crop to built area of Xiamen
hmi_urban <- crop(hmi_project, built_area, mask = TRUE)

# rename with HMI
names(hmi_urban) <- paste0("HMI")

# Plot map with tmap package
hmi_map = tm_shape(city) +
            tm_borders(col = "grey70", lwd = 1) +
  
          tm_shape(hmi_urban) +
            tm_raster(
              col.scale = tm_scale_continuous(
                values = "matplotlib.viridis" #brewer.rd_yl_gn
              ),
    
            col.legend = (tm_legend(
              title = "HMI",
              title.size = 0.7,
              text.size = 0.7,
              bg.color = "white",
              bg.alpha = 0.7,
              position = tm_pos_out("center", "bottom", align.v = "center"),
              frame = FALSE,
              orientation = "landscape"
            ))
            ) +

          tm_shape(built_area) +
            tm_borders(col = "black", lwd = 1,
                       col_alpha = 1
                       ) +
  
          tm_compass(
            position = c("right","top")
          ) +
  
  
          tm_layout(
            scale = 1.0,
            frame = FALSE
          )

hmi_map

tmap_save(hmi_map,
          filename = "E:/ES_Demand_Supply/map/hmi.jpg",
          width = 2100,
          dpi = 600,
          scale = 0.5
          )


# Supply ----------------------------------------------------------------------
# extract HMI of each district
border_hmi <- zonal(hmi_project, built_area, "mean", na.rm = TRUE, as.polygon = TRUE)
border_hmi$hmi <- round(border_hmi$hm_xm_jun, 2)
plot(border_hmi, "hmi", type="interval")

#apply Min-Max normalization to PM2.5 column
names(border_hmi)
border_hmi$hmi_n <- lapply(border_hmi["hmi"], vect_norm)
plot(border_hmi, "hmi_n", type="interval")

# Plot HMI of sub-district ----------------------------------------------------
hmi_urban = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
tm_shape(border_hmi) +
  tm_polygons(
    fill = "hmi",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      n = 5,
      values = "-brewer.rd_yl_bu",
      labels = c("very low (0.19 - 0.24)",
                 "low (0.24 - 0.31)",
                 "medium (0.31 - 0.37)",
                 "high (0.37 - 0.44)",
                 "very high (0.44 - 0.54)")
    ),
    col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.65) +

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
              
hmi_urban

tmap_save(hmi_urban,
          filename = "E:/ES_Demand_Supply/map/hmi_urban.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)


# Demand ----------------------------------------------------------------------
## Population
# pop_in <- "D:/sanwit/data/worldPop/age_sex/age_60"
# 
# pop <- list.files(path = pop_in, 
#                            pattern='\\.tif$', 
#                            all.files= T, 
#                            full.names= T)
# 
# pop_age <- rast(pop)

pop_sens = rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_sens.tif")
pop_sens_xm <- crop(pop_sens, city_4326, mask=TRUE)

pop_age_prj <- project(pop_age, "epsg:3395") # transform projection

pop_60_32650 <- project(pop_60, "EPSG:32650")

# extract number of pop >60
border_hmi <- zonal(pop_60_32650, border_hmi, 'median', na.rm = TRUE,
                    as.polygons = TRUE)
plot(border_hmi, "pop_60", type="interval")
border_hmi$pop_60_n <- lapply(border_hmi[,9], min_max_norm)
plot(border_hmi, "pop_60_n", type="interval")

par(mfrow=c(1,2))
m <- c(3.1, 3.1, 2.1, 2.1)
plot(border_hmi, "hm_n", col=c("grey", "darkgrey","lightgreen","green","darkgreen"),
     mar=m,
     plg=list(x="topright"),
     pax=list(las=1),
     main="Heat mitigation")
plot(border_hmi, "pop_60_n", col=c("grey", "darkgrey","orange","red","darkred"),
     mar=m,
     plg=list(x="topright", cex=.75),
     pax=list(las=1),
     main="Population age >60")
dev.off()

par(mfrow=c(1,2))
m <- c(3.1, 3.1, 2.1, 2.1)
plot(border_hmi, "hm_n", col=map.pal("greens", 100),
     type="interval", breaks=5, mar=c(3.1, 3.1, 2.1, 3.1),
     plg=list(x="topright"), main="Heat mitigation")
plot(border_hmi, "pop_60_n", 
     col=map.pal("reds", 100),
     type="interval", breaks=5, mar=c(3.1, 3.1, 2.1, 3.1),
     plg=list(x="topright"), main="Population age >60")
dev.off()

# mismatch
s_max <- max(border_hmi$hm_n, na.rm = T)
d_max <- max(border_hmi$pop_60_n, na.rm = T)

border_hmi$missmatch <- (border_hmi$hm_n - border_hmi$pop_60_n) / ((s_max + d_max) / 2)

values(border_hmi)

plot(border_hmi, "missmatch", col=c("darkred","red", "orange", "yellow", "lightgreen", "green", "darkgreen"), 
     type="interval", breaks=7, plg=list(x="bottomright", cex=.75),
     pax=list(las=1),
     main="Heat mitigation missmatch")
