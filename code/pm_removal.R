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

tmap_options(scale = 0.75)

source("code/helper.R") # call function for normalization

# Data used ------------------------------------------------------------------

## EVI
evi <- rast("E:/ES_Demand_Supply/data/xiamen/lai/EVI.tif")

evi[values(evi) > 0.8] =NA #Remove all values exceeding max threshold.
evi[values(evi) < 0.2] =NA #Remove all values that are assumed to not be vegetation
evi <- project(
  evi,
  "epsg:3395",
  method = "cubic",
  res=10
)
## PM2.5
pm25 <-  rast("E:/ES_Demand_Supply/data/xiamen/pm2.5/xiamen_pm25/pm25_xm.tif")

## Plot LAI built areas ------------------------------------------------------
evi_resamp <- resample(evi, pm25, method = "average", threads = TRUE)

lai <- (3.618 * evi_resamp - 0.118)
names(lai) <- paste0("lai")


tm_shape(lai) +
  tm_raster(
    col.scale = tm_scale_continuous(
      values = "-brewer.rd_yl_gn",
      midpoint = NA
    ),
    
    col.legend = tm_legend(
      title = "LAI",
      bg.color = "white",
      bg.alpha = 0.7,
      position = tm_pos_in("left", "top")
    )
  ) +
  
tm_shape(built_32650) +
  tm_borders(col = "black") +

tm_compass() +
  
tm_layout(
  scale = 1.0
)


### Calculate pollution removal or flux based on Nowak, 2013 ----------------
# F = Vd * C
# Vd = deposition velocity of the pollutant to leaf surface (mh1)
# C is pollutant concentration (μg m−3)
##  

pm_lai <- c(pm25, lai)
x <- focalPairs(pm_lai,
           w = 3,
           fun = "pearson",
           na.rm = TRUE)
plot(x)

## source: 1. Nowak, et al. Modeled PM2.5 removal by trees in ten U.S. cities 
## and associated health effects. Environmental Pollution 178, 395–402 (2013).
## cell_area <- cellSize(r_pm, unit="m")
Vd = 0.17 # Deposition velocity (Vd)
Con = pm25  # PM2.5 concentration
s = 0.06  # resuspension rate (%)
t = 365 * 3600

f_pm = Vd * Con * t * (1-s)
r_pm = (f_pm * lai) / 1000000



plot(r_pm)

plot(built_area, add = T)


names(r_pm) <- paste0("pm_removal")

r_pm_v <- zonal(r_pm, 
              built_area,
              fun="mean", 
              as.polygons=TRUE, 
              na.rm=TRUE,
              weights = FALSE,
              touches = TRUE
              )
r_pm_v$pm_removal_r <- round(r_pm_v$pm_removal, 2)

summary(r_pm_v)

lai_pm <- zonal(lai, r_pm_v, fun="mean", as.polygons=TRUE,na.rm=TRUE)


# ggally: spearman's correlation
names(lai_pm)

ggcorr(
  lai_pm[,c(11:13)],
  method = c("pairwise", "spearman"),
  label = TRUE,
  label_alpha = TRUE
)


# Plot PM2.5 removal by tree -------------------------------------------------

pm_removal_map <- tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
tm_shape(r_pm_v) +
  tm_polygons(
    fill = "pm_removal_r",
    
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      labels = c("very low (3.55 - 3.78)",
                 "low (3.78 - 4.00)",
                 "medium (4.00 - 4.18)",
                 "high (4.18 - 4.34)",
                 "very high (4.34 - 4.55)")
      ),
    col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  tm_text("name_pinyi", size = 0.60) +

tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +

tm_title("PM2.5 Removal (tons/year)",
           size = 1,
           width = 7,
           group_id = "top",
           z = 0
  ) +

tm_layout(frame = FALSE) +

  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

pm_removal_map

tmap_save(pm_removal_map,
          filename = "E:/ES_Demand_Supply/map/pm_removal_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)

# spatial autocorrelation ------------------------------------------------------
## global
autocor(lai)

## local
lai_auto <- autocor(r_pm, w=5, "moran", global=FALSE)
plot(lai_auto)
# Global (Vector) ---------------------------------------------------------
# xm_demand layer must be sf object
## check empty neighbor sets -----------------------------------------------

# create a neighbor list  based on queen contiguity
r_pm_sf <- st_as_sf(r_pm_v)

list_nb <- poly2nb(r_pm_sf, queen = TRUE)

# Check for empty neighbor sets
# card() calculates number of neighbors for each polygon in the list
# which() finds polygons with 0 neighbors
empty_nb <- which(card(list_nb) == 0)
empty_nb  

# Remove polygons with empty neighbor sets from the data
tes_subset <- r_pm_sf[-empty_nb, ]

# Subset 'tes_data' to extract polygons with empty neighbor sets
empty_polygons <- r_pm_sf[empty_nb, ]
empty_polygons$name_pinyi  # print neighborhood names

## Global G Test -----------------------------------------------------------

# Test for global spatial autocorrelation
# Now that we removed empty neighbor sets (tes_subset)
# Identify neighbors with queen contiguity (edge/vertex touching)
tes_nb <- poly2nb(tes_subset, queen = TRUE)

# Binary weighting assigns a weight of 1 to all neighboring features 
# and a weight of 0 to all other features
tes_w_binary <- nb2listw(tes_nb, style="B")

# Calculate spatial lag of TreEqty
tes_lag <- lag.listw(tes_w_binary, tes_subset$pm_removal_r)

# Test for global G statistic of TreEqty
globalG.test(tes_subset$pm_removal_r, tes_w_binary)

## Local Gi Test -----------------------------------------------------------

# Identify neighbors, create weights, calculate spatial lag
names(tes_subset)
tes_nbs <- tes_subset |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tes_lag = st_lag(pm_removal_r, nb, wt)    # calculate spatial lag of TreEqty
  )

# Calculate the Gi using local_g_perm
tes_hot_spots <- tes_nbs |> 
  mutate(
    Gi = local_g_perm(pm_removal_r, nb, wt, nsim = 999)
    # nsim = number of Monte Carlo simulations (999 is default)
  ) |> 
  # The new 'Gi' column itself contains a dataframe 
  # We can't work with that, so we need to 'unnest' it
  unnest(Gi)

# Cursory visualization
# Plot looks at gi values for all locations
tes_hot_spots |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2() # makes the value 0 (random) be the middle

# Create a new data frame called 'tes_hot_spots"
tes_hot_spots |> 
  # with the columns 'gi' and 'p_folded_sim"
  # 'p_folded_sim' is the p-value of a folded permutation test
  select(gi, p_folded_sim) |> 
  mutate(
    # Add a new column called "classification"
    classification = case_when(
      # Classify based on the following criteria:
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # Convert 'classification' into a factor for easier plotting
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")
    )
  ) |> 
  # Visualize the results with ggplot2
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "PM2.5 Removal capacity in Xiamen"
  )
