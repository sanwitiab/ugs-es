# Hotspot analysis --------------------------------------------------------

library(sf)
library(sfdep)
library(spdep)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)


recreation <- st_read("E:/ES_Demand_Supply/data/xiamen/result/recreation/recreation_missmatch.shp")
head(recreation)

recreation_hotspot <- recreation |> 
  select(zone, Name, 区县, 常人口, re_mm, recre_sup,
         recre_dem) |> 
  drop_na()

# recreation mismatch
hist(recreation_hotspot$recre_sup, main = "Distribution of Recreation supply",
     xlab="Green areas")

hist(recreation_hotspot$recre_dem, main = "Distribution of Recreation demand",
     xlab="Green areas")

# Visualize across neighborhoods
ggplot(recreation_hotspot) +
  geom_sf(aes(fill=recre_sup), color= "black", lwd=0.15) +
  scale_fill_gradient(name="Recreation supply score",
                      low="white",
                      high = "darkgreen") +
  ggtitle("Recreation supply scores of Xiamen City") +
  labs(caption = "Data source: NDVI from Sentinel-2") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")

ggplot(recreation_hotspot) +
  geom_sf(aes(fill=recre_dem), color= "black", lwd=0.15) +
  scale_fill_gradient(name="Recreation demand score",
                      low="white",
                      high = "darkgreen") +
  ggtitle("Recreation demand scores of Xiamen City") +
  labs(caption = "Data source: NDVI from Sentinel-2") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")

# check empty neighbor sets -----------------------------------------------

# create a neighbor list  based on queen contiguity
list_nb <- poly2nb(recreation_hotspot, queen = TRUE)

# Check for empty neighbor sets
# card() calculates number of neighbors for each polygon in the list
# which() finds polygons with 0 neighbors
empty_nb <- which(card(list_nb) == 0)
empty_nb  

# Remove polygons with empty neighbor sets from the data
tes_subset <- recreation_hotspot[-empty_nb, ]

# Subset 'tes_data' to extract polygons with empty neighbor sets
empty_polygons <- recreation_hotspot[empty_nb, ]
empty_polygons$Name  # print neighborhood names



# Global G Test -----------------------------------------------------------

# Test for global spatial autocorrelation
# Now that we removed empty neighbor sets (tes_subset)
# Identify neighbors with queen contiguity (edge/vertex touching)
tes_nb <- poly2nb(tes_subset, queen = TRUE)

# Binary weighting assigns a weight of 1 to all neighboring features 
# and a weight of 0 to all other features
tes_w_binary <- nb2listw(tes_nb, style="B")

# Calculate spatial lag of TreEqty
tes_lag <- lag.listw(tes_w_binary, tes_subset$recre_sup)

# Test for global G statistic of TreEqty
globalG.test(tes_subset$recre_sup, tes_w_binary)


# Local Gi Test -----------------------------------------------------------

# Identify neighbors, create weights, calculate spatial lag
tes_nbs <- tes_subset |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tes_lag = st_lag(recre_dem, nb, wt)    # calculate spatial lag of TreEqty
  )

# Calculate the Gi using local_g_perm
tes_hot_spots <- tes_nbs |> 
  mutate(
    Gi = local_g_perm(recre_dem, nb, wt, nsim = 999)
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
    title = "Recreation services Equity Hot Spots in Xiamen"
  )
sessionInfo()
