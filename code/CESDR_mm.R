# missmatch indicators
library(sf)
library(sfdep)
library(spdep)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)
library(terra)


es_mm <- county_recreation
names(es_mm)



names(es_mm)

summary(es_mm)

plot(es_mm, "esdr")

plot(es_mm, "esdr", col=c("darkred","red", "orange", "yellow", "lightgreen", "green", "darkgreen"), 
     type="interval", breaks=7, plg=list(x="bottomright", cex=.75),
     pax=list(las=1),
     main="comprehensive supply-demand ratio (CESDR)")

writeVector(es_mm,
            "E:/ES_Demand_Supply/data/xiamen/result/cesdr_missmatch.shp",
            overwrite=TRUE,
            options="ENCODING=UTF-8")

# check empty neighbor sets -----------------------------------------------
es_mm_sf <- vect("E:/ES_Demand_Supply/data/xiamen/result/cesdr_missmatch.shp")

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 100
}

names(es_mm_sf)
#apply Min-Max normalization to PM2.5 column
es_mm_sf$esdr_n <- lapply(es_mm_sf[,36], min_max_norm)
# create a neighbor list  based on queen contiguity

es_sf <- st_as_sf(es_mm_sf)


list_nb <- poly2nb(es_sf, queen = TRUE)

# Check for empty neighbor sets
# card() calculates number of neighbors for each polygon in the list
# which() finds polygons with 0 neighbors
empty_nb <- which(card(list_nb) == 0)
empty_nb  

# Remove polygons with empty neighbor sets from the data
tes_subset <- es_sf[-empty_nb, ]

# Subset 'tes_data' to extract polygons with empty neighbor sets
empty_polygons <- es_sf[empty_nb, ]
empty_polygons$zone  # print neighborhood names



# Global G Test -----------------------------------------------------------

# Test for global spatial autocorrelation
# Now that we removed empty neighbor sets (tes_subset)
# Identify neighbors with queen contiguity (edge/vertex touching)
tes_nb <- poly2nb(tes_subset, queen = TRUE)

# Binary weighting assigns a weight of 1 to all neighboring features 
# and a weight of 0 to all other features
tes_w_binary <- nb2listw(tes_nb, style="B")

# Calculate spatial lag of TreEqty
tes_lag <- lag.listw(tes_w_binary, tes_subset$esdr_n)

# Test for global G statistic of TreEqty
globalG.test(tes_subset$esdr_n, tes_w_binary)


# Local Gi Test -----------------------------------------------------------

# Identify neighbors, create weights, calculate spatial lag
tes_nbs <- tes_subset |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tes_lag = st_lag(esdr_n, nb, wt)    # calculate spatial lag of TreEqty
  )

# Calculate the Gi using local_g_perm
tes_hot_spots <- tes_nbs |> 
  mutate(
    Gi = local_g_perm(esdr_n, nb, wt, nsim = 999)
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
    title = "Comprehensive supply-demand ratio Hot Spots in Xiamen"
  )
sessionInfo()
