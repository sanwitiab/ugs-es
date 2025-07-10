# Sources: https://luukvdmeer.github.io/sfnetworks/articles/sfn03_join_filter.html
library(sfnetworks)
library(sf)
library(tidygraph)
library(dplyr)
library(purrr)
library(TSP)
library(ggplot2)

library(igraph)
library(dbscan)

# Road networks ---------------------------------------------------------------
road <- st_read("E:/ES_Demand_Supply/data/xiamen/road_osm/road_xm_clean.shp")


## give each edge a unique index ----------------------------------------------
edges <- road |> 
          mutate(edgeID = c(1:n()))
edges

net = as_sfnetwork(edges)


# Activation ------------------------------------------------------------------
net %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))

net %>%
  activate("nodes") %>%
  st_as_sf()

st_as_sf(net, "edges")

plot(net)

autoplot(net) + ggtitle("Road network of Xiamen City")

net = net %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness())

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(net, "nodes"), aes(col = bc, size = bc)) +
  ggtitle("Betweenness centrality in Xiamen City")

# Closest facility
residential <- st_read("E:/ES_Demand_Supply/data/xiamen/landuse/resident_cen.shp")
ugs <- st_read("E:/ES_Demand_Supply/data/xiamen/urban_green_space/ugs_cent.shp")

residential_crop <- st_crop(residential, road)
ugs_crop <- st_crop(ugs, road)

# Find indices of nearest nodes.
nearest_nodes = st_nearest_feature(residential_crop, net)
ugs_nearest_nodes = st_nearest_feature(ugs_crop, net)

# Snap geometries of POIs to the network.
snapped_residential = residential_crop %>%
  st_set_geometry(st_geometry(net)[nearest_nodes])

snapped_ugs = ugs_crop %>%
  st_set_geometry(st_geometry(net)[ugs_nearest_nodes])


plot(snapped_residential)
plot(snapped_ugs)

st_join(net, snapped_residential)

blended = st_network_blend(net, residential_crop)
blended = st_network_blend(net, ugs_crop)

blended


ggplot() +
  geom_sf(data = st_as_sf(blended, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(blended, "nodes"), aes(col = leisure)) +
  ggtitle("Betweenness centrality in Xiamen City")

# Blend the sites and facilities into the network to get better results.
# Also select only the largest connected component.
new_net = net %>%
  activate("nodes") %>%
  #filter(group_components() == 1) %>%
  st_network_blend(c(residential_crop, ugs_crop))

  net <- as_sfnetwork(c(road, residential, ugs))
