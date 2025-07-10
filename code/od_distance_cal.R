library(sfnetworks) # tidy geo-spatial networks
library(sf)         # simple feature  managements
library(tidygraph)  # graph manipulation
library(dplyr)      # data manipulation
library(purrr)      # functional programming tools
library(TSP)        # Travelling Salesperson Problem
library(ggplot2)
library(ggspatial) # Spatial data plus the power of the ggplot2 framework means easier mapping.
library(tidyterra)
library(terra)

library(tmap)
tmap_options(scale = 1)

library(igraph)
library(dbscan)

library(fca)

source("E:/ES_Demand_Supply/code/helper.R")

# Data used ---------------------------------------------------------------

## road network from OSM
road <- st_read("E:/ES_Demand_Supply/data/xiamen/road_osm/osm_highway_urban.shp")

## population
pop <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_count_32650.tif")

## Urban park and Residential node
nodes_all <- st_read("E:/ES_Demand_Supply/result/node_entrance.shp")

resd_nodes <- nodes_all |> 
                filter(types == "residential") |> 
                mutate(resd_id = c(1:n())) |> 
                select(-orig_id, -dest_id, -services)

ugs_nodes <- nodes_all |> 
                filter(types != "residential") |> 
                mutate(ugs_id = c(1:n())) |> 
                select(-orig_id,-dest_id,-pop)


#nodes_all_update <- nodes_all |> 
#  mutate(
#    services_w = case_match(
#      services,
#      c(2000, 3000, 5000) ~ 1200,
#      .default = services
#    )
#  )

## give each edge a unique index ----------------------------------------------

edges <- road |> 
  mutate(edgeID = c(1:n()))
# edges

## round coordinates to 0 digits
st_geometry(edges) = st_geometry(edges) |> 
  lapply(function(x) round(x, 0)) |> 
  st_sfc(crs = st_crs(edges))


## activation
net = as_sfnetwork(edges) |> # directed = FALSE
        activate("edges") |> 
        mutate(weight = edge_length()) |> 
        activate("nodes") |> 
        mutate(id = c(1:n()))

## extraction 
#net %>%
#  activate("nodes") %>%
#  st_as_sf()

# st_as_sf(net, "edges")
# st_as_sf(net, "nodes")


## visualization
# plot(net)
# # plot(net)
# 
# autoplot(net) + ggtitle("Road network of Xiamen City")
# 
# ggplot() +
#   geom_sf(data = st_as_sf(net, "edges"), aes(col = highway)) +
#   theme_void()

# Clean network ---------------------------------------------------------------

simple = net %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

# simple

subdivision = convert(simple, to_spatial_subdivision)

smoothed = convert(subdivision, to_spatial_smooth)

#smoothed = convert(subdivision, to_spatial_smooth,
#                   require_equal = "highway",
#                   protect = 2)
# smoothed
# plot(smoothed)

# Simplify intersection ------------------------------------------------------
# Retrieve the coordinates of the nodes.
node_coords = smoothed %>%
  activate("nodes") %>%
  st_coordinates()


# Cluster the nodes with the DBSCAN spatial clustering algorithm.
# We set eps = 0.5 such that:
# Nodes within a distance of 0.5 from each other will be in the same cluster.
# We set minPts = 1 such that:
# A node is assigned a cluster even if it is the only member of that cluster.
clusters = dbscan(node_coords, eps = 0.5, minPts = 1)$cluster
# Add the cluster information to the nodes of the network.
clustered = smoothed %>%
  activate("nodes") %>%
  mutate(cls = clusters)

clustered = clustered %>%
  mutate(cmp = group_components())

select(clustered, cls, cmp)

contracted = convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

contracted_clean = contracted |> 
  activate("edges") |> 
  mutate(weight = edge_length(),
         d_km = weight/1000,
         time_min = 60 * (d_km / 4.5),
         edge_id = c(1:n())
  ) |>
  activate("nodes") |>
  filter(!node_is_isolated()) |>
  mutate(node_id = c(1:n()))

# Isochrone -------------------------------------------------------------------
contracted_clean

# edge_time <- st_as_sf(contracted_clean, "edges") |> st_drop_geometry()

# Snapping point to their nearest node before joining -------------------------


# Blend the residential and UGS

tol = units::set_units(100, "m")

blended_nodes = st_network_blend(contracted_clean, nodes_all, tolerance = tol)

blended_subdivision = convert(blended_nodes, to_spatial_subdivision)

# str(st_as_sf(contracted, "nodes"))
# st_as_sf(blended_subdivision, "nodes")

# ggplot() +
#   geom_sf(data = st_as_sf(blended_subdivision, "edges"), col = "grey50") +
#   geom_sf(data = st_as_sf(blended_subdivision, "nodes"), aes(col = types)) +
#   ggtitle("Road network with combined residential and urban park nodes")

# plot(blended, "nodes")

blended_all = blended_subdivision |> 
            activate("edges") |> 
            mutate(weight = edge_length(),
			 edge_id = c(1:n())
			) |>
		activate("nodes") |>
		filter(!node_is_isolated()) |>
		mutate(node_id = c(1:n()))

blend_pois = st_as_sf(blended_subdivision, "nodes")
edge_clean = st_as_sf(blended_subdivision, "edges")

## extract nodes
residential <- blend_pois |> filter(types == "residential")

ugs <- blend_pois |> filter(types != "residential")

## Isochrone ------------------------------------------------------------------
iso = blended_subdivision |> 
  filter(
    node_distance_from(
      st_nearest_feature(ugs, blended_subdivision),
      weights = time_min
    ) <= 15
  )

iso_poly = iso |> 
  st_geometry() |> 
  st_combine() |> 
  st_convex_hull()

plot(blended_subdivision, col = "grey")
plot(iso_poly, col= NA, border = "black", lwd=3, add = T)
plot(iso, add=T)
plot(ugs, pch=8, cex=2, lwd=2, add=T)


# st_write(edge_clean,
#          dsn = "E:/ES_Demand_Supply/result/edge_clean.shp",
#          driver = "ESRI Shapefile",
#          append = FALSE)

#summary(st_as_sf(blended_all, "nodes"))

# plot(blended_all)

# OD calculation --------------------------------------------------------------



## not blended OD matrix
cost_matrix = blended_subdivision |> 
                st_network_cost(
                  from = resd_nodes,
                  to = ugs_nodes,
                  weights = "time_min",
                  direction = "out",
                  Inf_as_NaN = TRUE
                )

## calculate OD matrix
dist_matrix = blended_all |> 
                convert(to_spatial_directed) |> 
                st_network_cost(
                  from = residential,
                  to = ugs,
                  #weights = "weight",
                  direction = "out",
                  Inf_as_NaN = TRUE
                )

dist_matrix


d_eu <- st_distance(resd_nodes, ugs_nodes) # use when don't have network data

# Calculate spatial accessibility
r <- st_drop_geometry(resd_nodes)
p <- st_drop_geometry(ugs_nodes)


# dim(r)
# dim(p)
# 
r_row <- nrow(r)
p_row <- nrow(p)

# network matrix
net_m <- matrix(cost_matrix, # d_eu = euclidean; dist_matrix=network
              ncol =r_row,
              nrow = p_row,
              byrow = TRUE,
              dimnames = list(p$ugs_id, r$resd_id)
			)

# euclidean matrix
eu_m <- matrix(d_eu,
			ncol = r_row,
			nrow = p_row,
			byrow = TRUE,
			dimnames = list(p$ugs_id, r$resd_id)
			)


# normalize distance
w <- dist_normalize(
  net_m,
  d_max = 15,
  imp_function = "gaussian", function_d_max = 0.01
)


w_eu <- dist_normalize(
  eu_m,
  d_max = 1200,
  imp_function = "gaussian", function_d_max = 0.01
)


# ensure order of ids
r_order <- r[order(r$resd_id), ]
p_order <- p[order(p$ugs_id), ]

# named vector
(r_name <- setNames(r_order$pop, as.character(r_order$resd_id)))
(p_name <- setNames(p_order$services, as.character(p_order$ugs_id)))

# cal 3SFCA with euclidean matrix
(spai3fsca <- spai_3sfca(r_name, p_name, w_eu, step = 3))

# calculate 3SFCA with network analysis distance
(spai3_network <- spai_3sfca(p_name, r_name, w, step = 3))



summary(spai3fsca)

summary(spai3_network)


spai3fsca <- spai3fsca |> 
  mutate(row_id = row_number())

spai3_network <- spai3_network |> 
  mutate(row_id = row_number())

residential_id <- resd_nodes |> 
  mutate(row_id = row_number())

park_id <- ugs_nodes |> 
  mutate(row_id = row_number())

# join 3stepFCA with point data
pop_fca_net <- merge(park_id, spai3_network,
                  by.x = "row_id",
                  by.y = "row_id",
                  all.x = TRUE) |> 
              drop_na()

summary(pop_fca_net)

plot(pop_fca_net["step3"])
dist_network <- rasterize(
  pop_fca_net,
  ras_tem,
  field = "step3"
)

plot(dist_network)

nb_outdoor <- zonal(dist_network, built_32650, fun="mean", exact = FALSE,
                    as.polygons=TRUE, na.rm=TRUE)

tm_shape(nb_outdoor) +
  tm_polygons(
    fill = "last",
    fill.scale = tm_scale_intervals(
      style = "jenks"
    )
  )

pop_fca_eu <- merge(residential_id, spai3fsca,
                     by.x = "row_id",
                     by.y = "row_id",
                     all.x = TRUE) |> 
              drop_na()

summary(pop_fca_eu)

plot(pop_fca_eu["step3"])

st_write(pop_fca_eu,
          dsn = "E:/ES_Demand_Supply/result/pop_fca_net.shp",
          driver = "ESRI Shapefile",
          append = FALSE)

# pop_3fca |> mapview(zcol = "step3") +
#   mapview(ugs_sup)

## SPAI interpolation ----------------------------------------------------

library(gstat)

# template for interpolation
ras_tem <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_count.tif")
ras_tem <- project(ras_tem,
                   "epsg:32650",
                   res = 100)

spaisf <- gstat(id = "step3",
                formula = step3~1,
                data=pop_fca_net,
                nmax=7,
                set=list(idp = .5))

idw <- interpolate(
  ras_tem,
  spaisf,
  debug.level=0
)

interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

zsf <- interpolate(ras_tem,
                   spaisf,
                   debug.level=0,
                   fun=interpolate_gstat,
                   crs=crs(ras_tem),
                   index=1)

names(zsf) <- paste0("spai3")

zsf_xm <- mask(zsf, built_32650)

writeRaster(zsf_xm,
            "E:/ES_Demand_Supply/result/park_ai_net.tif",
            overwrite=TRUE)

plot(zsf_xm)
plot(ugs, add=T)

nb_outdoor <- zonal(zsf_xm, built_32650, fun="mean", exact = FALSE,
                    as.polygons=TRUE, na.rm=TRUE)

plot(nb_outdoor, "spai3")

nb_outdoor$spai3_r = round(nb_outdoor$spai3, 2)

nb_outdoor$spai3_n <- lapply(nb_outdoor["spai3_r"], vect_norm)



# make a final map

city_prj <- project(city, nb_outdoor)

# Plot with tmap to check jenks values ----------------------------------------
## symbol color palette = c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020")
## 
## nb_outdoor = st_read("E:/ES_Demand_Supply/result/park_3sfca.shp")

## check Jenks class
tm_shape(nb_outdoor) +
  tm_polygons(
    fill = "spai3_r",
    fill.scale = tm_scale_intervals(
      style = "jenks"
    )
  )

park_ai_map = tm_shape(city_prj) +
  tm_borders(col = "grey70", lwd = 1) +

tm_shape(nb_outdoor, unit = "m") +

  tm_polygons(fill = "spai3_r", 
		  fill.scale = tm_scale_intervals(n = 5, style = "jenks", 
		                                  values = "-brewer.rd_yl_bu",
		                                  labels = c("very low (0.00 - 0.28)", 
		                                             "low (0.28 - 0.72)", 
		                                             "medium (0.72 - 1.40)", 
		                                             "high (1.40 - 2.67)", 
		                                             "very high (2.67 - 4.35)")),
              col_alpha = 0.2,
		  fill.legend = tm_legend("", group_id = "top")
		 ) +
  tm_text("name_pinyi", size = 0.5) +

tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +

tm_title("Urban park SAI",
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

park_ai_map

tmap_save(park_ai_map,
          filename = "E:/ES_Demand_Supply/map/park_ai_map.jpg",
          width = 2100,
          # height = 600,
          units = "px",
          dpi = 600,
          # asp = 0,
          scale = 0.35)


spai_map <- tm_shape(city_prj) +
	tm_borders(col = "grey70", lwd = 1) +

tm_shape(nb_outdoor, unit = "m") +
  tm_polygons(fill = "spai3_r", 
		  fill.scale = tm_scale_intervals(n = 5, style = "jenks", values = "brewer.greens"),
              col_alpha = 0.2,
		  fill.legend = (
					tm_legend(
							title  = "3SFCA"
							
						   )
				    )
		 ) +
  tm_text("sub_names", size = 0.425) +
  
  tm_compass(position = c("right", "bottom")) +
  
  tm_scalebar(position = c("right", "bottom")) +
  
  tm_title("Park spatial accessibility in urban area by sub-district") +
  tm_layout(
    main.title.size= 0.95,
    frame = FALSE,
    legend.outside.position = "right"
  ) +

tm_shape(built_area_district) +
	tm_borders(col = "black", lwd = 2)
	

spai_map

## save map
tmap_save(spai_map, filename = "E:/ES_Demand_Supply/map/spai_map.jpg", width = 900, height = 600, dpi = 300,
		scale = 0.4
		)

nb_outdor_level <- nb_outdoor |> 
  mutate(
    park_ai = case_when(
      spai3 <= 0.088 ~ "very low",
      spai3 > 0.088 & spai3 <= 0.165 ~ "low",
      spai3 > 0.165 & spai3 <= 0.255 ~ "medium",
      spai3 > 0.255 & spai3 <= 0.465 ~ "high",
      spai3 > 0.465 ~ "very high"
    )
  )

ggplot() +
  geom_spatvector(data = city_prj, fill = NA, color="grey70") +
  
  geom_spatvector(data = nb_outdor_level, aes(fill = park_ai), color = "grey60") +
  scale_fill_whitebox_d(palette = "bl_yl_rd",
                        limits = c("very low", "low", "medium", "high", "very high")) +
  
  geom_spatvector(data = built_area_district, fill = NA, color = "black") +
  geom_spatvector_text(data = built_area_district, aes(label = district),
                       nudge_x = -4300, nudge_y = 2500,
                       fontface = "bold") +
  
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "Park Accessibility Index"
  )
  
  

ggplot() +
  geom_spatvector(data = city_prj, fill = NA, color="grey70") +
  geom_spatvector(data = nb_outdoor, aes(fill=spai3_n), color="grey60") +
  #geom_spatvector_text(data = nb_outdoor, aes(label = round(spai3, 2))) +
  geom_spatvector(data = built_area_district, fill = NA, color = "black") +
  geom_spatvector_text(data = built_area_district, aes(label = district),
                       nudge_x = -4300, nudge_y = 2500,
                       fontface = "bold") +
  #geom_spatvector_label(aes(label = name), color = "black") +
  scale_fill_whitebox_c(palette = "bl_yl_rd",
                        direction = 1,
                        breaks = c(0.156, 0.362, 0.594, 0.918, 1.406),
                        # labels = c("Very Low", "Low", "Average",
                        #            "High", "Very High")
                        ) + #reverse color
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "Park Accessibility Index"
  )


plot(nb_outdoor, "spai3_n", type="interval",
     breakby = "cases")


  


writeVector(nb_outdoor,
            "E:/ES_Demand_Supply/result/park_3sfca.shp",
            overwrite = TRUE
            )
