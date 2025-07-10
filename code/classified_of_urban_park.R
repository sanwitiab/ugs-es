# Urban Green Space ES mismatch ----------------------------------------------

# Library
library(sf)         # simple features management
library(tidyverse)  # data science tools
library(terra)      # spatial data analysis
library(tidyterra)  # applied common method from tidyverse to SpatRaster & SpatVector
library(fca)

source("code/helper.R") # call function for normalization

# Input directory

in_dir <- "E:/ES_Demand_Supply/data/xiamen/urban_green_space/"


# Import file

ugs <- st_read(paste0(in_dir, "ugs_clean.shp"))

plot(ugs)
plot(ugs["leisure"])

st_drop_geometry(ugs) |> 
  group_by(leisure) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = leisure, y = n)) +
  geom_col()

# classified ugs based on size
# Ref: 

ugs_level <- ugs |> 
  #filter(leisure != "forest") |> 
  mutate(
    level = case_when(
                      area_ha <= 1 ~ "community",
                      area_ha > 1 & area_ha <=10 ~ "neighborhood",
                      area_ha > 10 & area_ha <= 50 ~ "district",
                      area_ha > 50 ~ "municipal"
    ),
    services = case_when(
                      level == "municipal" ~ 5000,
                      level == "district" ~ 2500,
                      level == "neighborhood" ~ 1000,
                      level == "community" ~ 400
    ),
    types = case_when(
        area_ha >= 10 & leisure == "park" ~ "comprehensive park",
        area_ha < 10 & leisure == "park" ~ "community park",
        leisure == "garden" ~ "specialized purpose",
        leisure == "forest" ~ "forest"
    )
  )

plot(ugs_level["types"])

ggplot() +
  geom_spatvector(data = ugs_level, aes(fill=types)) +
  geom_spatvector(data = urban_prj, fill = NA, color = "grey", size = 1) +
  geom_spatvector(data = xm_prj, fill = NA, color = "black") +
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(style = "ticks", location = "br") +
  theme_void() +
  labs(
    fill = "Green Space Types"
  )

st_drop_geometry(ugs_level) |> 
  group_by(level) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = level, y = n)) +
  geom_col()

ugs_level_prj <- st_transform(ugs_level, 4527)
ugs_level_cen <- st_centroid(ugs_level_prj)

plot(ugs_level_cen["leisure"])

# Sampling park locations at least 2 entrance
set.seed(123)
ugs_sample <- st_sample(ugs_level_prj, size = c(2,2))

plot(ugs_level_prj$geometry)
plot(ugs_level_cen, col="green", add=TRUE)

# 2sfca for UGS supply ----------------------------------------------------

# library(matrixStats)
# library(SpatialAcc)   # spatial accessibility measures
library(fca)          # calculate spatial accessibility

## Data pre-processing ----------------------------------------------------
### Zone -------------------------------------------------------------
zone_dir <- "E:/ES_Demand_Supply/data/xiamen/extent/"
zone <- st_read(paste0(zone_dir, "xiamen_urban_zone.shp"))
plot(zone)

zone_urban <- zone |> 
                filter(zone == "urban area")

zone_prj <- st_transform(zone_urban, 4527) #4527; 3395; 32650
zone_32650 <- st_transform(zone_urban, 32650)


### LULC select residential for origin -------------------------------------
lulc <- st_read("E:/ES_Demand_Supply/data/xiamen/landuse/lu_single_valid.shp")

colnames(lulc)
summary(is.valid(vect(lulc)))


unique(lulc$一级地)

# plot(lulc[lulc$一级地 == "住宅用地",], max.plot = 1)

residential <- lulc |> filter(一级地 == "住宅用地") # filter residential

# plot(residential$geometry)


### Population data ----------------------------------------------------------
pop_dir <- "D:/sanwit/data/worldPop/pop_number_100m/"

pop <- rast(paste0(pop_dir, "chn_ppp_2020_constrained.tif"))
pop_xm <- crop(pop, zone)
# pop_prj <- project(pop, lulc, method="bilinear", mask=TRUE)
# pop_crop <- crop(pop_prj, lulc)
# plot(pop_crop)
# 
# # Write rater to files
# writeRaster(pop_crop, 
#             "E:/ES_Demand_Supply/data/xiamen/world_pop/pop_xm.tif",
#             datatype="INT8U")

# pop_xm <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_xm.tif")
pop_utm <- project(pop_xm, "EPSG:32650", method="bilinear")
pop_lu <- project(pop_xm, residential, method="bilinear")
plot(pop_utm)

# compare population with correlation function
pop_dist <- vect("E:/ES_Demand_Supply/data/xiamen/extent/county_population_valid.shp")
pop_dist_4527 <- project(pop_dist, pop_xm)

# rename column
pop_dist_4527 <- pop_dist_4527 |> 
  rename(
    district = "区县",
    population = "常人口",
    area = "面积",
    popDens = "人密"
  )

# Calculate total population
global(pop_utm, fun = "sum", na.rm=TRUE)

zone_pop <- zonal(pop_xm, vect(zone_prj),
                  fun="sum",
                  as.polygons=TRUE,
                  na.rm=TRUE
                  )

pop_dist_worldpop <- zonal(pop_xm, pop_dist_4527, fun="sum",
                           as.polygons=TRUE, na.rm=TRUE
                           )

cor(pop_dist_worldpop$population, pop_dist_worldpop$chn_ppp_2020_constrained)

par(mfrow=c(1,2))
m <- c(3.1, 3.1, 2.1, 2.1)
plot(pop_dist_worldpop,"population",
     mar=m,
     plg=list(x="topright"),
     pax=list(las=1),
     type="interval",
     main="Xiamen Gov.")
plot(pop_dist_worldpop, "chn_ppp_2020_constrained",
     mar=m,
     plg=list(x="topright"),
     pax=list(las=1),
     type="interval",
     main="WorldPop")
dev.off()

# ggally: spearman's correlation
library(GGally)
ggcorr(
  pop_dist_worldpop[,c(4:7)],
  method = c("pairwise", "spearman"),
  label = TRUE,
  label_alpha = TRUE
)

# extract population per residential areas
residential_pop <- zonal(pop_lu, vect(residential), fun="sum",
                         as.polygons=TRUE, na.rm=TRUE)

plot(residential_pop, "chn_ppp_2020_constrained", border=NULL)

#st_as_sf(residential_pop) |> summarise(popSum = sum(chn_ppp_2020_constrained))

residential_cen <- st_centroid(st_as_sf(residential_pop))

res_pop <- residential_cen |> 
  drop_na()

res_pop |> summarise(popSum = sum(chn_ppp_2020_constrained))


# check CRS
st_crs(zone_prj) == st_crs(res_pop)
pop_urban <- st_intersection(res_pop, zone_prj)


plot(zone_prj$geometry)
plot(pop_urban$geometry, col="red", add=TRUE)
plot(ugs_level_cen$geometry, col="green", add=TRUE)


## Floating Catchment Area (FCA) -------------------------------------------

pop_residential <- pop_urban |> 
                      mutate(orig_id = row_number()) |> 
                      select(orig_id, chn_ppp_2020_constrained) |> 
                      st_transform(32650)

ugs_sup <- ugs_level_cen |> 
              mutate(dest_id = row_number()) |> 
              select(dest_id, services) |> 
              st_transform(32650)

# st_write(ugs_sup, 
#          dsn = "E:/ES_Demand_Supply/result/ugs_sup.shp",
#          driver = "ESRI Shapefile")

st_crs(pop_residential) == st_crs(ugs_sup)

p <- st_drop_geometry(pop_residential)
s <- st_drop_geometry(ugs_sup)

d_eu <- st_distance(pop_residential, ugs_sup) # use when don't have network data

dim(p)
dim(s)

p_row <- nrow(p)
s_row <- nrow(s)

d_m <- matrix(d_eu, # d_eu = euclidean; dist_matrix=network
              ncol = p_row,
              nrow = s_row,
              byrow = TRUE)

d_m

# normalize distance
w <- dist_normalize(
  d_m,
  d_max = 5000,
  imp_function = "gaussian", function_d_max = 0.01
)

# ensure order of ids
p <- p[order(p$orig_id),]
s <- s[order(s$dest_id),]

# named vector
(p <- setNames(p$chn_ppp_2020_constrained, as.character(p$orig_id)))
(s <- setNames(s$services, as.character(s$dest_id)))

(spai <- spai_2sfca(p,s,w, step = 2))

spai_id <- spai |> 
              mutate(row_id = row_number())

# join 3stepFCA with point data
pop_3fca <- merge(pop_residential, spai_id,
                  by.x = "orig_id",
                  by.y = "row_id")

plot(pop_3fca["step2"], breaks = "jenks")
plot(ugs_level_cen$geometry, col="green")

# st_write(pop_3fca,
#          dsn = "E:/ES_Demand_Supply/result/pop_3fca.shp",
#          driver = "ESRI Shapefile")

# pop_3fca |> mapview(zcol = "step3") +
#   mapview(ugs_sup)

## SPAI interpolation ----------------------------------------------------

library(gstat)

zone_32650 <- st_transform(zone_urban, 32650)

r <- project(pop_xm, pop_3fca, method="bilinear", mask=TRUE)
r <- crop(r, zone_32650, mask=TRUE)

spaisf <- gstat(id = "step2", formula = step2~1, data=pop_3fca,  nmax=7, set=list(idp = .5))

interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

zsf <- interpolate(r, spaisf, debug.level=0, fun=interpolate_gstat, crs=crs(r), index=1)

names(zsf) <- paste0("spai3")

zsf_xm <- mask(zsf, pop_utm)
plot(zsf)
plot(zone_32650$geometry, add=TRUE)
plot(ugs_sup, col="green",add=TRUE)

# Potential supply based on spatial accessibility index (3sfca) -----------------
library(ggplot2)

nb_urban <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area.shp")
nb_urban_prj <- project(nb_urban, pop_3fca)

nb_outdoor <- zonal(zsf, nb_urban_prj, fun="median", exact = FALSE,
                    as.polygons=TRUE, na.rm=TRUE)

nb_outdoor <- zonal(pop_utm, nb_outdoor, fun="sum",
                    as.polygons=TRUE, na.rm=TRUE)

plot(nb_outdoor, "spai3")
plot(nb_outdoor, "chn_ppp_2020_constrained")

ggplot(data = st_as_sf(nb_outdoor)) +
  geom_sf(aes(fill = spai3)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = st_as_sf(nb_outdoor)) +
  geom_sf(aes(fill = chn_ppp_2020_constrained)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Desired demand by Society / Policy (Xiamen Gov. = 11 m2 per capita ) --------
# Vegetation
esa_lc <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_2021.tif")

# Reclassify 0 = novegetaion and >0 = vegetation
m <- c(10, 30, 1, # 0=NoVegetation
       31, 100, NA) # vegetation
relveg <- matrix(m, ncol = 3, byrow = TRUE)

esa_green <- classify(esa_lc, relveg, include.lowest=TRUE)

# transform projection
esa_green_prj <- project(esa_green, "EPSG:32650", method="mode")

nb_outdoor$zone <- seq.int(nrow(nb_outdoor)) # add row id
nb_outdoor$area_m2 <- expanse(nb_outdoor)      # calculate area in m2

# Zonal statistics
veg_freq_county <- freq(esa_green_prj, zone = nb_outdoor)
veg_freq_county$greenarea <- veg_freq_county$count * 100
head(veg_freq_county)

# Join table
nb_outdoor_sd <- merge(nb_outdoor, veg_freq_county, all.x=TRUE, by.x="zone", by.y="zone")
head(nb_outdoor_sd)

cor(nb_outdoor_sd$spai3, nb_outdoor_sd$greenarea) # check correlation

# recreation demand
# pop_xm <- rast("E:/ES_Demand_Supply/data/xiamen/world_pop/pop_xm_32650.tif")

names(pop_utm) <- paste0("pop")

nb_outdoor_sd <- zonal(pop_utm, nb_outdoor_sd, 
                       fun = "sum",
                       as.polygons = TRUE,
                       na.rm=TRUE)
summary(nb_outdoor_sd)


nb_outdoor_sd$green_capita <- (nb_outdoor_sd$greenarea - (nb_outdoor_sd$chn_ppp_2020_constrained * 11))
head(nb_outdoor_sd)

plot(nb_outdoor_sd, "green_capita")

ggplot(data = st_as_sf(nb_outdoor_sd)) +
  geom_sf(aes(fill = green_capita)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


# normalized value
# demand
#apply Min-Max normalization to PM2.5 column
names(nb_outdoor_sd)
nb_outdoor_sd$green_demand <- lapply(nb_outdoor_sd[,16], min_max_norm)
nb_outdoor_sd$green_supply <- lapply(nb_outdoor_sd[,8], min_max_norm)

plot(nb_outdoor_sd, "green_demand")

ggplot(data = st_as_sf(nb_outdoor_sd)) +
  geom_sf(aes(fill = green_supply)) +
  scale_fill_viridis_c(option = "D", trans = "sqrt")

ggplot(data = st_as_sf(nb_outdoor_sd)) +
  geom_sf(aes(fill = green_demand)) +
  scale_fill_viridis_c(option = "D", trans = "sqrt")

# Air temperature ------------------------------------------------------------
t_air <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/intermediate/T_air_xm_jun.tif")
hmi <- rast("E:/ES_Demand_Supply/data/UrbanCooling/data/hm_xm_jun.tif")

plot(t_air)

t_air_prj <- project(t_air, nb_outdoor_sd, method="bilinear")
hmi_prj <- project(hmi, nb_outdoor_sd, method="bilinear")

plot(t_air_prj)

nb_outdoor_temp <- zonal(t_air_prj, nb_outdoor_sd,
                         fun="mean", as.polygons=TRUE, na.rm=TRUE
                         )
head(nb_outdoor_temp)

plot(nb_outdoor_temp, "T_air_xm_jun")

nb_outdoor_hmi <- zonal(hmi_prj, nb_outdoor_temp,
                         fun="mean", as.polygons=TRUE, na.rm=TRUE
)

head(nb_outdoor_hmi)

ggplot(data = st_as_sf(nb_outdoor_hmi)) +
  geom_sf(aes(fill = T_air_xm_jun)) +
  scale_fill_viridis_c(option = "D", trans = "sqrt")

ggplot(data = st_as_sf(nb_outdoor_hmi)) +
  geom_sf(aes(fill = hm_xm_jun)) +
  scale_fill_viridis_c(option = "D", trans = "sqrt")


# PM2.5 ---------------------------------------------------------------------

## PM2.5 concentration ------------------------------------------------------
pm25 <- rast("E:/ES_Demand_Supply/data/xiamen/pm2.5/xiamen_pm25/pm25_xm.tif")
nb_outdoor_temp_pm <- zonal(pm25, nb_outdoor_hmi,
                         fun="median", as.polygons=TRUE, na.rm=TRUE
                         )
head(nb_outdoor_temp_pm)

ggplot(data = st_as_sf(nb_outdoor_temp_pm)) +
  geom_sf(aes(fill = pm25)) +
  scale_fill_viridis_c(option = "D", trans = "sqrt")

## PM2.5 Removal by tree leave ----------------------------------------------


evi <- rast("E:/ES_Demand_Supply/data/xiamen/lai/EVI.tif")

evi[values(evi) > 0.8] =NA #Remove all values exceeding max threshold.
evi[values(evi) < 0.2] =NA #Remove all values that are assumed to not be vegetation

plot(evi)

lai <- (3.618 * evi - 0.118)
lai <- crop(lai, pm25)

lai_resamp <- resample(lai, pm25, method = "average")

plot(lai_resamp)

### Calculate pollution removal or flux based on Nowak, 2013 ----------------
# F = Vd * C
# Vd = deposition velocity of the pollutant to leaf surface (mh1)
# C is pollutant concentration (μg m−3)

Vd = 0.17
C = pm25
r = 6

f_pm = Vd * C * lai_resamp * r
plot(f_pm)

names(f_pm) <- paste0("pm_removal")

nb_outdoor_temp_pm_r <- zonal(f_pm, nb_outdoor_temp_pm,
                            fun="median", as.polygons=TRUE, na.rm=TRUE
)

head(nb_outdoor_temp_pm_r)
ggplot(data = st_as_sf(nb_outdoor_temp_pm_r)) +
  geom_sf(aes(fill = pm_removal)) +
  scale_fill_viridis_c(option = "D", trans = "sqrt")

# Pearson's correlation -----------------------------------------------------

xm_demand <- st_as_sf(nb_outdoor_temp_pm_r)

xm_es <- st_drop_geometry(xm_demand)

names(xm_es)
xm_es_cor <- xm_es |> 
                select(zone, green_capita, T_air_xm_jun, pm25,
                       spai3, hm_xm_jun, pm_removal)

pairs(~green_capita + T_air_xm_jun + pm25,
      data=xm_es_cor,
      main="Demand Scatterplot Matrix")

pairs(~spai3 + hm_xm_jun + pm_removal,
      data=xm_es_cor,
      main="Supply Scatterplot Matrix")

#create scatterplot of hours studied vs. exam score
plot(xm_es_cor$green_capita, xm_es_cor$T_air_xm_jun, pch=16,
     col="black",
     main="UGS per capita vs. Air temperture",
     xlab="UGS per capita", ylab="Air Temp (C)"
     )

plot(xm_demand$T_air_xm_jun, xm_demand$pm25, pch=16,
     col='black',
     main='Air temperture vs. PM2.5 Concentration',
     xlab='Air temperture', ylab='PM2.5 mUg'
)

plot(xm_demand$pm25, xm_demand$green_capita, pch=16,
     col='black',
     main='PM2.5 Concentration vs. UGS per capita',
     xlab='PM2.5 Concentration', ylab='UGS per capita'
)

#calculate correlation between hours studied and exam score received
cor(xm_demand$green_capita, xm_demand$T_air_xm_jun)
cor(xm_demand$T_air_xm_jun, xm_demand$pm25)
cor(xm_demand$green_capita, xm_demand$pm25)

cor(xm_demand$spai3, xm_demand$hm_xm_jun)
cor(xm_demand$spai3, xm_demand$pm_removal)
cor(xm_demand$hm_xm_jun, xm_demand$pm_removal)

# supply scatter plot
ggplot(xm_demand, aes(x=spai3, y=pm_removal)) +
  geom_point(shape=16) +
  geom_smooth(method = lm, # add linear regression line (95% confidence)
              se=FALSE     # don't add shaded confidence region
              ) +
  ggtitle("3sfca vs. PM2.5 Removal") +
  theme(plot.title = element_text(lineheight = .8, face = "bold"))

# demand scatter plot
ggplot(xm_demand, aes(x=green_capita, y=T_air_xm_jun)) +
  geom_point(shape=16) +
  geom_smooth(method = lm, # add linear regression line (95% confidence)
              se=FALSE     # don't add shaded confidence region
  )

# Raster regression --------------------------------------------------------
# pm25 vs lai_resamp
pm_lai_x <- regress(pm25, lai_resamp, na.rm=TRUE)
pm_lai_x
plot(pm_lai_x)


## Spatial autocorrelation ------------------------------------------------
library(spdep)
library(sfdep)
### Global ----------------------------------------------------------------
autocor(hmi_prj)
autocor(pm25)
autocor(t_air)
autocor(lai)

### Local -----------------------------------------------------------------
hmi_aoto <- autocor(hmi_prj, w=5, "moran", global=FALSE)
plot(hmi_aoto)

lai_auto <- autocor(lai, w=5, "moran", global=FALSE)
plot(lai_auto)

# Global (Vector) ---------------------------------------------------------
# xm_demand layer must be sf object
## check empty neighbor sets -----------------------------------------------

# create a neighbor list  based on queen contiguity
list_nb <- poly2nb(xm_demand, queen = TRUE)

# Check for empty neighbor sets
# card() calculates number of neighbors for each polygon in the list
# which() finds polygons with 0 neighbors
empty_nb <- which(card(list_nb) == 0)
empty_nb  

# Remove polygons with empty neighbor sets from the data
tes_subset <- xm_demand[-empty_nb, ]

# Subset 'tes_data' to extract polygons with empty neighbor sets
empty_polygons <- xm_demand[empty_nb, ]
empty_polygons$Name  # print neighborhood names

## Global G Test -----------------------------------------------------------

# Test for global spatial autocorrelation
# Now that we removed empty neighbor sets (tes_subset)
# Identify neighbors with queen contiguity (edge/vertex touching)
tes_nb <- poly2nb(tes_subset, queen = TRUE)

# Binary weighting assigns a weight of 1 to all neighboring features 
# and a weight of 0 to all other features
tes_w_binary <- nb2listw(tes_nb, style="B")

# Calculate spatial lag of TreEqty
tes_lag <- lag.listw(tes_w_binary, tes_subset$spai3)

# Test for global G statistic of TreEqty
globalG.test(tes_subset$spai3, tes_w_binary)

## Local Gi Test -----------------------------------------------------------

# Identify neighbors, create weights, calculate spatial lag
names(tes_subset)
tes_nbs <- tes_subset |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tes_lag = st_lag(spai3, nb, wt)    # calculate spatial lag of TreEqty
  )

# Calculate the Gi using local_g_perm
tes_hot_spots <- tes_nbs |> 
  mutate(
    Gi = local_g_perm(spai3, nb, wt, nsim = 999)
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
    title = "UGS spatial accessibility index in Xiamen"
  )
sessionInfo()
