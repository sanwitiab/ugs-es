library(landscapemetrics)
library(motif)

source("E:/ES_Demand_Supply/code/helper.R") # call function for normalization

esa_lc <- rast("E:/ES_Demand_Supply/data/xiamen/landuse/esa_2021/esa_2021.tif")

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
esa_prj_utm <- project(esa_class, "epsg:32650", method = "mode")


# Crop to Xiamen city
lc_xiamen <- crop(esa_prj_utm, built_32650, mask=TRUE)
plot(lc_xiamen)

# check data
check_landscape(lc_xiamen)

list_lsm(level = "class")

# Shannon’s diversity index
lsm_l_shdi(lc_xiamen)

# aggregation index
lsm_l_ai(lc_xiamen)

# mean shape index on class level
mean_shape_c <- lsm_c_shape_mn(lc_xiamen)

mean_shape_l <- lsm_l_shape_mn(lc_xiamen) # landscape level

# area for each patch
area_patch <- lsm_p_area(lc_xiamen)

plot(lc_xiamen)
plot(built_32650, add = TRUE)

# calculate lsm for irregular polygons 
irr_landscapes = sample_lsm(
				lc_xiamen,
				built_32650,
				what = c(
					"lsm_c_pland",	#% of UGS
					"lsm_c_shape_mn",	#mean patch size
					"lsm_c_pd",		#patch density
					"lsm_c_lsi"		# landscape shape index
					)
				)

summary(irr_landscapes)

irr_landscapes |> 
  mutate(
    lu_class = case_when(
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
  
