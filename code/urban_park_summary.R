library(sf)

source("code/helper.R") # call function for normalization

# Urban park standard
ub_park <- st_read("E:/ES_Demand_Supply/result/urban_park_standard.shp")

ub_park <- st_transform(
  ub_park,
  "EPSG:32650"
)

built_district <- st_as_sf(built_32650) # built area

# Join table
built_park <- st_join(ub_park, built_district, left = TRUE, largest = TRUE)

# Summary by sub-district
built_park |> 
  group_by(district, types) |> 
  summarise(n = n(),
            area_ha = round(sum(area_ha))) |> 
  st_drop_geometry() |> 
  flextable()

# Select usefull attributes
urban_park_district = built_park |> 
  select(
    district, name_pinyi, types, services, area_m2, area_ha
  )

# save to file
st_write(
  urban_park_district,
  "E:/ES_Demand_Supply/result/urban_park_summary.shp",
  overwrite=TRUE
)

# # Level services
# level_area <- as.data.frame(ugs_level) |> 
#   group_by(types) |> 
#   summarise(amount = n(),
#             area_ha = round(sum(area_ha))) |> 
#   arrange(desc(types))
# 
# ugs_table <- flextable(data = level_area)
# 
# ugs_table
# 
# # Urban park types
# upark <- st_drop_geometry(ugs_level) |> 
#   group_by(types) |> 
#   summarise(amount = n(),
#             area_ha = round(sum(area_ha))
#   )
# 
# upark
# 
# upark |> flextable()
# 
# 
# 
# st_drop_geometry(ugs_level) |> 
#   group_by(level, services) |> 
#   summarise(n = n()) |> 
#   ggplot(aes(x = level, y = n)) +
#   geom_col()
# 
# ugs_level_prj <- st_transform(ugs_level, 4527)
# ugs_level_cen <- st_centroid(ugs_level_prj)
# 
# plot(ugs_level_cen["leisure"])
# 
# # Sampling park locations at least 2 entrance
# set.seed(123)
# ugs_sample <- st_sample(ugs_level_prj, size = c(2,2))
# 
# plot(ugs_level_prj$geometry)
# plot(ugs_level_cen, col="green", add=TRUE)
# 
# # write to file
# writeVector(ugs_level,
#             "E:/ES_Demand_Supply/result/urban_park_standard.shp",
#             options = "ENCODING=UTF-8",
#             overwrite = T)