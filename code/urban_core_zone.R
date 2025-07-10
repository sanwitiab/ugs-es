library(zonebuilder)

core <- st_read("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/xm_core_urban.shp")

city_sf <- st_as_sf(city_32650)

built <- built_area_district |> 
          mutate(
            city = "Xiamen"
          ) |> 
          terra::aggregate("city") |> 
          fillHoles() |> 
          st_as_sf()

plot(built)


xm_zone <- zb_zone(
  core,
  built,
  n_circles = 9,
  city = "Xiamen"
)

plot(xm_zone)

# xm zone hmi
xm_zone <- vect(xm_zone)

# Rectangle
z = zb_quadrat(built,
               nrow = 100,
               ncol = 100)
plot(z)
