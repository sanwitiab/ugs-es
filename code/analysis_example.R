library(terra)
library(sf)
library(tmap)
library(tidyrgeoda)

es_mm_sf <- st_read("E:/ES_Demand_Supply/data/xiamen/result/cesdr_missmatch.shp")

head(es_mm_sf)

es_mm_sf$od_sub <- vect_norm(es_mm_sf$rsub_n)
es_mm_sf$od_dem <- vect_norm(es_mm_sf$rdem_n)

## MbSDES calculation
es_mm_sf$mbsdes <- 1 * es_mm_sf$od_sub - es_mm_sf$od_dem

summary(es_mm_sf)

## (supply - demand) / (supply_max + demand_max)/2
es_mm_sf$mm_cal <- (es_mm_sf$od_sub - es_mm_sf$od_dem) / 
  ((max(es_mm_sf$od_sub) +max(es_mm_sf$od_dem))/2)

plot(es_mm_sf["mm_cal"])

es_mm_sf |> 
  mutate(
    mm_cat = case_when(
      mm_cal > 0 ~ "surplus",
      mm_cal == 0 ~ "balance",
      mm_cal < 0 ~ "deficit"
    )
  ) |> 
  select(mm_cat) -> es_mm_cal

plot(es_mm_cal)

tm_shape(es_mm_sf) +
  tm_polygons(
    tm_vars(
      c("od_sub", "od_dem"),
      multivariate = TRUE
    ),
    fill.chart = tm_chart_heatmap()
  )

## NBClust
res <- NbClust::NbClust(es_mm_sf$esdr,
                        distance = "euclidean",
                        min.nc = 2,
                        max.nc = 8,
                        method = "complete",
                        index = "ch"
                        )

es_mm_sf$esdr_clust <- res$Best.partition


tm_shape(es_mm_sf) +
  tm_polygons(
    fill = "esdr_clust",
    fill.scale = tm_scale_categorical()
  )

# LISA
od_sub_lisa = es_mm_sf |> 
  mutate(
    lisa = st_local_moran(es_mm_sf,
                          'missmatch',
                          wt = st_weights(
                            es_mm_sf,
                            'contiguity',
                            queen = TRUE
                          )
                          )
  ) |> 
  select(lisa)


ggplot(data = od_sub_lisa) +
  geom_sf(aes(fill = lisa),lwd = .1,color = 'grey') +
  scale_fill_lisa()
