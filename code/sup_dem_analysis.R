
## mask with built-area

d_heat <- crop(h_dem, built_32650, mask=TRUE)
d_pm <- crop(pm_exp, built_32650, mask=TRUE)
d_nai <- crop(nature_access, built_32650, mask=TRUE)


d_heat <- resample(d_heat, nature_access)
d_pm <- resample(d_pm, nature_access)
# d_pdi <- resample(d_pdi, nature_access)
d_ugs <- resample(ugs_pc, nature_access)

d_gs <- c(d_heat, d_pm, d_ugs)

names(d_gs) <- paste0(c("d_heat", "d_pm", "d_ugs"))

writeRaster(
  d_gs,
  "E:/ES_Demand_Supply/result/deman_es.tif",
  overwrite=TRUE
)

dem_es_n <- lapply(d_gs, rast_norm) |> rast()
pairs(c(dem_es_n[[1]],dem_es_n[[2]], dem_es_n[[3]]))

# compute correlation of all layers -------------------------------------------
dem_cor <- layerCor(
  dem_es_n,
  "cor",
  # w=3,
  use="pairwise.complete.obs"
)

dem_cor$correlation

corrplot(dem_cor$correlation,
         method = "circle",
         add = F,
         type = "lower",
         order = "AOE",
         hclust.method = "average",
         diag = T,
         col = COL2('RdBu', 10), # RdBu
         tl.srt = 0,
         tl.offset = 1
)

sup_dem <- c(sup_es_n, dem_es_n)

names(sup_dem) <- paste0(c("s_heat", "s_pm", "s_nac",
                           "d_heat", "d_pm", "d_perc"))
plot(sup_dem)

pairs(sup_dem)

# compute correlation of all layers
sd_cor <- layerCor(
  sup_dem,
  "cor",
  use="pairwise.complete.obs"
)

corrplot(sd_cor$correlation,
         method = "number",
         add = F,
         type = "lower",
         order = "original",
         hclust.method = "average",
         diag = T,
         col = COL2('RdBu', 10), # RdBu
         tl.srt = 0,
         tl.offset = 1
)


mm_heat <- sup_dem[[1]] - sup_dem[[4]]
mm_pm <- sup_dem[[2]] - sup_dem[[5]]
mm_park <- sup_dem[[3]] - sup_dem[[6]]

plot(mm_heat)
plot(mm_pm)
plot(mm_park)

ggcorr(
  sup_dem,
  method = c("pairwise", "spearman"),
  label = TRUE,
  label_alpha = TRUE
)

# PCA ---------------------------------------------------------------------

pca <- princomp(
  sup_dem,
  cor=TRUE,
  fix_sign = TRUE,
  use="pairwise.complete.obs",
  maxcell = 10000
)

biplot(princomp(sup_dem))

p2 <- predict(sup_dem, pca)
plot(p2)


# Calculation principal components
set.seed(123)
pca_sd <- prcomp(
  sup_dem,
  scale = TRUE,
  maxcell = 12500
)

# reverse the signs
pca_sd$rotation <- -1*pca_sd$rotation
pca_sd$x <- -1*pca_sd$x

# display PCA
pca_sd$rotation
head(pca_sd$x)

# visualize the result with a Biplot
biplot(pca_sd, scale = 0)

# find variance explained by each PC
pca_sd$sdev / sum(pca_sd$sdev)

# calculate total variance explained by each pc
var_sd <- pca_sd$sdev / sum(pca_sd$sdev)

# creat scree plot
qplot(
  c(1:6),
  var_sd
) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


# Spatial autocorrelation -------------------------------------------------

f <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
autocor(sup_dem)
# local 
sd_heat <- subset(sup_dem, c(1,4))
heat_cor <- autocor(
  sup_dem[[4]], 
  w=f, 
  global=FALSE,
  method="moran")
plot(heat_cor)


# LISA --------------------------------------------------------------------

built_sd <- zonal(
  sup_dem,
  built_32650,
  fun = "mean",
  na.rm = TRUE,
  weights = TRUE,
  as.polygons = TRUE
)

head(built_sd)

# LISA
built_sd_sf <- st_as_sf(built_sd)

mm_lisa = built_sd_sf |> 
  mutate(
    h_bi = st_local_bimoran(
      built_sd_sf,
      c('s_heat', 'd_heat')
    ),
    
    pm_bi = st_local_bimoran(
      built_sd_sf,
      c('s_pm', 'd_pm')
    ),
    
    pdi_bi = st_local_bimoran(
      built_sd_sf,
      c('s_nac', 'd_perc')
    )
  ) |> 
  select(name_pinyi, h_bi, pm_bi, pdi_bi)

ggplot(data = mm_lisa) +
  geom_sf(aes(fill = h_bi),lwd = .1,color = 'grey') +
  scale_fill_lisa()
