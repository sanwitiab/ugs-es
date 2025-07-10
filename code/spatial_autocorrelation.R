# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html
library(sf)
library(spdep)
library(rgeoda)
library(ggplot2)

# read data
built_sup <- st_read("E:/ES_Demand_Supply/result/ugs_supply_index.shp")

# Check CRS
st_crs(built_sup)

# Check data
head(built_sup)
str(built_sup)
names(built_sup)

# Simple plot to confirm
plot(
	built_sup['pmr'],
	graticule = sf::st_crs(32650),
	main='Heat mitigation index',
	breaks="jenks",
	axes=TRUE
)

# Use spdep package to test (Global autocorrelation)
# spdep::moran, spdep::moran.test
# The zero.policy = TRUE allows zero-length weights vectors

built_sup %>%
	spdep::poly2nb(
	built_sup['Neigh_ID'] %>%
		st_drop_geometry() %>%
		magrittr::extract2(1) %>%
			as.vector
	) %>%
	spdep::nb2listw(
		zero.policy = TRUE
	) -> built_nblist

built_nblist %>%
	spdep::moran.test(built_sup$pmr, ., zero.policy = TRUE)

# Moran scatter plot to see details
spdep::moran.plot(
	built_sup$pmr,
	built_nblist,
	zero.policy = TRUE,
	xlab = 'Heat mitigation rate at neighborhood',
	ylab = 'Lagged Heat mitigationrate (of Neighbors)',
	pch=20
)

# Local Indicators of Spatial Autocorrelation (LISA)
# use spdep package to test (global autocorrelation)
# spdep::localmoran, spdep::localmoran.exact
# localmoran result: 
##  High positive Ii means similar values (either high or low clusters)
##  Low negative Ii means dissimilar values (outliers)
##

lisa_result <-spdep::localmoran(
			built_sup$pmr,
			built_nblist,
			zero.policy = TRUE,
			na.action = na.omit			
)

# The dimension of LISA result and the original sf ob ject
dim(lisa_result); dim(built_sup)

# some results
## Ii = local moran statistic
## EIi = expectation of local moran statistic
## Var.Ii = varaince of local moran statistic
## Z.Ii = standard deviate of local moran statistic
## Pr() = p-value of local moran statistic

## p-value >= significant level 
## i.e. p >= 0.05, they could be labeled insignificant
## and we would not know if they are cluster/outliers
## p < 0.05, and Ii positive = cluster (similar to nearby or neighboring values)
## otherwise, it would be a outlier
head(lisa_result)

# Now we can derive the cluster/outlier types (COType in ArcGIS term) 
# for each spatial feature in the data
sig_level <- 0.05; #95% confidence
meanVal <- mean(built_sup$pmr);

lisa_result %<>%
	tibble::as_tibble() %>%
    
		magrittr::set_colnames(c("Ii", "E.Ie", "Var.Ii", "Z.Ii", "Pr()")) %>%
		dplyr::mutate(coType = dplyr::case_when(
			`Pr()` > 0.05 ~ "Insignificant",
  			`Pr()` <= 0.05 & Ii >= 0 & built_sup$pmr >= meanVal ~ "HH",
  			`Pr()` <= 0.05 & Ii >= 0 & built_sup$pmr < meanVal ~ "LL",
  			`Pr()` <= 0.05 & Ii < 0 & built_sup$pmr >= meanVal ~ "HL",
  			`Pr()` <= 0.05 & Ii < 0 & built_sup$pmr < meanVal ~ "LH"
	)
)

# Now add this coType to original sf data
built_sup$coType <- lisa_result$coType %>%
  tidyr::replace_na("Insignificant")

ggplot(built_sup) +
  geom_sf(aes(fill=coType), color = 'lightgrey') +
  scale_fill_manual(
    values = c('red', 'brown', 'NA', 'blue','cyan'),
    name = 'Cluster & \nOutliers'
  ) +
  labs(title = "Heat mitigation at Town level")

# Define this as a function, which could save some space.
plotCOType <- function(varName, titleText, cols=11:13) {
  varVals <- built_sup[[varName]] %>% as.character() %>% as.numeric()
  lisaRslt <- spdep::localmoran(varVals, built_nblist, 
                                zero.policy = TRUE, na.action = na.exclude)
  significanceLevel <- 0.05; # 95% confidence
  meanVal <- mean(varVals, na.rm=TRUE);
  
  lisaRslt %<>% tibble::as_tibble() %>%
    magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
    dplyr::mutate(coType = dplyr::case_when(
      `Pr(z > 0)` > 0.05 ~ "Insignificant",
      `Pr(z > 0)` <= 0.05 & Ii >= 0 & varVals >= meanVal ~ "HH",
      `Pr(z > 0)` <= 0.05 & Ii >= 0 & varVals < meanVal ~ "LL",
      `Pr(z > 0)` <= 0.05 & Ii < 0 & varVals >= meanVal ~ "HL",
      `Pr(z > 0)` <= 0.05 & Ii < 0 & varVals < meanVal ~ "LH"
    ))
  
  # Now add this coType to original sf data
  built_sup$coType <- lisaRslt$coType %>% tidyr::replace_na("Insignificant")
  
  
  ggplot(built_sup) +
    geom_sf(aes(fill=coType), color = 'lightgrey') +
    scale_fill_manual(values = c('red','brown','NA','blue','cyan')[cols], name='Clusters & \nOutliers') +
    labs(title = titleText) +
    theme_void()
}

plotCOType('hmi', "Heat mitigation index at Town Level",c(1,3,5))
plotCOType('pmr', 'PM2.5 removal by tree at Town Level', c(1,3,5))
plotCOType('nai', 'Nature access at Census Town Level', c(1,3,5))

# Global Moran I
xmNearNBlist <- knn2nb(knearneigh(built_sup %>% sf::st_point_on_surface(), k=1)) %>% spdep::nb2listw()

moranBVboot <- spdep::moran_bv(x = datX <- built_sup$hmi, 
                               y = datY <- (built_sup$pmr_n/(1+built_sup$nai)) %>% as.numeric(), 
                               listw = xmNearNBlist, 
                               nsim = 500)

# Tend to overestimate the association

print(moranBVboot);
cat("\n")
boot::boot.ci(moranBVboot, conf=c(0.99, 0.95, 0.9), type="basic") %>% print()
plot(moranBVboot)

localMoranBV <- spdep::localmoran_bv(x = datX, 
                                     y = datY, 
                                     listw = xmNearNBlist, 
                                     nsim = 500) %>% as.data.frame()
# To plot as HH, HL, LH, LL
# refer 
# listw2mat, nb2mat functions to get a matrix mat
# apply the matrix to scaled y  mat%*%scale(y) > 0 high < 0 low
meanVal <- xmNearNBlist %>% listw2mat() %*% scale(datY) %>% as.vector()
significanceLevel <- 0.05; # 95% confidence

#localMoranBV$datY <- datY
#localMoranBV$meanVal <- meanVal

localMoranBV %<>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ibvi","E.Ibvi","Var.Ibvi","Z.Ibvi","P", "PSim", "PFoldedSim")) %>%
  dplyr::mutate(lmbvType = dplyr::case_when(
    `PSim` > 0.05 ~ "Insignificant",
    `PSim` <= 0.05 & Ibvi >= 0 & datY >= meanVal ~ "HH",
    `PSim` <= 0.05 & Ibvi >= 0 & datY < meanVal ~ "LL",
    `PSim` <= 0.05 & Ibvi < 0 & datY >= meanVal ~ "HL",
    `PSim` <= 0.05 & Ibvi < 0 & datY < meanVal ~ "LH"
  ))

# Now add this lmbvType to original sf data
built_sup$lmbvType <- localMoranBV$lmbvType %>% tidyr::replace_na("Insignificant")


ggplot(built_sup) +
  geom_sf(aes(fill=lmbvType),color = 'lightgrey') +
  scale_fill_manual(values = c('red','brown','NA','blue','cyan')[c(3:4)], name='Clusters & \nOutliers') +
  labs(title = "Local Bivariate Moran's I ")

subDat <- built_sup %>% 
  dplyr::select(name_pinyi, hmi, pmr, nai, lmbvType);

mapview::mapview(x = subDat, 
                 zcol='lmbvType', 
                 col.regions = c('red','brown','NA','blue','cyan')[3:4],
                 layer.name = 'Clusters & Outliers')
