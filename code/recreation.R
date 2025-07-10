
# Recreation --------------------------------------------------------------

library(terra)
library(dplyr)


# County boundary
county <- vect("E:/ES_Demand_Supply/data/xiamen/extent/XM_builtup/built_area.shp")

county

# Vegetation
veg <- rast("E:/ES_Demand_Supply/data/xiamen/ndvi_2020/ndvi_2020_summer.tif")

plot(veg)
freq(veg)
# Reclassify 0 = novegetaion and >0 = vegetation
m <- c(-1, 0.2, NA, # 0=NoVegetation
       0.2, 1, 1) # vegetation
relveg <- matrix(m, ncol = 3, byrow = TRUE)

veg_group <- classify(veg, relveg, include.lowest=TRUE)
plot(veg_group)
freq(veg_group)

# Project vector to raster CRS
county_prj <- project(county, "EPSG:32650")
veg_group_prj <- project(veg_group, "EPSG:32650")

county_prj$zone <- seq.int(nrow(county_prj)) # add row id
county_prj$area <- expanse(county_prj)      # calculate area in m2
head(county_prj)

# border_mm$area <- expanse(border_mm)
# head(border_mm)

plot(veg_group_prj)
plot(county_prj, add=TRUE)

# Zonal statistics
veg_freq_county <- freq(veg_group_prj, zone=county_prj)
veg_freq_county$greenarea <- veg_freq_county$count * 100
head(veg_freq_county)

# Join table
county_recreation <- merge(county_prj, veg_freq_county, all.x=TRUE, by.x="zone", by.y="zone")
county_recreation
values(county_recreation)

# recreation supply
county_recreation$recre_sup <- round(county_recreation$greenarea / county_recreation$area, 2)
values(county_recreation)

# recreation demand
pop_xm_prj <- project(pop_xm, "EPSG:32650")
names(pop_xm_prj) <- paste0("pop")

county_recreation_pop <- zonal(pop_xm_prj, county_recreation, fun="sum",
                         as.polygons=TRUE, na.rm=TRUE)
head(county_recreation_pop)

county_recreation_pop$recre_dem <- county_recreation_pop$pop * 11 #13.5 ShangHai
values(county_recreation_pop)
# normalized value
# supply
rs_min <- min(county_recreation$recre_sup, na.rm = TRUE)
rs_max <- max(county_recreation$recre_sup, na.rm = TRUE)

county_recreation$rsub_n <- (county_recreation$recre_sup + rs_min)/(rs_max - rs_min) #* 100
values(county_recreation)

# demand
rd_min <- min(county_recreation_pop$recre_dem, na.rm = TRUE)
rd_max <- max(county_recreation_pop$recre_dem, na.rm = TRUE)

county_recreation_pop$rdem_n <- (county_recreation_pop$recre_dem + rd_min)/(rd_max - rd_min) #* 100
values(county_recreation)

plot(county_recreation_pop, "rsub_n", type="interval")
plot(county_recreation_pop, "rdem_n", type="interval")


plot(county_recreation, c("rsub_n"), col=c("grey", "darkgrey","lightgreen","green","darkgreen"))
plot(county_recreation, c("rdem_n"), col=c("grey", "darkgrey","orange","red","darkred"))


par(mfrow=c(1,2))
m <- c(3.1, 3.1, 2.1, 2.1)
plot(county_recreation, "rsub_n", col=c("grey", "darkgrey","lightgreen","green","darkgreen"),
     mar=m,
     plg=list(x="topright"),
     pax=list(las=1),
     main="Recreation supply")
plot(county_recreation, "rdem_n", col=c("grey", "darkgrey","orange","red","darkred"),
     mar=m,
     plg=list(x="topright", cex=.75),
     pax=list(las=1),
     main="Recreation demand")
dev.off()

# Area missmatch
s_max <- max(county_recreation$rsub_n, na.rm = T)
d_max <- max(county_recreation$rdem_n, na.rm = T)

county_recreation$re_mm <- (county_recreation$rsub_n - county_recreation$rdem_n)/((s_max + d_max) / 2)
values(county_recreation)

plot(county_recreation, "re_mm", col=c("darkred","red", "orange", "yellow", "lightgreen", "green", "darkgreen"), 
     type="interval", breaks=7, plg=list(x="bottomright", cex=.75),
     pax=list(las=1),
     main="Recreation Mismatch")

plot(county_recreation, "人密", 
     type="interval", breaks=7, plg=list(x="bottomright", cex=.75),
     pax=list(las=1),
     main="Recreation demand")


# Write the result --------------------------------------------------------

writeVector(county_recreation,
            "E:/ES_Demand_Supply/data/xiamen/result/es_missmatch.shp",
            overwrite=TRUE,
            options="ENCODING=UTF-8")


