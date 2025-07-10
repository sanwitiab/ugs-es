library(terra)

es_mm_sf <- vect("E:/ES_Demand_Supply/data/xiamen/result/cesdr_missmatch.shp")

# recreation priority index
es_mm_sf$re_pri <- es_mm_sf$rdem_n/es_mm_sf$rsub_n
plot(es_mm_sf, "re_pri", col=c("darkred","red", "orange", "yellow", "lightgreen", "green", "darkgreen"), 
     type="interval", breaks=7, plg=list(x="bottomright", cex=.75),
     pax=list(las=1),
     main="comprehensive supply-demand ratio (CESDR)")
