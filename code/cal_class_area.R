library(terra)

# read file
vegmor <- rast("E:/Bird/data/xiamen/result/CUGIC_Result/3_Veg_Mor/vegetation_morphology_sub.tif")

vegmor

# sum area in km2
expanse(vegmor, unit="km")

# sume by values
expanse(vegmor, unit="ha", byValue=TRUE)