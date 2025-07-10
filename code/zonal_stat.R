# Zonal
library(terra)

# read file
veg_fcv <- rast("E:/Bird/data/xiamen/result/CUGIC_Result/1_VegCoverage/vegetation_density.tif")
veg_fcv

veg_tree <- rast("E:/Bird/data/xiamen/result/CUGIC_Result/2_Vegetation_Height_Class/Tree_class.tif")


# resampling
veg_tree <- resample(veg_tree, veg_fcv, method="bilinear")
veg_tree

# frequency
freq(veg_fcv)
freq(veg_tree)

# Cross-tabulate
x <- c(veg_tree, veg_fcv)
crosstab(x)

# zonal
zonal(veg_tree, veg_fcv, fun="mean", na.rm=TRUE)