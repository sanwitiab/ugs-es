library(terra)

source("E:/ES_Demand_Supply/code/helper.R")

# input directory
pop_60 <- "D:/sanwit/data/worldPop/age_sex/age_60"
pop_10 <- "D:/sanwit/data/worldPop/age_sex/age_14"
pop_10_30 <- "D:/sanwit/data/worldPop/age_sex/age_10_30"
pop_30_60 <- "D:/sanwit/data/worldPop/age_sex/age_30_60"


# list file
pop_60 <- list.files(path = pop_60, 
                           pattern='\\.tif$', 
                           all.files= T, 
                           full.names= T)

pop_10 <- list.files(path = pop_10, 
                     pattern='\\.tif$', 
                     all.files= T, 
                     full.names= T)

pop_10_30 <- list.files(path = pop_10_30, 
                     pattern='\\.tif$', 
                     all.files= T, 
                     full.names= T)

pop_30_60 <- list.files(path = pop_30_60, 
                        pattern='\\.tif$', 
                        all.files= T, 
                        full.names= T)

# read data
pop_60r <- rast(pop_60)
pop_10r <- rast(pop_10)

pop_10_30r <- rast(pop_10_30)

pop_30_60r <- rast(pop_30_60)

pop_total <- rast("D:/sanwit/data/worldPop/pop_number_100m/chn_ppp_2020_constrained.tif")

# crop to Xiamen city
pop_10xm <- crop(pop_10r, city_4326)
pop10_30xm <- crop(pop_10_30r, city_4326)
pop30_60xm <- crop(pop_30_60r, city_4326)
pop_60xm <- crop(pop_60r, city_4326)

pop_count_xm <- crop(pop_total, city_4326)

# summarize to total number
pop_60sum <- app(pop_60xm, fun=sum)
pop_10sum <- app(pop_10xm, fun=sum)
pop_10_30sum <- app(pop10_30xm, fun=sum)
pop_30_60sum <- app(pop30_60xm, fun=sum)

# stack
pop_sen <- c(pop_60sum,pop_60sum)

pop_sens <- app(pop_sen, fun=sum)

writeRaster(pop_60sum,"E:/ES_Demand_Supply/data/xiamen/world_pop/pop60.tif",
            overwrite = TRUE)

writeRaster(pop_10sum,"E:/ES_Demand_Supply/data/xiamen/world_pop/pop10.tif")

writeRaster(pop_sens,"E:/ES_Demand_Supply/data/xiamen/world_pop/pop_sens.tif")
writeRaster(pop_10_30sum,"E:/ES_Demand_Supply/data/xiamen/world_pop/pop_10_30y.tif")
writeRaster(pop_30_60sum,"E:/ES_Demand_Supply/data/xiamen/world_pop/pop_30_60y.tif")

writeRaster(pop_count_xm,"E:/ES_Demand_Supply/data/xiamen/world_pop/pop_count.tif")
