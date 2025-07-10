library(rio)
library(tidyverse)


weather <- import("E:/ES_Demand_Supply/data/xiamen/Tmrt/weather/urban_core/processed.xlsx")

head(weather)
str(weather)

summary(weather)

hottest <- weather |> 
            filter(
              datetime_lst >= "2020-06-22 00:00" & datetime_lst <= "2020-06-22 23:00"
              )

summary(hottest)
