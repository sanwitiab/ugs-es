library(rio)
library(tidyverse)
library(flextable)

dtu <- import("E:/ES_Demand_Supply/doc/dataused.csv")

dtu = dtu |> 
  mutate(
    Year = as.character(Year)
  )

dataused <- flextable(dtu)
  
dataused <- set_table_properties(
  dataused,  
  align = "center"
  )

plot(dataused)
