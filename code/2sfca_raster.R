library(spax)
library(terra)
library(sf)

# Load example data (already included in package)
pop <- read_spax_example("u5pd.tif") # Under-5 population density
distance <- read_spax_example("hos_iscr.tif") # Travel times
hospitals <- hc12_hos # Hospital locations and capacity

# Calculate accessibility using Enhanced 2SFCA
result <- spax_e2sfca(
  demand = pop, # Population density
  supply = hospitals |> st_drop_geometry(), # Hospital capacity
  distance = distance, # Travel times
  decay_params = list(
    method = "gaussian",
    sigma = 30 # 30-minute characteristic distance
  ),
  demand_normalize = "standard", # Prevent demand inflation
  id_col = "id",
  supply_cols = c("s_doc", "s_nurse") # Analyze both doctors and nurses
)

# Plot results
plot(result,   main = c("Access to Doctors", "Access to Nurses"),
     fun = function() lines(bound0)
)
