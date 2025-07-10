# https://r-graph-gallery.com/web-choropleth-barchart-map.html

library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)
library(readr)
library(sf)

recreation_hotspot

ggplot(recreation_hotspot) +
  geom_sf(aes(fill = recre_sup), lwd = 0.5, color="white")

ggplot(recreation_hotspot) +
  geom_sf(aes(fill = recre_dem), lwd = 0.5, color="white")

ggplot(recreation_hotspot) +
  geom_sf(aes(fill = recre_sup), lwd = 0.05, color = "grey") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(0.65, 0.95, 0.05),
    direction = 1,
    palette = "YlGnBu"
  )

rmap <- ggplot(recreation_hotspot) +
  geom_sf(aes(fill = recre_sup), lwd = 0.05, color = "grey") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(0.65, 0.95, 0.05),
    direction = 1,
    palette = "YlGnBu"
  ) +
  labs(
    title = "Recreation supply, 2020",
    subtitle = "county recreation supply in Xiamen",
    caption = "Source: IUE"
  ) +
  theme_map() +
  theme(
    #Legend
    legend.position = "top",
    legend.justification = 0.5,
    legend.key.size = unit(1.25, "cm"),
    legend.key.width = unit(1.75, "cm"),
    legend.text = element_text(size = 12),
    legend.margin = margin(),
    # Increase size and horizontal alignment of the both the title and subtitle
    plot.title = element_text(size = 28, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
rmap

# Calculate population share in each HDI group
pop_re <- recreation_hotspot |> 
  st_drop_geometry() |> 
  mutate(
    group_re = findInterval(recre_sup, seq(0.65, 0.95, 0.05), left.open = FALSE),
    group_re = factor(group_re)
  ) |> 
  group_by(group_re) |> 
  summarise(score = sum(常人口, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(share = score / sum(score) * 100) |> 
  na.omit()

ggplot(pop_re, aes(group_re, share, fill = group_re)) +
  geom_col()

# Create a variable to store the position of the text label
pop_re <- pop_re |> 
  mutate(
    y_text = if_else(group_re %in% c(0, 7), share + 3, share - 3),
    label = paste0(round(share, 1), "%")
  )

ggplot(pop_re, aes(group_re, share, fill = group_re)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  # Text labels
  geom_text(
    aes(y = y_text, label = label, color = group_re),
    size = 3
  ) +
  coord_flip() +
  # Use the same color palette as the map
  scale_fill_brewer(palette = "YlGnBu") +
  # Swap between black and white text
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  # Hide color legend
  guides(fill = "none", color = "none")

