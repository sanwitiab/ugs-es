# Heat mitigation map
hmi_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sup) +
  tm_fill(
    fill = "hmi_n",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      # labels = c("Very low (0.00 - 0.13)",
      #            "Low (0.13 - 0.35)",
      #            "Medium (0.35 - 0.52)",
      #            "High (0.52 - 0.72)",
      #            "Very high (0.72 - 1.00)"
      #           )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  # tm_text("name_pinyi", size = 0.45) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Heat mitigation index",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  tm_title_in('(a)',position = tm_pos_in("left","top")) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  # tm_credits("a) Heat mitigation index", group_id = "top") +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

# PM removal map
pm_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sup) +
  tm_fill(
    fill = "pmr_n",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      # labels = c("Very low (0.00 - 0.13)",
      #            "Low (0.13 - 0.35)",
      #            "Medium (0.35 - 0.52)",
      #            "High (0.52 - 0.72)",
      #            "Very high (0.72 - 1.00)"
      #           )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  # tm_text("name_pinyi", size = 0.45) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("PM2.5 removal",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_title_in('(b)',position = tm_pos_in("left","top")) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  # tm_credits("b) PM2.5 removal", group_id = "top") +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

# Nature access
nai_map = tm_shape(city) +
  tm_borders(col = "grey70", lwd = 1) +
  
  tm_shape(built_sup) +
  tm_fill(
    fill = "nai_n",
    fill.scale = tm_scale_intervals(
      style = "jenks",
      values = "-brewer.rd_yl_bu",
      # labels = c("Very low (0.00 - 0.13)",
      #            "Low (0.13 - 0.35)",
      #            "Medium (0.35 - 0.52)",
      #            "High (0.52 - 0.72)",
      #            "Very high (0.72 - 1.00)"
      #           )
    ),
    # col_alpha = 0.2,
    fill.legend = tm_legend("", group_id = "top")
  ) +
  
  # tm_text("name_pinyi", size = 0.45) +
  
  tm_shape(built_sup) +
  tm_borders(col="grey60" 
             ,lwd = 1) +
  
  tm_shape(built_area_district) +
  tm_borders(col = "black", lwd = 2) +
  
  tm_title("Urban nature access",
           size = 1,
           width = 10,
           group_id = "top",
           z = 0
  ) +
  
  tm_title_in('(c)',position = tm_pos_in("left","top")) +
  
  tm_layout(frame = FALSE) +
  
  tm_compass(group_id = "bottom") +
  
  tm_scalebar(group_id = "bottom", breaks = c(0, 5, 10)) +
  
  # tm_credits("c) Urban nature access", group_id = "top") +
  
  tm_comp_group("top", position = tm_pos_out("right", "center"), frame = FALSE, bg = FALSE,
  ) +
  
  tm_comp_group("bottom", position = tm_pos_in("right", "bottom", align.h = "center"))

# Supply correlation
supply_es <- rast("E:/ES_Demand_Supply/result/supply_es.tif")

sup_cor <- layerCor(
  supply_es,
  "cor",
  # w=3,
  use="pairwise.complete.obs"
)


# par(mar = c(4, 4, .1, .1))
par(mar = c(2,2, .1, .1))
print(hmi_map, main="(a)")
print(pm_map, main="(b)")
print(nai_map, main = "(c)")
corrplot(sup_cor$correlation,
         method = "circle",
         add = F,
         type = "lower",
         order = "AOE",
         hclust.method = "average",
         diag = T,
         col = COL2('RdBu', 10), # RdBu
         tl.srt = 0,
         tl.offset = 1,
         cl.ratio = 0.2,
         addCoef.col = "black",
         number.cex = 0.8,
         title = "(d)",
         mar=c(0,0,1,0)
)
