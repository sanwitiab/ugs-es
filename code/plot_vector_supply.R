corrplot(
  sup_cor$correlation,
  order = "AOE",
  type = 'lower',
  addCoef.col = 'grey10',
  col = COL2(n=10)
)


corrplot(
  sup_cor$correlation,
  order = 'AOE',
  type = 'lower',
  tl.pos = 'tp',
  title = '(d)',
  tl.srt = 45,
  cl.ratio = 0.2
)
corrplot(
  sup_cor$correlation,
  add = TRUE,
  type = 'upper',
  method = 'number',
  col = 'black',
  order = 'AOE',
  diag = FALSE,
  tl.pos = 'n',
  cl.pos = 'n'
)

corrplot(
  sup_cor$correlation,
  method = 'circle',
  type = 'lower',
  # insig = 'blank',
  addCoef.col = 'black',
  number.cex = 0.8,
  order = 'AOE',
  diag = FALSE
)

mar=c(0,0,1,0)
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
         built_sup <- vect("E:/ES_Demand_Supply/result/ugs_supply_index.shp")
         
)
