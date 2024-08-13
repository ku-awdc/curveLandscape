library(tmap)

tmap_mode("view")

naive_landscape <- all_landscapes$grid[[2]]
#' 1km sq grid does not have a single patch greater than 1.
#' 
#' 1283
#' 1325
#' 1326
#' 1303
#' 
#' in 2 km sq. grid (grid-based) has above 1 carrying capacity.
#' 
naive_landscape[
  c(1283, 1325, 1326, 1303),
]
naive_landscape %>% rowid_to_column() %>% tail()




tm_shape(naive_landscape %>% rowid_to_column(), "2km sq. hex grid") +
  tm_fill("Capacity") +
  tm_polygons() +
  tm_borders() +
  tm_text("rowid", size = 0.5) +
  NULL

