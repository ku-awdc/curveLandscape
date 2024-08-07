## code to prepare `habitat_map` dataset goes here
library("hexscape")
library("sf")
library("units")
# library("pbapply")

#' TODO:  look at ?st_interpolate_aw to simplify first stage (Corine -> Hexagons) - density version

#' Take corine data and simplify to non-habitable / low-habitable / high-habitable:
load_corine("DK032") |>
  mutate(Habitat = case_when(
    CLC_Label2 == "Forests" ~ "High",
    CLC_Label2 == "Scrub and/or herbaceous vegetation associations" ~ "Low",
    TRUE ~ "Non"
  )) |>
  group_by(NUTS, Habitat) |>
  summarise(
    Area = sum(Area), Area_simplified = sum(Area_simplified),
    geometry = st_union(geometry), .groups = "drop"
  ) ->
habitat_map

usethis::use_data(habitat_map, overwrite = TRUE)
