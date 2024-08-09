#' @param cellsize in km^2
#' @inheritParams sf::st_make_grid
create_naive_grid <- function(cellsize, ...) {
  dk_patches <- hexscape::load_map("DK032")

  # ggplot() +
  #   geom_sf(data = dk_patches, fill = NA) +
  #   coord_sf(expand = FALSE) +
  #   ggpubr::theme_pubr() +
  #   NULL

  dk_grid <- st_make_grid(
    dk_patches,
    cellsize = units::as_units(cellsize, "km^2"),
    ...
  ) %>%
    st_intersection(dk_patches)
  
  # sometimes we get a `POINT`, and we don't want that.
  dk_grid <- dk_grid[(st_geometry_type(dk_grid) == "POLYGON") |
    st_geometry_type(dk_grid) == "MULTIPOLYGON", ]

  # dk_grid
  # ggplot() +
  #   geom_sf(data = dk_patches, fill = NA) +
  #   geom_sf(data = dk_grid, fill = NA) +
  #   coord_sf(expand = FALSE) +
  #   ggpubr::theme_pubr() +
  #   NULL


  #' Assign proportion of high, low, and none to each cell in the grid.
  #'

  # note: the variable `habitat_map` comes from the package
  is_it_extensive <- TRUE
  dk_grid %>%
    st_sf() %>%
    mutate(Area = st_area(geometry) %>% units::set_units(value = "km^2")) %>%
    bind_cols(
      area_high = st_interpolate_aw(
        habitat_map[1, ] %>% select(Area = Area_simplified),
        dk_grid,
        extensive = is_it_extensive,
        keep_NA = TRUE
      ) %>%
        st_drop_geometry() %>%
        rename(Area_High = Area),
      area_low = st_interpolate_aw(
        habitat_map[2, ] %>% select(Area = Area_simplified),
        dk_grid,
        extensive = is_it_extensive,
        keep_NA = TRUE
      ) %>%
        st_drop_geometry() %>%
        rename(Area_Low = Area),
      st_interpolate_aw(
        habitat_map[3, ] %>% select(Area = Area_simplified),
        dk_grid,
        extensive = is_it_extensive,
        keep_NA = TRUE
      ) %>%
        st_drop_geometry() %>%
        rename(Area_None = Area)
    ) %>%
    replace_na(list(Area_High = 0, Area_Low = 0, Area_None = 0)) %>%
    mutate(
      Area_km2 = as.numeric(Area),
      Prop_High = Area_High / Area_km2,
      Prop_Low = Area_Low / Area_km2,
      Prop_None = Area_None / Area_km2,
      Area_High = NULL,
      Area_Low = NULL,
      Area_None = NULL,
      Area_km2 = NULL
    ) ->
  dk_grid_habitat

  dk_grid_habitat

  dk_grid_cells_with_only_none <- which(
    abs(dk_grid_habitat$Prop_High + dk_grid_habitat$Prop_Low) <= sqrt(.Machine$double.eps)
  )

  dk_grid_cells_with_only_none
  length(dk_grid_cells_with_only_none)

  # dk_grid %>% length()
  # dk_grid_habitat %>% nrow()
  # dk_grid_habitat[-dk_grid_cells_with_only_none, ] %>% nrow()
  # new_patches %>% nrow()

  dk_grid_reduced <- dk_grid_habitat[-dk_grid_cells_with_only_none, ]
  dk_grid_reduced
}
