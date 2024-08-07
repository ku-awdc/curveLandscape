devtools::load_all()

# create_habitat_centred(6.5) -> aaa

# aaa$Area %>%
#   set_units("km^2") %>%
#   hist(breaks = 25, probability=TRUE)

# ggplot() +
#   geom_sf(data = dk_patches, fill = NA, linetype = "dotted") +
#   geom_sf(data = create_habitat_centred(10), fill = NA) +
#   geom_sf(
#     data =
#       create_naive_grid(6.5), fill = NA
#   ) +
#   coord_sf(expand = FALSE) +
#   ggpubr::theme_pubclean(base_size = 15)


boar_centric_patches %>%
  group_by(MainPatch) %>%
  summarise() ->
# summarise(geometry = st_union(geometry)) ->
habitat_islands

ggplot() +
  geom_sf(data = dk_patches, fill = NA, linetype = "dotted") +
  geom_sf(data = habitat_islands, fill = "forestgreen") +
  geom_sf(
    data =
      create_naive_grid(25), fill = NA
  ) +
  coord_sf(expand = FALSE) +
  ggpubr::theme_pubclean(base_size = 15)


# naive_grid <- create_naive_grid(25)



# cellsize <- as_units(15, "km^2")
# cellsize <- 1
# st_area(habitat_islands) %>%
#   set_units("km^2")

# set_units(15, "km^2")

# habitat_islands$geometry %>%
#   map(\(island){
#     st_make_grid(island %>% st_sfc(),
#       cellsize = set_units(15, "km^2")
#     )
#   })




target_pop_area <-
  (habitat_islands %>% st_union() %>% st_area() %>% set_units("km^2") %>% as.numeric())

calibrate_naive_grid <- function(naive_grid) {
  naive_grid <- naive_grid %>%
    st_drop_geometry() %>%
    mutate(area = as.numeric(Area))
  optim(
    par = c(alpha_High = 1, alpha_Low = 1 / 2, alpha_None = 0.1),
    fn = function(par) {
      with(as.list(par), {
        alpha <- par[1:3]

        if (alpha[1] < alpha[2]) {
          return(Inf)
        }

        total <- naive_grid %>%
          summarise(
            total = sum(alpha[1] * Prop_High * area + alpha[2] * Prop_Low * area + alpha[3] * Prop_None * area)
          ) %>%
          pull(1)

        sqrt(
          (total - target_pop_area * alpha[1])**2
        )
      })
    }
  )
}

# calibrate_naive_grid(create_naive_grid(50))
# calibrate_naive_grid(create_naive_grid(25))
# calibrate_naive_grid(create_naive_grid(20))
# calibrate_naive_grid(create_naive_grid(10))

calib_output <- tibble(
  cellsize_km2 = 1:50
) %>%
  mutate(
    result = map(
      .progress = TRUE,
      cellsize_km2, \(cellsize) {
        calibrate_naive_grid(create_naive_grid(cellsize))
      }
    )
  )


calib_output %>%
  hoist(result, "par", "value") %>%
  unnest_wider("par") %>%
  print(n = Inf)

calib_output$result[[1]]$par -> optimal_alpha



habitat_islands %>%
  st_area() %>%
  set_units("km^2") %>%
  as.numeric() %>%
  {
    area_km2 <- .
    sum(area_km2 * optimal_alpha[1])
  }

naive_grid %>%
  mutate(area = as.numeric(Area)) %>%
  st_drop_geometry() %>%
  summarise(
    total = sum(optimal_alpha[1] * Prop_High * area + optimal_alpha[2] * Prop_Low * area + optimal_alpha[3] * Prop_None * area)
  )
