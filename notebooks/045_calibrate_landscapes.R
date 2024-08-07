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

        total <- with(naive_grid, {
          area_high <- alpha[1] * Prop_High * area
          area_low <- alpha[2] * Prop_Low * area
          area_none <- alpha[3] * Prop_None * area
          sum(area_high + area_low + area_none)
        })

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
        naive_grid <- create_naive_grid(cellsize)
        list(
          naive_grid = naive_grid,
          result = calibrate_naive_grid(naive_grid)
        )
      }
    )
  ) %>%
  unnest_wider(result)


calib_output %>%
  hoist(result, "par", "value") %>%
  unnest_wider("par") %>%
  print(n = Inf) ->
calib_output

calib_output %>%
  pivot_longer(
    starts_with("alpha_"),
    names_prefix = "alpha_", names_to = "HabitatType", values_to = "alpha"
  ) %>%
  identity() %>%
  {
    ggplot(.) +
      aes(cellsize_km2, alpha, group = HabitatType) +
      geom_line() +
      ggpubr::theme_pubclean() +
      NULL
  }

# calib_output$result[[1]]$par -> optimal_alpha


# habitat_islands %>%
#   st_area() %>%
#   set_units("km^2") %>%
#   as.numeric() %>%
#   {
#     area_km2 <- .
#     sum(area_km2 * optimal_alpha[1])
#   }

# naive_grid %>%
#   mutate(area = as.numeric(Area)) %>%
#   st_drop_geometry() %>%
#   summarise(
#     total = sum(optimal_alpha[1] * Prop_High * area + optimal_alpha[2] * Prop_Low * area + optimal_alpha[3] * Prop_None * area)
#   )

calib_output %>%
  mutate(
    total_habitat_island = map_dbl(alpha_High, \(alpha_High) {
      sum((habitat_islands %>%
        st_area() %>%
        set_units("km^2") %>%
        as.numeric()) * alpha_High)
    }),
    total_naive_grid = pmap_dbl(select(., naive_grid, alpha_High, alpha_Low, alpha_None), \(naive_grid, alpha_High, alpha_Low, alpha_None) {
      naive_grid$area <- naive_grid$Area %>% as.numeric()
      total <- with(naive_grid, {
        area_high <- alpha_High * Prop_High * area
        area_low <- alpha_Low * Prop_Low * area
        area_none <- alpha_None * Prop_None * area
        sum(area_high + area_low + area_none)
      })
      total
    })
  ) %>%
  glimpse() %>%
  identity() %>%
  {
    ggplot(.) +
      # aes(cellsize_km2, alpha, group = HabitatType) +
      geom_line(aes(cellsize_km2, total_habitat_island, color = "HC")) +
      geom_line(aes(cellsize_km2, total_naive_grid, color = "naive")) +
      ggpubr::theme_pubclean() +
      NULL
  }
