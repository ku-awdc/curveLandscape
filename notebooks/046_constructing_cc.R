devtools::load_all()
library("hexscape")
library("sf")
library("units")
library("pbapply")

# create_habitat_centred(6.5) -> aaa

# aaa$Area %>%
#   set_units("km^2") %>%
#   hist(breaks = 25, probability=TRUE)


naive_grid5 <- create_naive_grid(5) %>% 
  mutate(
    cc = as.numeric(Area) * 0.75 * Prop_High + 
      as.numeric(Area) * 0.25 * Prop_Low
  ) %>% 
  # summarise(total_cc = sum(cc)) %>% 
  identity()



dk_patches <- load_map("DK032")
ggplot() +
  # geom_sf(data = habitat_map, aes(fill = Habitat)) +
  geom_sf(data = dk_patches, fill = NA, linetype = "dotted") +
  geom_sf(data = boar_centric_patches, fill = NA) +
  geom_sf(
    data =
      create_naive_grid(5), fill = NA
  ) +
  scale_fill_manual(
    values = c(
      "High" = "forestgreen",
      "Low" = "wheat",
      "Non" = "grey"
    )
  ) +
  coord_sf(expand = FALSE) +
  ggpubr::theme_pubclean(base_size = 15)


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
    par = c(alpha_High = 2, alpha_Low = 1, alpha_None = 0),
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
          sum(pmax(0, area_high + area_low + area_none))
        })

        abs(
          # (total - pmax(0, target_pop_area * alpha[1]))
          (total - pmax(0, target_pop_area * 1 / 2.5))
        )
      })
    }
  )
}

calibrate_naive_grid(create_naive_grid(50))
calibrate_naive_grid(create_naive_grid(25))
calibrate_naive_grid(create_naive_grid(20))
calibrate_naive_grid(create_naive_grid(10))

calib_output <- tibble(
  cellsize_km2 = 5:50
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
        as.numeric()) * 1 / 2.5)
    }),
    total_naive_grid = pmap_dbl(select(., naive_grid, alpha_High, alpha_Low, alpha_None), \(naive_grid, alpha_High, alpha_Low, alpha_None) {
      naive_grid$area <- naive_grid$Area %>% as.numeric()
      total <- with(naive_grid, {
        area_high <- alpha_High * Prop_High * area
        area_low <- alpha_Low * Prop_Low * area
        area_none <- alpha_None * Prop_None * area
        sum(pmax(0, area_high + area_low + area_none))
      })
      total
    }),
    naive_grid = pmap(select(., naive_grid, alpha_High, alpha_Low, alpha_None), \(naive_grid, alpha_High, alpha_Low, alpha_None) {
      naive_grid$area <- naive_grid$Area %>% as.numeric()
      with(naive_grid, {
        area_high <- alpha_High * Prop_High * area
        area_low <- alpha_Low * Prop_Low * area
        area_none <- alpha_None * Prop_None * area
        naive_grid$cc <- pmax(0, area_high + area_low + area_none)
        naive_grid
      })
    }),
  ) ->
calib_output

calib_output %>%
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
#'
#'
#'
calib_output$naive_grid[[1]]$cc %>%
  density() %>%
  plot()
calib_output$naive_grid[[2]]$cc %>%
  density() %>%
  plot()

calib_output$naive_grid[[5]]$cc %>%
  density() %>%
  plot()

calib_output$naive_grid[[4]]$cc %>%
  zapsmall() %>%
  {
    table(. == 0)
  }

calib_output$naive_grid[[4]]$cc %>% sum()

calib_output$total_habitat_island[[4]]

boar_centric_patches %>%
  glimpse()

naive_grid5 <- create_naive_grid(5) %>%
  mutate(area = Area %>% as.numeric())
par_naive_grid5 <- calibrate_naive_grid(naive_grid5)
naive_grid5 <- naive_grid5 %>%
  mutate(cc = with(as.list(par_naive_grid5$par), {
    area_high <- alpha_High * Prop_High * area
    area_low <- alpha_Low * Prop_Low * area
    area_none <- alpha_None * Prop_None * area
    pmax(0, area_high + area_low + area_none)
  }))

naive_grid5 %>%
  mutate(is_empty = zapsmall(cc) == 0) %>%
  identity() -> naive_grid5
{
  ggplot() +
    geom_sf(data = dk_patches, fill = NA) +
      # geom_sf(data = naive_grid5, aes(fill = is_empty)) +
        geom_sf(data = naive_grid5 %>% filter(!is_empty), aes(fill = cc), alpha = .5) +
    geom_sf(data = boar_centric_patches, fill = NA) +
    # scale_fill_manual(values = list(
    #   "TRUE" = "white",
    #   "FALSE" = "forestgreen"
    # )) +
    scale_fill_viridis_c(direction = -1) +
    coord_sf(expand = TRUE) +
    ggpubr::theme_pubclean()
}


# ggplot() +
#   geom_sf(data = dk_patches, fill = NA) +
#   geom_sf(data = boar_centric_patches, fill = NA) +
#   # ggplot2::geom_sf_text(
#   #   data = boar_centric_patches,
#   #   aes(
#   #     geometry = st_centroid(geometry),
#   #     label = str_c(PatchID, ":", MainPatch, ":", SubPatch)
#   #   ),size = 0.9
#   # ) +
#   ggpubr::theme_pubclean()
