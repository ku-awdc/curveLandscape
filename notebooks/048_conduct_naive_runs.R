devtools::load_all()

aggregated_patches <- read_rds("data/aggregated_patches.rds")

aggregated_patches <- aggregated_patches %>%
  enframe("name", "grid") %>%
  separate(name, c(NA, "cellsize_desc"), "_") %>%
  mutate(set_patch_size = str_extract(cellsize_desc, "(\\d+)km2", group = 1) %>% as.numeric()) %>%
  select(-cellsize_desc)

all_grids <- bind_rows(
  naive = naive_grids %>% as_tibble() %>% rename(grid = naive_grid) %>%
    mutate(grid = grid %>% map(. %>% rename(Capacity = cc))),
  habic = aggregated_patches,
  .id = "landscape_type"
)

all_grids

#' The map with 1km^2 doesn't look like it has 3k cells, but thus far, it looks like it.
#' 
all_grids$grid[[1]]$geometry %>% st_geometry_type() %>% table()
all_grids$grid[[1]]$geometry[[1]] %>% unclass() %>% `[[`(1) %>% nrow()
all_grids$grid[[1]]$geometry %>% st_area() %>% hist()

ggplot() +
  geom_sf(data=
    all_grids$grid[[1]], fill = NA) +
  NULL

aggregated_patches$grid[[4]] %>%
  print(n = Inf) %>%
  summarise(sum(Capacity))

all_grids %>%
  mutate(
    total_cc = grid %>% map_dbl(. %>% summarise(sum(Capacity)) %>% pull(1)),
    n_len = grid %>% map_int(nrow),
    #   cc_null = grid %>% map_int(. %>% st_drop_geometry() %>%
    #     summarise(sum(abs(zapsmall(Capacity)) <= 0.00001)) %>% pull(1))
  ) -> all_grids_stats

all_grids_stats

all_grids_stats %>%
  select(-grid) %>%
  pivot_wider(values_from = set_patch_size:n_len, names_from = landscape_type) %>%
  unnest() %>%
  identity() %>%
  print() %>%
  # tinytable::tt(theme = "void")  %>%
  # print("typst") %>%
  identity()


# ode_source_only(
#   growth_rate = 3, carrying_capacity = naive_grid$cc, n0, migration_baseline, delta_t,
#   t_max = 25
# )
