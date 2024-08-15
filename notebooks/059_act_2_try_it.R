devtools::load_all()


#' Act 2: Investigate the properties of an emergent wild boar population in the landscape.
#'
#' For each landscape, find the number of animals, that need to be seeded, in order to attain a non-extinction rate of the population
#' of say 10%, within 1,2,3 years
#'
#'
t_max <- c(1, 2, 3)
total_n0 <- 1:45

migration_baseline <- 1 / (8 / 12)
repetitions <- 250
# t_max <- 25
delta_t <- 1 / 12

config_df <- expand_grid(
  t_max, total_n0,
) %>%
  rowid_to_column("config_id")

seed_locations_in_grid <- function(grid, total_n0) {
  stopifnot(
    !is.null(grid$Capacity),
    total_n0 >= 1
  )

  # some grids don't have capacity for one boar in any of the cells. circumvent that..
  

  current_cc <- grid$Capacity
  n0 <- vector("integer", length = length(current_cc))
  repeat {
    stopifnot(any(current_cc >= 1))

    location <- sample.int(
      n = length(current_cc),
      size = 1,
      replace = FALSE,
      prob = {
        weight <- current_cc
        weight[current_cc < 1] <- 0
        weight
      }
    )
    n0[location] <- n0[location] + 1
    total_n0 <- total_n0 - 1
    current_cc[location] <- current_cc[location] - 1
    if (total_n0 == 0) {
      break
    }
  }
  n0
}

# Amend simulation details to the object
all_landscapes <- structure(all_landscapes,
  simulation_configuration = list(
    migration_baseline = migration_baseline,
    repetitions = repetitions,
    t_max = list(t_max),
    delta_t = delta_t,
    total_n0 = list(total_n0)
  )
)

all_landscapes_trans <- purrr::transpose(all_landscapes)

config_df %>% 
  expand_grid(
    all_landscapes
  ) -> 
  with_landscape_config_df

with_landscape_config_df %>% 
  rowwise() %>% 
  group_map(\(landscape, key_unused) {
    # tibble(t = 2)

    p_landscape_caption <-
      labs(caption = glue("Landscape type {landscape$landscape_type}, with patch-size {landscape$set_patch_size} km^2, and total patches {landscape$n_len}."))
  
    n0 <- seed_locations_in_grid(landscape$grid[[1]], landscape$total_n0)
  
    wild_ssa_model <- WildSSA$new(
      n0 = n0,
      birth_baseline = rep.int(4, times = landscape$n_len),
      death_baseline = rep.int(1, times = landscape$n_len),
      carrying_capacity = landscape$grid$Capacity,
      migration_baseline = migration_baseline
    )



  }) %>% 
  bind_rows()

# for (id_landscape in seq_along(all_landscapes_trans)) {
#   landscape <- all_landscapes_trans[[id_landscape]]

#   p_landscape_caption <-
#     labs(caption = glue("Landscape type {landscape$landscape_type}, with patch-size {landscape$set_patch_size} km^2, and total patches {landscape$n_len}."))

#   n0 <- seed_locations_in_grid(landscape$grid, total_n0)

#   wild_ssa_model <- WildSSA$new(
#     n0 = n0,
#     birth_baseline = rep.int(4, times = landscape$n_len),
#     death_baseline = rep.int(1, times = landscape$n_len),
#     carrying_capacity = landscape$grid$Capacity,
#     migration_baseline = migration_baseline
#   )
# }
