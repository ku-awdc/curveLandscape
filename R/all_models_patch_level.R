#' Get results for all 4 models (ODE and SSA) at patch level
#'
#' @param carrying_capacity vector of carrying capacity
#' @param n0 vector of starting numbers
#'
#' @export
all_models_patch_level <- function(carrying_capacity, n0=carrying_capacity, t_max=5, repetitions = 250){

  stopifnot(length(carrying_capacity)==length(n0))

  n0 <- as.integer(n0)
  growth_rate <- 4-1
  birth_baseline <- rep.int(4, times = length(n0))
  death_baseline <- rep.int(1, times = length(n0))
  migration_offset <- 0
  migration_baseline <- 1 / (8 / 12)
  migration_intercept <- 0
  delta_t <- 1 / (12 * 4)
  fixed_times <- seq.default(0, t_max, by = delta_t)


  ## ODEs
  bind_rows(
    ode_migration_only(
      growth_rate = growth_rate,
      carrying_capacity = carrying_capacity,
      n0 = n0,
      migration_offset = 0,
      migration_baseline = 0,
      migration_intercept = 0,
      delta_t = delta_t,
      t_max = t_max
    ) |> mutate(Model = "NoMigration"),
    ode_migration_only(
      growth_rate = growth_rate,
      carrying_capacity = carrying_capacity,
      n0 = n0,
      migration_offset = migration_offset,
      migration_baseline = migration_baseline,
      migration_intercept = migration_intercept,
      delta_t = delta_t,
      t_max = t_max
    ) |> mutate(Model = "Static"),
    ode_source_only_wedge(
      growth_rate = growth_rate,
      carrying_capacity = carrying_capacity,
      n0 = n0,
      migration_offset = migration_offset,
      migration_baseline = migration_baseline,
      migration_intercept = migration_intercept,
      delta_t = delta_t,
      t_max = t_max
    ) |> mutate(Model = "Wedge"),
    ode_source_only_smooth(
      growth_rate = growth_rate,
      carrying_capacity = carrying_capacity,
      n0 = n0,
      migration_offset = migration_offset,
      migration_baseline = migration_baseline,
      migration_intercept = migration_intercept,
      delta_t = delta_t,
      t_max = t_max
    ) |> mutate(Model = "Smooth"),
  ) |>
  select(Time = time, Patch = id_patch, N, Model) |>
  mutate(Type = "ODE", Iteration=0) ->
  ode_results

#  yints <- tibble(CC = carrying_capacity) |> mutate(Patch = row_number())
#  ggplot(ode_results, aes(x=Time, y=N, col=Model)) +
#    geom_line() +
#    facet_wrap(~Patch) +
#    geom_hline(aes(yintercept=CC), yints, lty="dashed")

  ## SSAs
  models=result <- list(
    nomig = NA,
    static = NA,
    wedge = NA,
    smooth = NA
  )

  models$nomig <-
    WildSSA$new_static(n0, birth_baseline, death_baseline, carrying_capacity, 0.0, 0.0)
  models$static <-
    WildSSA$new_static(n0, birth_baseline, death_baseline, carrying_capacity, migration_intercept, migration_baseline)
  models$wedge <-
    WildSSA$new_wedge(n0, birth_baseline, death_baseline, carrying_capacity, migration_intercept, migration_baseline)
  models$smooth <-
    WildSSA$new_smooth(n0, birth_baseline, death_baseline, carrying_capacity, migration_intercept, migration_baseline)

  result$nomig <- models$nomig$run_and_record_fixed_time_patches_par(fixed_time_points = fixed_times, t_max, repetitions, seed)
  result$static <- models$static$run_and_record_fixed_time_patches_par(fixed_time_points = fixed_times, t_max, repetitions, seed)
  result$wedge <- models$wedge$run_and_record_fixed_time_patches_par(fixed_time_points = fixed_times, t_max, repetitions, seed)
  result$smooth <- models$smooth$run_and_record_fixed_time_patches_par(fixed_time_points = fixed_times, t_max, repetitions, seed)

  all_patch_result_df <- result %>%
    enframe("migration_scheme", "output") %>%
    unnest_longer(output) %>%
    unnest_wider(output) %>%
    select(-current_time, -current_count, -current_time_index, -fixed_time) %>%
    rename(patch_count = count) %>%
    # mutate(id_time = list(seq_along(time[[1]]))) %>%
    # unnest(c(id_time, time, id_patch, patch_count)) %>%
    unnest(c(time, id_patch, patch_count)) %>%
    mutate(id_patch = id_patch + 1) %>%
    # mutate(id_time = rep())
    identity()

  all_patch_result_df |>
    mutate(Iteration = repetition+1) |>
    mutate(Model = case_match(migration_scheme,
      "nomig" ~ "NoMigration",
      "static" ~ "Static",
      "wedge" ~ "Wedge",
      "smooth" ~ "Smooth"
    ), Type = "SSA") |>
    select(Time = time, Patch = id_patch, N = patch_count, Model, Iteration, Type) ->
    SSA_results

  bind_rows(
    ode_results |> mutate(Patch = as.integer(Patch)),
    SSA_results
  ) ->
    comb_results

  comb_results |>
    group_by(Time, Model, Type, Iteration) |>
    mutate(N = sum(N), Patch=0) |>
    ungroup() |>
    bind_rows(comb_results) ->
    comb_results

  comb_results |>
    filter(Type=="SSA") |>
    group_by(Time, Model, Patch, Type) |>
    summarise(LCI = quantile(N, 0.025), UCI = quantile(N, 0.975), N = mean(N), Iteration=0, .groups="drop") |>
    bind_rows(
      comb_results |> mutate(LCI=NA_real_, UCI=NA_real_)
    ) |>
    mutate(Model = factor(Model, levels=c("NoMigration", "Static","Smooth","Wedge")))
}
