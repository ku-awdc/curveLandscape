
#'
#'
#' @inheritParams sim_bdm
#'
ssa_source_only_wedge <- function(
  n0,
  birth_baseline,
  death_baseline,
  carrying_capacity,
  migration_baseline,
  t_max,
  reps = 100) {
map(
  seq_len(reps), \(rep) {
    sim_bdm(
      n0 = n0,
      birth_baseline = birth_baseline,
      death_baseline = death_baseline,
      carrying_capacity = carrying_capacity,
      migration_baseline = migration_baseline,
      t_max = t_max
    ) %>%
      as_tibble() %>%
      mutate(repetition = rep)
  }
) %>%
  bind_rows() %>%
  rename(N = count) %>%
  mutate(id_patch = id_patch + 1)
}

ssa_source_only_smooth <- function() {
  stop("unimplemented")
}