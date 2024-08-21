test_that("equal to R's approx", {
  # R's approx(method = "constant", f = 0)
  # must agree with the `FixedPopulationRecorder` output in `WildSSA`

  # wild_ssa_model <- WildSSA$new_static(
  wild_ssa_model <- WildSSA$new_wedge(
    n0 = as.integer(c(4, 6, 2, 0)),
    birth_baseline = rep.int(4, times = 4),
    death_baseline = rep.int(1, times = 4),
    carrying_capacity = c(10, 20, 3, 2),
    migration_intercept = 0,
    migration_baseline = 1 / (8 / 12)
    # migration_baseline = 0
  )

  fixed_time <- seq.default(0, 5, by = 0.1)
  ssa_outputs_binned <-
    wild_ssa_model$run_and_record_fixed_time_population_par(
      fixed_time_points = fixed_time,
      t_max = 5,
      repetitions = 10,
      seed = 20240822
    )

  ssa_outputs <-
    wild_ssa_model$run_and_record_population_par(
      # fixed_time_points = fixed_time,
      t_max = 5,
      repetitions = 10,
      seed = 20240822
    )

  ssa_binned_outputs_true <- ssa_outputs %>%
    map(\(output) {
      approx(output$time, output$count, fixed_time, method = "constant")$y
    })

  ssa_outputs_binned[[1]]$count
  ssa_binned_outputs_true[[1]]

  map2_lgl(
    ssa_outputs_binned,
    ssa_binned_outputs_true,
    \(rust_version, r_version) {
      all(rust_version$count == r_version)
    }
  ) %>%
    all() %>%
    expect_true()
})
