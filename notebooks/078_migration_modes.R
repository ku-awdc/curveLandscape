devtools::load_all()


all_landscapes


result <- list(
  static = NA,
  wedge = NA,
  smooth = NA
)

n0 <- as.integer(c(1, 2, 3))
birth_baseline <- rep.int(4, times = length(n0))
death_baseline <- rep.int(1, times = length(n0))
carrying_capacity <- c(5, 2, 20)
migration_intercept <- 0
migration_baseline <- 1 / (8 / 12)

# simulation settings
t_max <- 10
delta_t <- 1 / 12
fixed_times <- seq.default(0, t_max, by = delta_t)
repetitions <- 250
seed <- 20240825

# sanity check
stopifnot(length(n0) == length(carrying_capacity))

models <- result

models$static <-
  WildSSA$new_static(n0, birth_baseline, death_baseline, carrying_capacity, migration_intercept, migration_baseline)

models$wedge <-
  WildSSA$new_wedge(n0, birth_baseline, death_baseline, carrying_capacity, migration_intercept, migration_baseline)

models$smooth <-
  WildSSA$new_smooth(n0, birth_baseline, death_baseline, carrying_capacity, migration_intercept, migration_baseline)


result$static <- models$static$run_and_record_fixed_time_population_par(fixed_time_points = fixed_times, t_max, repetitions, seed)
result$wedge <- models$wedge$run_and_record_fixed_time_population_par(fixed_time_points = fixed_times, t_max, repetitions, seed)
result$smooth <- models$smooth$run_and_record_fixed_time_population_par(fixed_time_points = fixed_times, t_max, repetitions, seed)


all_result_df <-
  result %>%
  enframe("migration_scheme", "output") %>%
  unnest_longer(output) %>%
  unnest_wider(output) %>%
  select(-current_time, -current_count, -current_time_index) %>%
  rename(population_count = count) %>%
  mutate(id_time = list(seq_along(fixed_times))) %>%
  unnest(c(id_time, time, population_count))

all_result_df %>%
  glimpse() %>%
  mutate(migration_scheme = fct_inorder(migration_scheme)) %>%
  {
    ggplot(.) +
      aes(time, population_count, group = str_c(repetition, migration_scheme)) +
      geom_line() +
      labs(y = "Total population") +
      geom_hline(data = tibble(carrying_capacity = sum(carrying_capacity)), aes(
        yintercept = carrying_capacity,
        color = "Carrying Capacity",
      )) +
      labs(color = NULL) +
      facet_wrap(~migration_scheme) +
      ggpubr::theme_pubclean(15) +
      NULL
  }


all_result_df %>%
  glimpse() %>%
  reframe(
    .by = c(migration_scheme, id_time),
    time = time[1],
    mean = mean(population_count),
    ci_lower = quantile(population_count, probs = 0.025),
    ci_upper = quantile(population_count, probs = 1 - 0.025)
  ) %>%
  select(-id_time) %>%
  mutate(migration_scheme = fct_inorder(migration_scheme)) %>%
  {
    ggplot(.) +
      aes(time, population_count, group = str_c(migration_scheme)) +
      geom_line(aes(time, mean)) +
      geom_line(linetype = "dotted", aes(time, ci_lower)) +
      geom_line(linetype = "dotdash", aes(time, ci_upper)) +
      # geom_line() +
      labs(y = "Total population") +
      geom_hline(data = tibble(carrying_capacity = sum(carrying_capacity)), aes(
        yintercept = carrying_capacity,
        color = "Carrying Capacity",
      )) +
      labs(color = NULL) +
      facet_wrap(~migration_scheme, ncol = 1, strip.position = "right") +
      ggpubr::theme_pubclean(15) +
      ggpubr::labs_pubr() +
      NULL
  }

#' Simulate and retrieve pr. patch records
#'

patch_result <- list()

devtools::load_all()

# patch_result$static <- models$static$run_and_record_fixed_time_patches(fixed_time_points = fixed_times, t_max, repetitions, seed)
# patch_result$wedge <- models$wedge$run_and_record_fixed_time_patches(fixed_time_points = fixed_times, t_max, repetitions, seed)
# patch_result$smooth <- models$smooth$run_and_record_fixed_time_patches(fixed_time_points = fixed_times, t_max, repetitions, seed)

patch_result$static <- models$static$run_and_record_fixed_time_patches_par(fixed_time_points = fixed_times, t_max, repetitions, seed)
patch_result$wedge <- models$wedge$run_and_record_fixed_time_patches_par(fixed_time_points = fixed_times, t_max, repetitions, seed)
patch_result$smooth <- models$smooth$run_and_record_fixed_time_patches_par(fixed_time_points = fixed_times, t_max, repetitions, seed)

# patch_result$static[[1]]$count
# patch_result$static[[2]]$count
# patch_result$static[[1]]$count

# patch_result$wedge %>% transpose() %>% `[[`("count") %>% unlist() %>% {
#   .[print(which(. > 10000))]
# }

# DEBUG
# patch_result$static %>% length()
# patch_result$static[[1]] %>% length()
# patch_result$static[[1]] %>% names()
# patch_result$static[[1]] %>% lengths()

all_patch_result_df <- patch_result %>%
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

# fixed_times %>% length()
# repetitions
# 3 * 301 * 250 * 3


all_patch_result_df %>%
  glimpse() %>%
  reframe(
    .by = c(migration_scheme, id_patch, time),
    time = time[1],
    mean = mean(patch_count),
    ci_lower = quantile(patch_count, probs = 0.025),
    ci_upper = quantile(patch_count, probs = 1 - 0.025)
  ) %>%
  # select(-id_time) %>%
  mutate(migration_scheme = fct_inorder(migration_scheme)) %>%
  {
    ggplot(.) +
      aes(time, patch_count, group = str_c(migration_scheme, id_patch)) +
      geom_line(aes(time, mean, color = fct(as.character(id_patch)))) +
      # geom_line(linetype = "dotted", aes(time, ci_lower, color = fct(as.character(id_patch)))) +
      # geom_line(linetype = "dotted", aes(time, ci_upper, color = fct(as.character(id_patch)))) +
      # geom_line() +
      labs(y = "Total population") +
      geom_hline(data = . %>% mutate(carrying_capacity = carrying_capacity[as.integer(id_patch)]), aes(
        yintercept = carrying_capacity,
        color = "Carrying Capacity",
      ), linetype = "dashed", linewidth = 1.1) +
      labs(color = NULL) +
      labs(linetype = NULL) +
      guides(color = guide_legend(override.aes = list(linewidth = 1.5), linetype = NULL)) +
      facet_wrap(~migration_scheme, ncol = 1, strip.position = "right") +
      # facet_wrap(id_patch~migration_scheme, strip.position = "right") +
      ggpubr::theme_pubclean() +
      ggpubr::labs_pubr() +
      # theme_bw() +
      NULL
  }
