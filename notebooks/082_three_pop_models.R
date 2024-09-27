devtools::load_all()

library(tidyverse)

## 3-patch scenario:
### patches have 0.75 a/km2, 0.25 a/km2, 0.1 a/km2
dens <- c(0.75, 0.5, 0.25)
### Sizes are 1,2,5,10,25
sizes <- c(1,2,5,10,25)
lapply(sizes, \(x) x*dens)

#ccs <- c(19.9,7.9,9.9)
#n0s <- c(10,0,0)

ccs <- dens*sizes[2]
ccs <- c(10,0.5,0.5)
n0s <- c(sum(ccs),1,1)

res <- all_models_patch_level(ccs, n0s)
ggplot(res |> filter(Iteration==0), aes(x=Time, y=N, col=Type)) +
  geom_line() +
  facet_grid(Patch~Model, scales="free_y")

ode_migration_only(
  growth_rate = 4 - 1,
  carrying_capacity = ccs,
  n0 = n0s,
  migration_offset = 0,
  migration_baseline = 1 / (8 / 12),
  migration_intercept = 0,
  delta_t = 1 / (12 * 4),
  t_max = 5
)

ode_source_only_smooth(
  growth_rate = 4 - 1,
  carrying_capacity = ccs,
  n0 = n0s,
  migration_offset = 0,
  migration_baseline = 1 / (8 / 12),
  migration_intercept = 0,
  delta_t = 1 / (12 * 4),
  t_max = 5
) -> deter_output

#' Add carrying capacity information for plotting...
deter_output <- deter_output %>%
  mutate(carrying_capacity = carrying_capacity[as.integer(id_patch)])

deter_output %>%
  identity() %>%
  {
    ggplot(.) +
      aes(time, N, group = id_patch) +
      geom_line(
        # alpha = 0.5,
        aes(color = factor(id_patch)),
        show.legend = FALSE,
        # linetype = "dashed"
      ) +
      # geom_line(
      #   data = ssa_output,
      #   alpha = 0.1,
      #   aes(
      #     group = str_c(id_patch, repetition),
      #     color = factor(id_patch)
      #   ),
      #   show.legend = FALSE
      # ) +
      # stat_smooth(
      #   data = ssa_output,
      #   aes(
      #     color = factor(id_patch)
      #   ),
      #   linewidth = .8,
      #   se = FALSE
      # ) +
      labs(
        color = NULL,
        y = "Patch-level count",
        x = "time [years]"
      ) +
      geom_hline(
          yintercept = ccs,
        color = "forestgreen",
        linetype = "dotdash",
        # linewidth = 0.3
      ) +

      # scale_colour_manual(
      #   values = c(
      #     "ODE" = "black",
      #     "Stochastic" = "lightblue"
      #   )
      # ) +
      # expand_limits(y = c(0, 2, 8)) +
      #ylim(c(NA, 10)) +
      theme_bw(base_size = 15) +
      # ggpubr::theme_transparent() +
      theme(legend.position = "bottom") +
      NULL
  }




result <- list(
  static = NA,
  wedge = NA,
  smooth = NA
)

n0 <- n0s |> as.integer()
birth_baseline <- rep.int(4, times = length(n0))
death_baseline <- rep.int(1, times = length(n0))
carrying_capacity <- ccs
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

models$nomig <-
  WildSSA$new_static(n0, birth_baseline, death_baseline, carrying_capacity, 0.0, 0.0)

models$static <-
  WildSSA$new_static(n0, birth_baseline, death_baseline, carrying_capacity, 0.0, 0.0)

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

if(FALSE){
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

all_patch_result_df |>
  group_by(repetition, time, migration_scheme) |>
  mutate(total = sum(patch_count)) |>
  ungroup() |>
  summary()


all_patch_result_df %>%
  group_by(repetition, time, migration_scheme) |>
  mutate(total = sum(patch_count)) |>
  #filter(total > 0) |>
  ungroup() |>
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
