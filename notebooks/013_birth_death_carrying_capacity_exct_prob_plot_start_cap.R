yearly_prob_to_weekly_prob <- function(p_annual) {
  1 - (1 - p_annual)**(1 / 52)
}

rate_to_probability <- function(rate) {
  1 - exp(-rate)
}

# set.seed(20240412)


litter_size_distr <- c(2, 4, 13, 22.5, 17.5, 14, 9, 3) / 100
avg_litter_size <- sum(seq_along(litter_size_distr) * litter_size_distr)
avg_litter_size <- avg_litter_size / 2

# litter_size_distr %>% sum()
# litter_size0 <- c(`0` = 1 - sum(litter_size_distr), litter_size_distr)
# sample_litter_size <- function(n) {
#   sample.int(length(litter_size0),
#              size = n, replace = TRUE, prob = litter_size0) - 1
# }

birth <-  avg_litter_size
death <- 0.6540
# birth <- yearly_prob_to_weekly_prob(rate_to_probability(birth))
# death <- yearly_prob_to_weekly_prob(death)
c(birth = birth, death = death)

birth_baseline <- birth
death_baseline <- death

(birth_baseline - death_baseline) / (birth_baseline + death_baseline)

#' carrying capacity
k0 <- 80
# u0 <- seq.default(1, 12, by = 1)
# u0 %>% log1p()
# exp(-(0:12) + log(10)) %>% prettyNum()
# sqrt(c(0:10**2)) %>%
#   plot(y=rep.int(0, length(.)))
# sqrt(0:10**2)
u0 <- seq.default(1, 10, by = 1)

reps <- 20 * 5 * 2 * 2
max_t_years <- 25

if (any(k0 > u0)) {
  warning("Starting population larger than carrying capacity")
}

# max_t_weeks <- max_t_years * 52

# stopifnot(length(u0) == 1)
# TODO: maybe sort it
current_u <- u0 %>% rep.int(times = reps)
current_t <- numeric(length(current_u))
id_rep <- seq_along(current_u)
results <- cbind(rep = integer(0), t = numeric(0), u = numeric(0))
results <- rbind(
  results,
  cbind(rep = id_rep, t = current_t, u = current_u)
)
results_info <- list(
  u0 = tibble(id_u0 = seq_along(u0) %>% rep.int(reps),
              rep = id_rep, u0 = current_u)
)

repeat {

  stopifnot(all(!is.na(current_u),
                !is.na(current_t)))

  current_n <- length(current_u)
  next_unif <- runif(current_n)

  stopifnot(
    vapply(next_unif, \(x) !isTRUE(all.equal.numeric(x, 0)), FUN.VALUE = logical(1))
  )
  # stopifnot(current_u < k0)

  birth <- birth_baseline * (1 - (birth_baseline - death_baseline) / (birth_baseline + death_baseline) * current_u / k0)
  death <- death_baseline * (1 + (birth_baseline - death_baseline) / (birth_baseline + death_baseline) * current_u / k0)
  stopifnot(all(birth > 0), all(death > 0))

  delta_t <- -log(next_unif) / ((birth + death) * current_u)
  # ALTERNATIVE:
  # delta_t <- rexp(n = current_n, rate = (birth + death) * current_u)
  stopifnot(all(!is.infinite(delta_t)))
  stopifnot(all(delta_t >= 0))
  next_t <- current_t + delta_t

  # IDEA: use rexp instead you crazy person
  # birth <- yearly_prob_to_weekly_prob(rate_to_probability(birth))
  # death <- yearly_prob_to_weekly_prob(death)
  birth <- rate_to_probability(birth)
  death <- rate_to_probability(death)
  stopifnot(all(birth >= 0), all(death >= 0),
            all(birth <= 1), all(death <= 1))

  delta_u <- rbinom(n = current_n, size = 1, prob = birth / (birth + death))
  delta_u <- 2 * delta_u - 1
  next_u <- current_u + delta_u

  # shift
  current_u <- next_u
  current_t <- next_t

  # record
  results <- rbind(
    results,
    cbind(rep = id_rep, t = current_t, u = current_u)
  )

  # retire elapsed trajectories
  # elapsed_t <- which(current_t >= max_t_weeks)
  elapsed_t <- which(current_t >= max_t_years)
  elapsed_u <- which(current_u == 0)
  elapsed_reps <- unique(c(elapsed_t, elapsed_u))

  if (length(elapsed_reps) > 0) {
    # message(glue("{length(elapsed_u)}"))

    #add `t_max` to the `elapsed_u` group
    results <- rbind(
      results, cbind(rep = id_rep[elapsed_u],
                     # t = current_t[elapsed_u],
                     t = rep.int(max_t_years, length(elapsed_u)),
                     u = current_u[elapsed_u])
    )

    # remove the elapsed simulations from consideration
    current_u <- current_u[-elapsed_reps]
    current_t <- current_t[-elapsed_reps]
    id_rep <- id_rep[-elapsed_reps]
  }
  stopifnot(length(current_u) == length(current_t),
            length(current_t) == length(id_rep)
  )
  if (length(current_u) == 0) {
    break
  }
}
stopifnot(length(id_rep) == 0)

# current_t
# current_u
# id_rep
results <- results |>
  as_tibble() %>%
  left_join(
    results_info %>% bind_rows(),
    by = join_by(rep)
  )
u0
#'
#' Cache the result
fs::dir_create(".cache")
results %>% write_rds(".cache/13_results_with_info.RDS")


glm_results <- glm(
  u ~ t + offset(-log(results$u0)),
  # u ~ factor(id_u0)*0 + t,
  data = results,
  family = poisson()
)
glm_results
glm_results %>% summary()
# glm_results <- glm(
#   u ~ t + 0,
#   offset = log(u0),
#   data = results,
#   family = poisson()
# )
# glm_results
# glm_results %>% summary()
# glm_results %>%
#   predict.glm(
#     newdata = tibble(t = 0.1),
#     terms = TRUE
#   ) %>%
#   # str()
#   as_tibble()

# predict.glm(glm_results) %>%
#   as_tibble()
# predict.glm(glm_results,
#             newdata = tibble(t = 0.1, u0 = 1)) %>%
#   as_tibble()
#
# glm_predict <-
#   expand_grid(
#     # t = seq.default(0, max_t_years, length.out = 100),
#     # u0 = seq.default(0, 10, by = 1)
#   ) %>%
#   bind_cols(
#     predict =
#       predict.glm(
#         glm_results,
#         newdata = .,
#         type = "response"
#       )
#   )
# results %>%
#   bind_cols(
#     predict = predict.glm(
#       glm_results, type = "response"
#     )
#   ) %>%
#   # nrow() %>%
# # glm_predict %>%
#   ggplot() +
#   aes(t, predict, group = id_u0) +
#
#   geom_line(aes(color = u0)) +
#   scale_color_binned() +
#
#   theme_bw(base_size = 14) +
#   NULL
#'
#'
p_traj <- results |>
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step() +
  # geom_line(color = "grey90", show.legend = FALSE) +
  geom_line(aes(alpha = u), show.legend = FALSE) +
  # geom_step(aes(alpha = u), show.legend = FALSE) +
  geom_hline(aes(yintercept = k0), linetype = "dotted") +

  scale_alpha_continuous(range = c(0.15)) +

  theme_bw()
#'
# p_traj
#'
#'
results %>%
  # glimpse()

  dplyr::filter(rep %in% sample(unique(rep), size = 10),
                .by = id_u0) %>%
  # distinct(rep)
  ggplot() +
  aes(t, u, group = rep) +
  # geom_step() +
  # geom_line(color = "grey90", show.legend = FALSE) +
  geom_line(aes(alpha = u0), show.legend = FALSE) +
  # geom_step(aes(alpha = u), show.legend = FALSE) +
  geom_hline(aes(yintercept = k0), linetype = "dotted") +

  scale_alpha_continuous(range = c(0.15)) +

  theme_bw()

# p_traj +
#   facet_grid(~u0) +
#   lims(u0 = c(0,10))
#'
# ode_times <- results$t %>% zapsmall() %>%  unique() %>% sort()
#'
#'

ode_results <- deSolve::ode(
  y = c(u = u0), times = seq.default(0, 25, by = 0.001),
  parms = list(r = birth_baseline - death_baseline, k0 = k0,
               birth_baseline = birth_baseline,
               death_baseline = death_baseline),
  func = function(times, y, parameters) {
    with(parameters, {
      dN <- r * y * (1 - y / k0)
      list(dN)
    })
  }
)
ode_results <-
  ode_results %>%
  unclass() %>%
  as_tibble() %>%
  rename(t = time) %>%
  pivot_longer(
    -t,
    names_pattern = "u(\\d+)",
    names_to = c("id_u0"),
    values_to = "u") %>%
  mutate(u0 = u0[as.integer(id_u0)],
         id_u0 = NULL)
#TODO
# p_traj +
#   # `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
#   geom_smooth(aes(group = NA, color = "smooth")) +
#   geom_line(aes(color = "ODE", group = id_u0), data = ode_results) +
#
#   labs(color = NULL) +
#   theme(legend.position = "bottom") +
#   NULL
#'
results %>%
  dplyr::filter(t<0)
#'
results %>%
  dplyr::filter(id_u0 == 5) %>%
  ggplot() +
  aes(t, u, group = rep) +
  # geom_line(aes(color = identity(u0)), show.legend = TRUE) +

  # `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  geom_smooth(aes(group = id_u0, color = id_u0)) +
  geom_line(aes(color = u0, group = u0), data = ode_results) +

  # scale_color_viridis_c(direction = -1) +
  scale_color_viridis_b(direction = -1) +

  labs(color = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  NULL
#'
#' Plotting P(extinction, t | u0)
#'
results %>%
  slice_max(t, by = rep) %>%
  summarise(
    `P(extinction)` = mean(u < 1),
    .by = c(u0)
  ) %>%
  identity() %>%
  ggplot() +
  aes(u0, `P(extinction)`) +
  geom_line() +
  # geom_line(aes(color = u0)) +
  # scale_colour_viridis_c(direction = -1) +
  # scale_color_viridis_b(direction = -1, n.breaks = 9) +

  labs(y = "P(extinct)",
       x = "u0",
       color = NULL) +
  theme_bw(base_size = 14) +
  NULL
#'
#'
#'
#TODO: I need to do this for each of these lines, and calculate the difference
#' between the two, although they do look very similar...
#'
results_approxfun <-
  tapply(
    X = results[, c("t", "u")],
    INDEX = results$rep,
    FUN = \(x) {
      # rule = 1:2 for extinct numbers make sense.
      approxfun(x$t, y = x$u, method = "constant")
    }
  )
#'
max_t_years
#'
# DEBUG
# curve(
#   results_approxfun[[40]](x), to = 100
# )
results_info %>% str()
traj_approx_df <-
  results_approxfun %>%
  enframe("id_rep", "traj_approxfun") %>%
  bind_cols(results_info)
stopifnot(!anyNA(traj_approx_df))
stopifnot(all(traj_approx_df$id_rep == traj_approx_df$rep))
traj_approx_df
# traj_approx_df %>% View()

t_linspace <- seq.default(0, max_t_years, length.out = 100)
id_time <- seq_along(t_linspace)
id_time_linspace <- rep.int(id_time, length(results_info$u0$u0))

outcome_linspace <-
  vapply(results_approxfun,
         \(traj_fun) traj_fun(t_linspace),
         FUN.VALUE = double(length(t_linspace)))
# outcome_linspace %>%
#   Matrix::Matrix() %>% `[`(1:10, 1:10)
outcome_linspace %>% dim()
# id_time_linspace %>% dim()
id_time_linspace %>% length()
u0_linspace <- rep(results_info$u0$u0, each = length(t_linspace))
u0_linspace %>% length()

prob_ext <- tapply(
  X = outcome_linspace,
  INDEX = list(u0 = u0_linspace, time = id_time_linspace),
  FUN = \(x) mean(x < 1)
)
prob_ext %>%
  str()
prob_ext %>%
  as_tibble()
prob_ext %>%
  as.data.frame.table() %>%
  as_tibble() %>%
  mutate(u0 = as.numeric(u0),
         time = as.numeric(time),
         time = t_linspace[time]) %>%
  identity() %>%
  ggplot() +
  aes(time, Freq, group = u0) +
  geom_line(aes(color = u0)) +
  # scale_colour_viridis_c(direction = -1) +
  scale_color_viridis_b(direction = -1, n.breaks = 9) +

  labs(y = "P(extinct)",
       x = "time",
       color = NULL) +
  theme_bw(base_size = 14) +
  NULL
#'
#'
#'
#'

