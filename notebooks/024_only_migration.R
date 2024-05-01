#'
#'
#'
#' This implements migration (of the first kind)
#'
#' m_(ij) = m_0 "kern"("distance"_(ij)") N_j
#'
#'
#'
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
birth_baseline <- 4
death_baseline <- 1

growth_baseline <- birth_baseline - death_baseline

mu_0 <- death_baseline
beta_0 <- birth_baseline

# migration baseline
m_baseline <- 0.15

dist_01 <- 0.2
#' Adjustment coefficient
# (birth_baseline - death_baseline) / (birth_baseline + death_baseline)

#' carrying capacity
k0 <- 15
# k0 <- 25
# k0 <- 80
# u0 <- seq.default(1, 12, by = 1)
# u0 %>% log1p()
# exp(-(0:12) + log(10)) %>% prettyNum()
# sqrt(c(0:10**2)) %>%
#   plot(y=rep.int(0, length(.)))
# sqrt(0:10**2)
# u0 <- seq.default(1, ceiling(1.5 * k0), by = 1)


reps <- 100
reps <- 50
max_t_years <- 25

# max_t_weeks <- max_t_years * 52

curve(exp(-x), to = 10)
kern <- function(distance, d0 = 1) {
  exp(-distance / d0)
}

# curve(exp(-x/5), to = 30)
curve(kern(x), to = 10)

# stopifnot(length(u0) == 1)
# TODO: maybe sort it
u1 <- 10
u2 <- 0
current_u <- c(u1 = u1, u2 = u2) %>% rep.int(times = reps)
# current_u
# reps
id_rep <- seq_len(reps)
current_t <- numeric(length(id_rep))


if (any(k0 > current_u)) {
  warning("Starting population larger than carrying capacity")
}

# results <- cbind(rep = integer(0), t = numeric(0)) %>%
#   cbind(u = identity(t(numeric(2))))
results <- cbind(rep = integer(0), t = numeric(0),
                 u1 = integer(0),
                 u2 = integer(0))
results <- rbind(
  results,
  cbind(rep = id_rep, t = current_t,
        matrix(current_u, ncol = 2, byrow = TRUE)
  )
)
stopifnot(nrow(results) == reps)

repeat {

  stopifnot(all(!is.na(current_u),
                !is.na(current_t)))

  current_n <- length(id_rep)
  next_unif <- runif(current_n)

  stopifnot(
    vapply(next_unif, \(x) !isTRUE(all.equal.numeric(x, 0)), FUN.VALUE = logical(1))
  )
  # birth <- growth_baseline * pmax(0, 1 - current_u / k0)
  # death <- growth_baseline * pmax(0, current_u / k0 - 1)

  # BIRTH and DEATH process
  # birth <- beta_0 * pmax(0, (1 - (current_u * (beta_0 - mu_0)) / (k0 * (beta_0 + mu_0))))
  # death <- mu_0 * (1 + (current_u * (beta_0 - mu_0)) / (k0 * (beta_0 + mu_0)))
  # stopifnot(all(birth >= 0), all(death >= 0))
  # delta_t <- -log(next_unif) / ((birth + death) * current_u)

  # MIGRATION
  current_u1 <- current_u[c(TRUE, FALSE)]
  current_u2 <- current_u[c(FALSE, TRUE)]
  # all_m <- kern(distance = dist_01) * current_0
  m_common <- m_baseline * kern(distance = dist_01) * (current_u1 + current_u2)
  #FIXME: check if any (all?) m_common is zero
  delta_t <- -log(next_unif) / (m_common)
  stopifnot(
    length(delta_t) == length(m_common)
  )
  # what if no-one migrates?
  # delta_t[current_u == k0] <- 0

  # DEBUG
  # (death == 0) & (birth == 0) & (current_u!=k0)

  # ALTERNATIVE:
  # delta_t <- rexp(n = current_n, rate = (birth + death) * current_u)
  stopifnot(all(!is.infinite(delta_t)))
  stopifnot(all(delta_t >= 0))
  next_t <- current_t + delta_t

  # IDEA: use rexp instead you crazy person

  # BIRTH AND DEATH APPROACH
  # birth <- rate_to_probability(birth)
  # death <- rate_to_probability(death)
  # stopifnot(all(birth >= 0), all(death >= 0),
  #           all(birth <= 1), all(death <= 1))

  # which direction did the movement occur in?
  delta_u <- rbinom(n = current_n, size = 1,
                    prob = current_u2 / (current_u1 + current_u2))
  # 0 => u1, 1 => u2
  delta_u <- c(-1, 1)[delta_u + 1]
  # current_u
  # delta_u
  next_u <- current_u
  next_u[c(TRUE, FALSE)] <- current_u1 + delta_u
  next_u[c(FALSE, TRUE)] <- current_u2 + (-1) * delta_u
  # current_u
  stopifnot(all(next_u >= 0))
  stopifnot(all(!is.na(delta_u)))

  #shift
  current_t <- next_t
  current_u <- next_u

  # record
  results <- rbind(
    results,
    cbind(rep = id_rep, t = current_t,
          u = current_u %>% matrix(byrow = TRUE, ncol = 2))
  )

  # retire elapsed trajectories
  elapsed_t <- which(current_t >= max_t_years)
  elapsed_t_u <- c(
    2 * elapsed_t - 1,
    2 * elapsed_t
  )
  # elapsed_t <- c(elapsed_t, elapsed_t + 1)
  # FIXME: it has to be all u in the iteration
  # elapsed_u <- which(current_u == 0)
  elapsed_u <- integer()
  # elapsed_k0 <- which(current_u == k0) # or delta_t == 0
  elapsed_k0 <- integer()
  elapsed_reps <- unique(c(elapsed_t, elapsed_u, elapsed_k0))

  if (length(elapsed_reps) > 0) {
    #   # message(glue("{length(elapsed_u)}"))
    #
    #   #add `t_max` to the `elapsed_u` group
    #   # FIXME: this is currently disabled
    #   elapsed_u_and_k0 <- unique(c(elapsed_u, elapsed_k0))
    #   elapsed_t_u
    #   # current_u
    #   results <- rbind(
    #     results, cbind(rep = id_rep[elapsed_u_and_k0],
    #                    t = rep.int(max_t_years, length(elapsed_u_and_k0)),
    #                    u = current_u[elapsed_u_and_k0] %>%
    #                      matrix(byrow=TRUE, ncol = 2))
    #   )
    # }

    # remove the elapsed simulations from consideration
    # current_u <- current_u[-elapsed_reps]
    # current_t <- current_t[-elapsed_reps]
    current_u <- current_u[-elapsed_t_u]
    current_t <- current_t[-elapsed_t]
    # FIXME:
    # id_rep <- id_rep[-elapsed_reps]
    id_rep <- id_rep[-elapsed_t]
  }

  stopifnot(length(current_u) == 2 * length(current_t),
            length(current_t) == length(id_rep)
  )
  if (length(current_u) == 0) {
    break
  }
}
stopifnot(length(id_rep) == 0)

results <-
  results %>%
  as_tibble()

# current_t
# current_u
# id_rep
# stop()
# results <- results |>
#   as_tibble() %>%
#   left_join(
#     results_info %>% bind_rows(),
#     by = join_by(rep)
#   )
# u0
#'
#' Cache the result
fs::dir_create(".cache")
results %>% write_rds(".cache/024_results_with_info.RDS")

results

results %>%
  pivot_longer(c(u1, u2),
               names_pattern = "u(\\d+)",
               names_to = "id_u",
               values_to = "u") %>%
  identity() %>% {
    ggplot(.) +
      aes(t, u, group = str_c(rep, id_u)) +
      geom_line(
        aes(color = id_u),
        alpha = 0.4,
      ) +

      # APPROACH: Poisson-regression
      # geom_smooth(aes(group = id_u,
      #                 color = id_u),
      #             method = glm,
      #             method.args = list(family = poisson),
      #             linetype = "dashed",
      #             se = FALSE) +
      # APPROACH: GAM
      geom_smooth(aes(group = id_u,
                      color = id_u),
                  # method = glm,
                  # method.args = list(family = poisson),
                  linetype = "dashed",
                  se = FALSE) +

      geom_vline(aes(xintercept = max_t_years,
                 color = "max_t"),
                 linetype = "dotted") +

      theme_bw(base_size = 14) +
      labs(color = NULL) +
      theme(legend.position = "bottom") +
      NULL
  }



#'
#' # glm_results <- glm(
#' #   u ~ t + offset(-log(results$u0)),
#' #   # u ~ factor(id_u0)*0 + t,
#' #   data = results,
#' #   family = poisson()
#' # )
#' # glm_results
#' # glm_results %>% summary()
#' # glm_results <- glm(
#' #   u ~ t + 0,
#' #   offset = log(u0),
#' #   data = results,
#' #   family = poisson()
#' # )
#' # glm_results
#' # glm_results %>% summary()
#' # glm_results %>%
#' #   predict.glm(
#' #     newdata = tibble(t = 0.1),
#' #     terms = TRUE
#' #   ) %>%
#' #   # str()
#' #   as_tibble()
#'
#' # predict.glm(glm_results) %>%
#' #   as_tibble()
#' # predict.glm(glm_results,
#' #             newdata = tibble(t = 0.1, u0 = 1)) %>%
#' #   as_tibble()
#' #
#' # glm_predict <-
#' #   expand_grid(
#' #     # t = seq.default(0, max_t_years, length.out = 100),
#' #     # u0 = seq.default(0, 10, by = 1)
#' #   ) %>%
#' #   bind_cols(
#' #     predict =
#' #       predict.glm(
#' #         glm_results,
#' #         newdata = .,
#' #         type = "response"
#' #       )
#' #   )
#' # results %>%
#' #   bind_cols(
#' #     predict = predict.glm(
#' #       glm_results, type = "response"
#' #     )
#' #   ) %>%
#' #   # nrow() %>%
#' # # glm_predict %>%
#' #   ggplot() +
#' #   aes(t, predict, group = id_u0) +
#' #
#' #   geom_line(aes(color = u0)) +
#' #   scale_color_binned() +
#' #
#' #   theme_bw(base_size = 14) +
#' #   NULL
#' #'
#' #'
#' p_traj <- results |>
#'   ggplot() +
#'   aes(t, u, group = rep) +
#'   # geom_step() +
#'   # geom_line(color = "grey90", show.legend = FALSE) +
#'   geom_line(aes(alpha = u), show.legend = FALSE) +
#'   # geom_step(aes(alpha = u), show.legend = FALSE) +
#'   geom_hline(aes(yintercept = k0), linetype = "dotted") +
#'
#'   scale_alpha_continuous(range = c(0.15)) +
#'   # xlim(c(NA, 4)) +
#'   theme_bw()
#' #'
#' p_traj
#' #'
#' #'
#' results |>
#'   ggplot() +
#'   aes(t, u, group = rep) +
#'   # geom_step(aes(color = u0)) +
#'   geom_line(aes(color = u0), alpha = 0.25) +
#'   geom_hline(aes(yintercept = k0), linetype = "dotted") +
#'
#'   scale_alpha_continuous(range = c(0.15)) +
#'   scale_color_binned() +
#'   xlim(c(NA, 11)) +
#'   ylim(c(0, NA)) +
#'
#'   theme_bw() +
#'   theme(legend.position = "bottom") +
#'   NULL
#' #'
#' results %>%
#'   # glimpse()
#'   ggplot() +
#'   aes(t, u, group = rep) +
#'   # geom_step() +
#'   # geom_line(color = "grey90", show.legend = FALSE) +
#'   geom_line(aes(alpha = u0), show.legend = FALSE) +
#'   # geom_step(aes(alpha = u), show.legend = FALSE) +
#'   geom_hline(aes(yintercept = k0), linetype = "dotted") +
#'
#'   scale_alpha_continuous(range = c(0.15)) +
#'
#'   theme_bw()
#'
#' # p_traj +
#' #   facet_grid(~u0) +
#' #   lims(u0 = c(0,10))
#' #'
#' # ode_times <- results$t %>% zapsmall() %>%  unique() %>% sort()
#' #'
#' #'
#'
#' ode_results <- deSolve::ode(
#'   y = c(u = u0), times = seq.default(0, 25, by = 0.001),
#'   parms = list(r = birth_baseline - death_baseline, k0 = k0,
#'                birth_baseline = birth_baseline,
#'                death_baseline = death_baseline),
#'   func = function(times, y, parameters) {
#'     with(parameters, {
#'       dN <- r * y * (1 - y / k0)
#'       list(dN)
#'     })
#'   }
#' )
#' ode_results <-
#'   ode_results %>%
#'   unclass() %>%
#'   as_tibble() %>%
#'   rename(t = time) %>%
#'   pivot_longer(
#'     -t,
#'     names_pattern = "u(\\d+)",
#'     names_to = c("id_u0"),
#'     values_to = "u") %>%
#'   mutate(u0 = u0[as.integer(id_u0)],
#'          id_u0 = NULL)
#' #TODO
#'
#'
#' p_traj_and_ode <- p_traj +
#'   aes(color = id_u0) +
#'   geom_line(aes(color = u0, group = u0), data = ode_results) +
#'
#'   labs(color = NULL) +
#'   theme(legend.position = "bottom") +
#'   NULL
#' p_traj_and_ode
#'
#'
#' results |>
#'   ggplot() +
#'   aes(t, u, group = rep) +
#'   # geom_step(aes(color = u0)) +
#'   geom_line(aes(color = u0), alpha = 0.15) +
#'   geom_hline(aes(yintercept = k0), linetype = "dotted") +
#'
#'
#'   # ODE
#'   geom_line(aes(color = u0, group = u0),
#'             linewidth = 0.75, linetype = "dashed",
#'             data = ode_results) +
#'
#'   labs(color = NULL, y = "N", x = "time") +
#'   scale_alpha_continuous(range = c(0.15)) +
#'   scale_color_binned() +
#'   xlim(c(NA, 11)) +
#'   ylim(c(0, NA)) +
#'   theme_bw(base_size = 14) +
#'   theme(aspect.ratio = 1) +
#'   theme(legend.position = "bottom") +
#'   NULL
#'
#' fs::dir_create("figures")
#' # ggsave(
#' #   width = 10*2,
#' #   filename = "figures/024_balanced_birth_death_with_ode.svg",
#' #   scale = 2,
#' #   device = svglite::svglite
#' # )
#'
#' #'
#' # fs::dir_create("figures")
#' # pdf(file = "figures/017_gillespie_growth_with_ode.pdf")
#' # results |>
#' #   group_by(u0) %>%
#' #   group_map(\(data, key) {
#' #     ggplot(data) +
#' #       aes(t, u, group = rep) +
#' #       # geom_step() +
#' #       # geom_line(color = "grey90", show.legend = FALSE) +
#' #       geom_line(aes(alpha = u), show.legend = FALSE) +
#' #       # geom_step(aes(alpha = u), show.legend = FALSE) +
#' #       geom_hline(aes(yintercept = k0), linetype = "dotted") +
#' #
#' #       scale_alpha_continuous(range = c(0.15)) +
#' #
#' #       theme_bw() +
#' #       geom_line(aes(color = u0, group = u0), data = ode_results) +
#' #
#' #       labs(color = NULL) +
#' #       theme(legend.position = "bottom") +
#' #       NULL
#' #   })
#' # dev.off()
#'
#' #'
#' # VALIDATION: must be zero / empty
#' # results %>%
#' #   dplyr::filter(t < 0)
#' #'
#' results %>%
#'   # dplyr::filter(id_u0 == 5) %>%
#'   ggplot() +
#'   aes(t, u, group = rep) +
#'   # geom_line(aes(color = identity(u0)), show.legend = TRUE) +
#'
#'   # `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
#'   geom_smooth(aes(group = u0, color = u0)) +
#'   geom_line(aes(color = u0, group = u0), data = ode_results) +
#'
#'   # scale_color_viridis_c(direction = -1) +
#'   scale_color_viridis_b(direction = -1) +
#'
#'   labs(color = NULL) +
#'   theme_bw(base_size = 14) +
#'   theme(legend.position = "bottom") +
#'   NULL
#' #'
#' #' Plotting P(extinction, t | u0)
#' #'
#' results %>%
#'   slice_max(t, by = rep) %>%
#'   summarise(
#'     `P(extinction)` = mean(u < 1),
#'     .by = c(u0)
#'   ) %>%
#'   identity() %>%
#'   glimpse() %>%
#'   ggplot() +
#'   aes(u0, `P(extinction)`) +
#'   geom_line() +
#'   # geom_line(aes(color = u0)) +
#'   # scale_colour_viridis_c(direction = -1) +
#'   # scale_color_viridis_b(direction = -1, n.breaks = 9) +
#'
#'   labs(y = "P(extinct)",
#'        x = "u0",
#'        color = NULL) +
#'   theme_bw(base_size = 14) +
#'   ylim(y = c(0, NA)) +
#'   NULL
#' #'
#' #'
#' #'
#' #TODO: I need to do this for each of these lines, and calculate the difference
#' #' between the two, although they do look very similar...
#' #'
#' results_approxfun <-
#'   tapply(
#'     X = results[, c("t", "u")],
#'     INDEX = results$rep,
#'     FUN = \(x) {
#'       # rule = 1:2 for extinct numbers make sense.
#'       approxfun(x$t, y = x$u, method = "constant")
#'     }
#'   )
#' #'
#' max_t_years
#' #'
#' # DEBUG
#' # curve(
#' #   results_approxfun[[40]](x), to = 100
#' # )
#' results_info %>% str()
#' traj_approx_df <-
#'   results_approxfun %>%
#'   enframe("id_rep", "traj_approxfun") %>%
#'   bind_cols(results_info)
#' stopifnot(!anyNA(traj_approx_df))
#' stopifnot(all(traj_approx_df$id_rep == traj_approx_df$rep))
#' traj_approx_df
#' # traj_approx_df %>% View()
#'
#' t_linspace <- seq.default(0, max_t_years, length.out = 100)
#' id_time <- seq_along(t_linspace)
#' id_time_linspace <- rep.int(id_time, length(results_info$u0$u0))
#'
#' outcome_linspace <-
#'   vapply(results_approxfun,
#'          \(traj_fun) traj_fun(t_linspace),
#'          FUN.VALUE = double(length(t_linspace)))
#' # outcome_linspace %>%
#' #   Matrix::Matrix() %>% `[`(1:10, 1:10)
#' outcome_linspace %>% dim()
#' # id_time_linspace %>% dim()
#' id_time_linspace %>% length()
#' u0_linspace <- rep(results_info$u0$u0, each = length(t_linspace))
#' u0_linspace %>% length()
#'
#' prob_ext <- tapply(
#'   X = outcome_linspace,
#'   INDEX = list(u0 = u0_linspace, time = id_time_linspace),
#'   FUN = \(x) mean(x < 1)
#' )
#' prob_ext %>%
#'   str()
#' prob_ext %>%
#'   as_tibble()
#' prob_ext %>%
#'   as.data.frame.table() %>%
#'   as_tibble() %>%
#'   mutate(u0 = as.numeric(u0),
#'          time = as.numeric(time),
#'          time = t_linspace[time]) %>%
#'   identity() %>%
#'   ggplot() +
#'   aes(time, Freq, group = u0) +
#'   geom_line(aes(color = u0)) +
#'   # scale_colour_viridis_c(direction = -1) +
#'   scale_color_viridis_b(direction = -1, n.breaks = 9) +
#'
#'   labs(y = "P(extinct)",
#'        x = "time",
#'        color = NULL) +
#'   theme_bw(base_size = 14) +
#'   NULL
#' #'
#' #'
#' #'
#' #'
#'
