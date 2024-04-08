# source(".Rprofile")

# birth <- 1 / 25
# death <- 1 / (52 * 7)


yearly_prob_to_weekly_prob <- function(p_annual) {
  1 - (1 - p_annual)**(1 / 52)
}

rate_to_probability <- function(rate) {
  1 - exp(-rate)
}

# 
birth <-  4.81 / 100
death <- 0.67 / 100
rate_conversion <- function(rate) {
  yearly_prob_to_weekly_prob(rate_to_probability(rate))
}

birth <- rate_conversion(birth) 
death <- rate_conversion(death)
growth_rate <- birth - death
growth_rate

if (growth_rate <= 1) {
  message("growth rate will lead to extinction")
}

growth_rate <- yearly_prob_to_weekly_prob(rate_to_probability(growth_rate))

u0 <- 100

max_t_years <- 100
max_t_weeks <- max_t_years * 52

reps <- 100
stopifnot(length(u0) == 1)
current_u <- rep.int(u0, reps)
current_t <- rep.int(0, reps)
results <- cbind(rep = integer(0), t = numeric(0), u = numeric(0))
id_rep <- seq_len(reps)

repeat {
  current_n <- length(current_u)
  next_unif <- runif(current_n)
  
  stopifnot(
    vapply(next_unif, \(x) !isTRUE(all.equal.numeric(x, 0)), FUN.VALUE = logical(1))
  )

  delta_t <- -log(next_unif) / ((birth + death) * current_u)
  next_t <- current_t + delta_t

  delta_u <- rbinom(n = current_n, size = 1, prob = death / (birth + death))
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
  elapsed_t <- which(current_t >= max_t_weeks)
  elapsed_u <- which(current_u == 0)
  elapsed_reps <- unique(c(elapsed_t, elapsed_u))
    
  if (length(elapsed_reps) > 0) {
    # message(glue("{length(elapsed_u)}"))

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
  as_tibble()

results |> 
  ggplot() +
  aes(t, u, group = rep) + 
  # geom_step() + 
  geom_line(aes(alpha = u), show.legend = FALSE) + 
  
  scale_alpha_continuous(range = c(0.15)) +

  theme_bw()
#'
#' 
#' 
#' 