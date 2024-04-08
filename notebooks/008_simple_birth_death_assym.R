#' We'll explore the notion of asymetric birth-death feedback in a simulation
#' here.
#'
#' To start with, we'll have a function that procedurally simulates a birth
#' and death process.
#'

simBD <- function(n, delta_t, prob_birth, prob_death) {
  birth_d <- delta_t * prob_birth
  death_d <- delta_t * prob_death

  results <- cbind(
    time = numeric(), count = integer(0)
  )

  current_time <- 0
  for (step_n in seq_len(n)) {
    current_time <- current_time + delta_t

  }


}
