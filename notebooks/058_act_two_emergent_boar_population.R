devtools::load_all()


#' Act 2: Investigate the properties of an emergent wild boar population in the landscape.
#'
#' For each landscape, find the number of animals, that need to be seeded, in order to attain a non-extinction rate of the population
#' of say 10%, within 1,2,3 years
#'
#'
all_tmax <- c(1, 2, 3)

total_n0 <- 1

seed_locations_in_grid <- function(grid, total_n0) {
  stopifnot(
    !is.null(grid$Capacity),
    total_n0 >= 1
  )
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

for (grid in all_landscapes$grid[-c(1,6)]) {
  for (total_n0 in 1:45) {
    n0 <-
      seed_locations_in_grid(grid = grid, total_n0 = total_n0)
    n0 %>%
      table() %>%
      print()
    n0 %>%
      print() %>%
      sum() %>%
      print()

    stopifnot(
      all(n0 <= grid$Capacity)
    )
  }
}
