## Emigration and Immigration
### Matt Denwood, 2024-05-01

library("gtools")
library("tidyverse")
theme_set(theme_light())

## Number of patches and total population size:
n_patches <- 3L
total_pop <- 1.0

## Carrying capacity (sums to unity for ease):
CC <- rdirichlet(1L, rep(1.0, n_patches)) |> as.numeric() |> magrittr::multiply_by(total_pop)

## Distances (symmetric in this case):
matrix(runif(n_patches^2), nrow=n_patches) |>
  {function(x) x*upper.tri(x)}() |>
  {function(x) x + t(x)}() ->
  distance

## Some (vectorised) function of distance, current N, and CC to calculate rate:
mig_rate <- function(dd, n, cc){
  stopifnot(length(n) %in% c(1L, length(dd)))
  stopifnot(length(cc) %in% c(1L, length(dd)))

  ## The movement propensity coefficient:
  move_coef <- 0.2

  ## Replace with any relationship you choose:
  # move_coef * dd * n / cc
  move_coef * dd * n * 2 * exp(-log(1 + exp(cc - n)))
  
}

## Starting N:
N <- rdirichlet(1L, c(10L, rep(1.0, n_patches-1L))) |> as.numeric() |> magrittr::multiply_by(total_pop)
stopifnot(sum(N)==total_pop)

## Pairwise emigration matrix, representing movement of animals from rows to columns:
emigration_matrix <- matrix(0.0, n_patches, n_patches)
for(source in seq_len(n_patches)){
  emigration_matrix[source, ] <- mig_rate(distance[source,], N[source], CC[source])
}

## Pairwise immigration matrix, representing movement of animals to rows from columns:
immigration_matrix <- matrix(0.0, n_patches, n_patches)
for(dest in seq_len(n_patches)){
  immigration_matrix[dest, ] <- mig_rate(distance[ , dest], N, CC)
}

## But of course:
stopifnot(emigration_matrix == t(immigration_matrix))
## We therefore only need one of emigration OR immigration...!
## The advantage of emigration is that it uses only quantities known to the source patch


## Total rate of gain (positive) or loss (negative) of animals for each patch:
rowSums(immigration_matrix) - rowSums(emigration_matrix)
### Or:
rowSums(t(emigration_matrix)) - rowSums(emigration_matrix)
### Or:
colSums(emigration_matrix) - rowSums(emigration_matrix)
### Where the sum is zero i.e. animals are not lost from (or gained into) the system:
stopifnot(all.equal(0, sum(colSums(emigration_matrix) - rowSums(emigration_matrix))))

## Where we have a small number of patches we can write out the equations manually eg:
movement_1 <-
  ### Immigration 2->1:
  0.2 * distance[2,1] * N[2]/CC[2] +
  ### Immigration 3->1:
  0.2 * distance[3,1] * N[3]/CC[3] +
  ### Emigration 1->2:
  -0.2 * distance[1,2] * N[1]/CC[1] +
  ### Emigration 1->3:
  -0.2 * distance[1,3] * N[1]/CC[1]
## Or for the more general migration rate function:
movement_1 <-
  ### Immigration 2->1:
  mig_rate(distance[2,1], N[2], CC[2]) +
  ### Immigration 3->1:
  mig_rate(distance[3,1], N[3], CC[3]) +
  ### Emigration 1->2:
  -mig_rate(distance[1,2], N[1], CC[1]) +
  ### Emigration 1->3:
  -mig_rate(distance[1,3], N[1], CC[1])
movement_1
(colSums(emigration_matrix) - rowSums(emigration_matrix))[1]
## But in general I would stick to calculation of the emigration_matrix and working from that
