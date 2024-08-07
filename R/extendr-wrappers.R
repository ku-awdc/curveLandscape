# Generated by extendr: Do not edit by hand

# nolint start

#
# This file was created with the following call:
#   .Call("wrap__make_curveLandscape_wrappers", use_symbols = TRUE, package_name = "curveLandscape")

#' @usage NULL
#' @useDynLib curveLandscape, .registration = TRUE
NULL

#' Simulates a birth and death process that is density-dependent
#'
#'
sim_bd_only <- function(n0, birth_baseline, death_baseline, carrying_capacity, t_max) .Call(wrap__sim_bd_only, n0, birth_baseline, death_baseline, carrying_capacity, t_max)

sim_bd_only_many <- function() invisible(.Call(wrap__sim_bd_only_many))

#' Simulates birth, death and migration process of a multi-patch system.
#'
#' @param migration_baseline Double that is m_0 in the formulas and is normalised
#' by (n-1) internally.
#'
#' @details
#'
#' The migration-baseline must be normalised
#'
#'
#'
#' It is assumed that birth-rate exceeds death-rate, i.e. beta >= mu
#'
#'
sim_bdm <- function(n0, birth_baseline, death_baseline, carrying_capacity, migration_baseline, t_max) .Call(wrap__sim_bdm, n0, birth_baseline, death_baseline, carrying_capacity, migration_baseline, t_max)

#' Returns the row and column from a 0-indexed, column-wise linear index `k` for a square matrix of dimension `n` x `n`
get_row_col <- function(k, n) .Call(wrap__get_row_col, k, n)

#' Returns the linear, 0-index id for (i,j) for n x n matrix.
get_linear_id <- function(i, j, n) .Call(wrap__get_linear_id, i, j, n)

#' Return the total number of elements in lower-triangular matrix (without diagonal)
get_total_number_of_elements <- function(n) .Call(wrap__get_total_number_of_elements, n)


# nolint end
