devtools::load_all()

st_make_grid
#' @param hex_size radius of outer circle (length to vertices)
#' @param flat_top `logical(1)` orientation of hexagons
#'
create_hexagon_grid <- function(x, c_x, c_y, side_length, flat_top = FALSE) {
  hex_size <- NA
  hex_flat_top <- FALSE
  # cx <- 0
  # cy <- 0
  #' inner circle 
  hex_width <- 2 * hex_size
  hex_height <- sqrt(3) * hex_size

  hex_coords_arr <-
    array(
      c(-5:5) %>% rep(each = 7 * 2),
      dim = c(7, 2, 11)
    )
  hex_coords_arr

  hex_each <- asplit(hex_coords_arr, MARGIN = 3)
  hex_each

  if (hex_flat_top) {
    spacing_horiz <- 3 / 2 * hex_size
    spacing_vert <- sqrt(3) * hex_size

    angles_deg <- (1:6) * 60
    angles_deg <- c(angles, angles[1])
    angles_rad <- (pi / 180) * angles_deg
  } else {
    # !flat_top
    # pointy_top is true
    spacing_horiz <- sqrt(3) * hex_size
    spacing_vert <- 3 / 2 * hex_size

    angles_deg <- c(1:6) * 60 - 30
    angles_deg <- c(angles, angles[1])
    angles_rad <- (pi / 180) * angles_deg
  }
}
#' We'd like to tessellate a square-polygon with hexagons. First, we choose
#' to retain all hexagons with centroid within the square-polygon. That, however,
#' doesn't suffice, as then there are parts of the square uncovered by a hexagon.
#' These areas stem from the staggered rows of hexagons, in which there is
#' a jagged part that is part of the square, but not part of hexagon.
#'
#'
