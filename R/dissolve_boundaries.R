#' @author Barry
dissolve_boundaries <- function(x) {
  if (!requireNamespace("spdep")) {
    stop("spdep is needed to use this function")
  }

  crs <- sf::st_crs(x)
  nbs <- spdep::poly2nb(x)
  subgraphs <- spdep::n.comp.nb(nbs)
  res <- lapply(
    split(x, subgraphs[["comp.id"]]),
    sf::st_union,
    is_coverage = TRUE
  )

  sf::st_sfc(
    unlist(res, recursive = FALSE),
    crs = crs
  )
}
