#' @inheritParams ggplot2::scale_color_manual
scale_habitat_fill_manual <- function(...) {
  scale_fill_manual(
    values = c(
      "High" = "forestgreen",
      "Low" = "wheat",
      "Non" = "grey"
    ), ...
  )
}

#' @inheritParams ggplot2::scale_color_manual
scale_habitat_color_manual <- function(...) {
  scale_color_manual(
    values = c(
      "High" = "forestgreen",
      "Low" = "wheat",
      "Non" = "grey"
    ), ...
  )
}
