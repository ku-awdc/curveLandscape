.DollarNames.WildSSA = function(env, pattern = "") {
  ls(WildSSA, pattern = pattern)
}

WildSSA.format <- function(x, ...) {
  cat(x$internal_debug_display())
}

WildSSA.print <- function(x, ...) {
  format(x)
}