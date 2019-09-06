.map_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, FUN.VALUE = logical(1L), ...)
}

.keep <- function(.x, .f, ...) {
  .x[.map_lgl(.x, .f, ...)]
}

.discard <- function(.x, .f, ...) {
  .x[!.map_lgl(.x, .f, ...)]
}