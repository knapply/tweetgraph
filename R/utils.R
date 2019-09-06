.map_chr <- function(.x, .f, ..., .n = 1L) {
  vapply(.x, .f, FUN.VALUE = character(.n), ...)
}

.map_lgl <- function(.x, .f, ..., .n = 1L) {
  vapply(.x, .f, FUN.VALUE = logical(.n), ...)
}

.keep <- function(.x, .f, ...) {
  .x[vapply(.x, .f, FUN.VALUE = logical(1L), ...)]
}

.discard <- function(.x, .f, ...) {
  .x[!vapply(.x, .f, FUN.VALUE = logical(1L), ...)]
}
