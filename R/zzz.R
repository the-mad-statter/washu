is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

.onAttach <- function(...) {
  if (!is_attached("dplyr")) {
    library(dplyr)
  }
}
