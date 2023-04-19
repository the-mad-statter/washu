is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
