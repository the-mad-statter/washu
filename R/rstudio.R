#' Valid project name
#' Check to see if project name follow YYYY-MM-DD-N format
#' @param project_name proposed project name to check
valid_project_name <- function(project_name) {
  matches_pattern <- grepl("^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])\\-\\d$", project_name)
  date_portion <- substr(project_name, 1, 10)
  parsable_date <-   tryCatch(expr = { tmp <- as.Date(date_portion); TRUE },
                              error = function(e) { FALSE })
  matches_pattern & parsable_date
}

#' Consult Project
#' @param path path to new project
#' @param principal_investigator name of the principal investigator
#' @param ... parameters passed from the new project dialog
#' @export
wu_consult_project <- function(path, principal_investigator = "Dr. Winnie Pooh", ...) {
  project_name <- basename(path)
  if(!valid_project_name(project_name))
    stop("Project name must be in YYYY-MM-DD-N format.")

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  from <- system.file(file.path("rmarkdown", "templates", "consult_report", "skeleton"), package = "washu")
  files <- list.files(from, full.names = TRUE)
  file.copy(files, path, recursive = TRUE)
  rmd_name <- file.path(path, paste0(project_name, "-v1.Rmd"))
  file.rename(file.path(path, "skeleton.Rmd"), rmd_name)

  #dots <- list(...)

  rmd_file <- file(rmd_name)
  rmd_body <- sub("Dr. Winnie Pooh", principal_investigator, readLines(rmd_file), fixed = TRUE)
  rmd_body <- sub('"`r Sys.Date()`"', substr(project_name, 1, 10), rmd_body, fixed = TRUE)
  writeLines(rmd_body, rmd_file)
  close(rmd_file)
}
