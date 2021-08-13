#' Valid project name
#' Check to see if project name follow YYYY-MM-DD-N format
#' @param project_name proposed project name to check
#' @param version flag to check for version number
valid_project_name <- function(project_name, version = FALSE) {
  if(version)
    matches_pattern <- grepl("^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])\\-\\d+\\-v\\d+$", project_name)
  else
    matches_pattern <- grepl("^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])\\-\\d+$", project_name)
  date_portion <- substr(project_name, 1, 10)
  parsable_date <-   tryCatch(expr = { tmp <- as.Date(date_portion); TRUE },
                              error = function(e) { FALSE })
  matches_pattern & parsable_date
}

#' Consult Project
#' @param path path to new project
#' @param ... parameters passed from the new project dialog
#' @export
wu_consult_project <- function(path, ...) {
  project_name <- basename(path)

  # test project name
  if(!valid_project_name(project_name))
    stop("Project name must be in YYYY-MM-DD-N format.")

  # test project exists
  project_exists <- c(
    file = file.exists(path),
    data = cdb_consult_exists(project_name)
  )
  if(all(unname(project_exists)))
    stop(sprintf("Consult %s already exists in both the filebase and database.", project_name))
  if(project_exists[["file"]])
    stop(sprintf("Consult %s already exists in the filebase.", project_name))
  if(project_exists[["data"]])
    stop(sprintf("Consult %s already exists in the database.", project_name))

  # edit database
  print(cdb_edit_app(project_name, new = TRUE))

  # make sure consult was written to database
  if(!cdb_consult_exists(project_name))
    stop(sprintf("Consult %s not in the database.", project_name))

  # retrieve pi name
  tbl_pi <- cdb_get_principal_investigator(project_name)
  principal_investigator <- paste(tbl_pi$personalTitle, tbl_pi$givenName, tbl_pi$sn)

  # copy template files and then delete the .tmp placeholders
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  from <- system.file(file.path("rmarkdown", "templates", "consult_report", "skeleton"), package = "washu")
  files <- list.files(from, full.names = TRUE)
  file.copy(files, path, recursive = TRUE)
  file.remove(list.files(path, "^.tmp$", all.files = TRUE, full.names = TRUE, recursive = TRUE))

  # edit the rmd
  rmd_name <- file.path(path, paste0(project_name, "-v1.Rmd"))
  file.rename(file.path(path, "skeleton.Rmd"), rmd_name)
  rmd_file <- file(rmd_name)
  rmd_body <- sub("Dr. Winnie Pooh", principal_investigator, readLines(rmd_file), fixed = TRUE)
  rmd_body <- sub('"`r Sys.Date()`"', substr(project_name, 1, 10), rmd_body, fixed = TRUE)
  writeLines(rmd_body, rmd_file)
  close(rmd_file)

  # open stuff
  dir.open(path)
  file.edit(rmd_name)
}
