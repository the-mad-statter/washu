#' @inherit REDCapR::retrieve_credential
#' @export
retrieve_credential_local <- function(project_id,
                                      path_credential = "~/.REDCapR",
                                      ...) {
  REDCapR::retrieve_credential_local(
    path_credential = path_credential,
    project_id = project_id,
    ...
  )
}

#' @inherit REDCapR::redcap_read
#' @export
redcap_read <- function(project_id, path_credential = '~/.REDCapR', ...) {
  credentials <- REDCapR::retrieve_credential_local(path_credential, project_id)
  REDCapR::redcap_read(
    redcap_uri = credentials$redcap_uri,
    token = credentials$token,
    ...
  )$data
}

#' Delete REDCap records
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap project. Required.
#' @param token The user-specific string that serves as the password for a project. Required.
#' @param records Vector of record ids to delete. Required.
#'
#' @return A \link[httr]{response} object
#' @export
#'
#' @examples
#' \dontrun{
#' creds <- retrieve_credential_local(1234)
#' redcap_delete_records(creds$redcap_uri, creds$token, c(1, 5, 7))
#' }
redcap_delete_records <- function(redcap_uri, token, records) {
  param_set_1 <- list(
    token = token,
    action = "delete",
    content = "record"
  )

  param_set_2 <- as.list(as.character(records))
  names(param_set_2) <- sprintf("records[%s]", 0:(length(records) - 1))

  httr::POST(redcap_uri, body = c(param_set_1, param_set_2))
}
