#' Store a token and other credentials in a file
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#' token is unique to each user for each project). See the section on the
#' left-hand menu for obtaining a token for a given project.
#' @param path_credential The file path to the CSV containing the credentials.
#' Required.
#' @param username REDCap username associated with the token
#'
#' @export
store_credential_local <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           path_credential = "~/.REDCapR",
           username = Sys.getenv("WU_REDCAP_USER")) {
    redcap_export_project_information(
      redcap_uri = redcap_uri,
      token = token,
      format = "csv"
    ) %>%
      httr::content() %>%
      dplyr::transmute(
        redcap_uri = redcap_uri,
        username = username,
        project_id = as.character(project_id),
        token = token,
        comment = project_title
      ) %>%
      readr::write_csv(path_credential, na = "", append = TRUE, quote = "all")
  }

#' Parse Content Type
#'
#' @param content_type content-type header from REDCap to be parsed
#' @return list containing type, name, and charset
parse_content_type <- function(content_type) {
  list(
    "type" = stringr::str_extract(content_type, "^.*(?=; )"),
    "name" = stringr::str_extract(content_type, "(?<=name=\").+(?=\";)"),
    "charset" = stringr::str_extract(content_type, "(?<=charset=).*$")
  )
}

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
redcap_read <- function(project_id, path_credential = "~/.REDCapR", ...) {
  credentials <- REDCapR::retrieve_credential_local(path_credential, project_id)
  REDCapR::redcap_read(
    redcap_uri = credentials$redcap_uri,
    token = credentials$token,
    ...
  )$data
}

#' REDCap Array
#'
#' @param name name of the array
#' @param values values to be included in the array
#'
#' @return an object of class "redcap_array"
#' @export
#'
#' @examples
#' ## records array for three records from static
#' redcap_array("records", c(16227, 16342, 16419))
#'
#' ## fields array for two fields from static
#' redcap_array("fields", c("chip1_install_date", "chip2_install_date"))
#'
#' ## events array for one event from mother
#' redcap_array("events", "baseline_arm_1")
redcap_array <- function(name, values) {
  a <- as.list(values)
  names(a) <- sprintf("%s[%i]", name, 0:(length(values) - 1))
  class(a) <- "redcap_array"
  return(a)
}

#' REDCap Logical
#'
#' @param name name to give logical
#' @param value R logical to cast as REDCap API logical
#'
#' @return an R logical represented as a named list of lowercase character
#'
#' @examples
#' \dontrun{
#' a <- TRUE
#' redcap_logical("a", a)
#'
#' b <- FALSE
#' redcap_logical("b", b)
#'
#' c <- "not gonna work"
#' redcap_logical("c", c)
#' }
redcap_logical <- function(name, value) {
  checkmate::assert(checkmate::test_logical(value))
  value <- as.list(tolower(as.character(value)))
  names(value) <- name
  return(value)
}

#' Export Entire Project as REDCap XML File (containing metadata & data)
#'
#' @description The entire project (all records, events, arms, instruments,
#' fields, and project attributes) can be downloaded as a single XML file, which
#'  is in CDISC ODM format (ODM version 1.3.1). This XML file can be used to
#'  create a clone of the project (including its data, optionally) on this
#'  REDCap server or on another REDCap server (it can be uploaded on the Create
#'  New Project page). Because it is in CDISC ODM format, it can also be used to
#'   import the project into another ODM-compatible system. NOTE: All the option
#'    paramters listed below ONLY apply to data returned if the
#'    'returnMetadataOnly' parameter is set to FALSE (default). For this API
#'    method, ALL metadata (all fields, forms, events, and arms) will always be
#'    exported. Only the data returned can be filtered using the optional
#'    parameters.
#'
#' Note about export rights: If the 'returnMetadataOnly' parameter is set to
#' FALSE, then please be aware that Data Export user rights will be applied to
#' any data returned from this API request. For example, if you have
#' 'De-Identified' or 'Remove all tagged Identifier fields' data export rights,
#' then some data fields *might* be removed and filtered out of the data set
#' returned from the API. To make sure that no data is unnecessarily filtered
#' out of your API request, you should have 'Full Data Set' export rights in the
#'  project.
#'
#' @note To use this method, you must have API Export privileges in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param filename name of xml file to write results
#' @param overwrite only overwrite existing filename if TRUE
#' @param return_metadata_only TRUE returns only metadata (all fields, forms,
#' events, and arms), whereas FALSE returns all metadata and also data (and
#' optionally filters the data according to any of the optional parameters
#' provided in the request)
#' @param records an array of record names specifying specific records you wish
#' to pull (by default, all records are pulled)
#' @param fields an array of field names specifying specific fields you wish to
#' pull (by default, all fields are pulled)
#' @param events an array of unique event names that you wish to pull records
#' for - only for longitudinal projects
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#' @param export_survey_fields specifies whether or not to export the survey
#' identifier field (e.g., 'redcap_survey_identifier') or survey timestamp
#' fields (e.g., instrument+'_timestamp') when surveys are utilized in the
#' project. If you do not pass in this flag, it will default to 'false'. If set
#' to 'true', it will return the redcap_survey_identifier field and also the
#' survey timestamp field for a particular survey when at least one field from
#' that survey is being exported. NOTE: If the survey identifier field or survey
#'  timestamp fields are imported via API data import, they will simply be
#'  ignored since they are not real fields in the project but rather are
#'  pseudo-fields.
#' @param export_data_access_groups specifies whether or not to export the
#' 'redcap_data_access_group' field when data access groups are utilized in the
#' project. If you do not pass in this flag, it will default to 'false'. NOTE:
#' This flag is only viable if the user whose token is being used to make the
#' API request is *not* in a data access group. If the user is in a group, then
#' this flag will revert to its default value.
#' @param filter_logic String of logic text (e.g., \[age\] > 30) for filtering
#' the data to be returned by this API method, in which the API will only return
#'  the records (or record-events, if a longitudinal project) where the logic
#'  evaluates as TRUE. This parameter is blank/null by default unless a value is
#'   supplied. Please note that if the filter logic contains any incorrect
#'   syntax, the API will respond with an error message.
#' @param export_files TRUE will cause the XML returned to include all files
#' uploaded for File Upload and Signature fields for all records in the project,
#'  whereas FALSE will cause all such fields not to be included. NOTE: Setting
#'  this option to TRUE can make the export very large and may prevent it from
#'  completing if the project contains many files or very large files.
#'
#' @return httr::response() object
#' @export
#'
#' @examples
#' \dontrun{
#' ## full export from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' redcap_export_project_xml(
#'   token,
#'   append_timestamp("static.xml"),
#'   exportFiles = TRUE
#' )
#'
#' ## full export from mother
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 6785)$token
#' redcap_export_project_xml(
#'   token,
#'   append_timestamp("mother.xml"),
#'   exportFiles = TRUE
#' )
#'
#' ## one record and two fields from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' redcap_export_project_xml(
#'   token,
#'   append_timestamp("partial_static.xml"),
#'   records = redcap_array("records", 16227),
#'   fields = redcap_array("fields", c("id", "updatedate"))
#' )
#' }
redcap_export_project_xml <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           filename = append_timestamp("redcap_project.xml"),
           overwrite = FALSE,
           return_metadata_only = FALSE,
           records,
           fields,
           events,
           return_format = c("xml", "json", "csv"),
           export_survey_fields = FALSE,
           export_data_access_groups = FALSE,
           filter_logic = NULL,
           export_files = FALSE) {
    filename <- paste0(filename, ".xml")

    body <- list(token = token, content = "project_xml")

    body <- append(
      body,
      redcap_logical("returnMetadataOnly", return_metadata_only)
    )

    if (missing(records)) {
      records <- NULL
    } else {
      checkmate::assert(checkmate::check_class(records, "redcap_array"))
    }
    body <- append(body, records)

    if (missing(fields)) {
      fields <- NULL
    } else {
      checkmate::assert(checkmate::check_class(fields, "redcap_array"))
    }
    body <- append(body, fields)

    if (missing(events)) {
      events <- NULL
    } else {
      checkmate::assert(checkmate::check_class(events, "redcap_array"))
    }
    body <- append(body, events)

    body <- append(
      body,
      list("returnFormat" = match.arg(return_format))
    )

    body <- append(body, redcap_logical(
      "exportSurveyFields",
      export_survey_fields
    ))

    body <- append(body, redcap_logical(
      "exportDataAccessGroups",
      export_data_access_groups
    ))

    if (!is.null(filter_logic)) {
      filter_logic <- list("filterLogic" = filter_logic)
    }
    body <- append(body, filter_logic)

    body <- append(body, redcap_logical(
      "exportFiles",
      export_files
    ))

    httr::POST(redcap_uri, httr::write_disk(filename, overwrite), body = body)
  }

#' Delete Records
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param records an array of record names specifying specific records you wish
#' to delete
#' @param arm the arm number of the arm in which the record(s) should be
#' deleted. (This can only be used if the project is longitudinal with more than
#'  one arm.) NOTE: If the arm parameter is not provided, the specified records
#'  will be deleted from all arms in which they exist. Whereas, if arm is
#'  provided, they will only be deleted from the specified arm.
#'
#' @return httr::response() object containing the number of records deleted.
#' @export
#'
#' @examples
#' \dontrun{
#' ## delete two records from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' redcap_delete_records(
#'   token,
#'   records = redcap_array("records", c(16227, 16342))
#' )
#' }
redcap_delete_records <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           records,
           arm) {
    body <- list(
      token = token,
      action = "delete",
      content = "record"
    )

    checkmate::assert(checkmate::check_class(records, "redcap_array"))
    body <- append(body, records)

    if (missing(arm)) {
      arm <- NULL
    } else {
      checkmate::assert(checkmate::check_integer(arm))
    }
    body <- append(body, list(arm = arm))

    httr::POST(redcap_uri, body = body)
  }

#' Import Records
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml \[default\], odm ('odm' refers to CDISC ODM XML
#' format, specifically ODM version 1.3.1)
#' @param type
#' * flat - output as one record per row \[default\]
#' * eav - input as one data point per row
#'     + Non-longitudinal: Will have the fields - record(1), field_name, value
#'     + Longitudinal: Will have the fields - record*, field_name, value,
#'     redcap_event_name(2)
#'
#' 1. 'record' refers to the record ID for the project
#' 2. Event name is the unique name for an event, not the event label
#' @param overwrite_behavior
#' * normal - blank/empty values will be ignored \[default\]
#' * overwrite - blank/empty values are valid and will overwrite data
#' @param force_auto_number If record auto-numbering has been enabled in the
#' project, it may be desirable to import records where each record's record
#' name is automatically determined by REDCap (just as it does in the user
#' interface). If this parameter is set to 'true', the record names provided in
#' the request will not be used (although they are still required in order to
#' associate multiple rows of data to an individual record in the request), but
#' instead those records in the request will receive new record names during the
#'  import process. NOTE: To see how the provided record names get translated
#'  into new auto record names, the returnContent parameter should be set to
#'  'auto_ids', which will return a record list similar to 'ids' value, but it
#'  will have the new record name followed by the provided record name in the
#'  request, in which the two are comma-delimited. For example, if false (or
#'  'false') - The record names provided in the request will be used.
#'  \[default\] true (or 'true') - New record names will be automatically
#'  determined.
#' @param data The formatted data to be imported.
#'
#' TIP: If importing repeating instances for a repeating event or repeating
#' instrument, you may auto-number the instances by providing a value of 'new'
#' for the 'redcap_repeat_instance' field in the dataset you are importing. This
#'  is useful because it allows you to import such data without the need to
#'  determine how many instances already exist for a given repeating
#'  event/instance prior to the import. NOTICE: The 'new' value option for
#'  auto-numbering instances does NOT work for 'eav' type data but only for
#'  'flat' type.
#'
#' NOTE: When importing data in EAV type format, please be aware that checkbox
#' fields must have their field_name listed as variable+'___'+optionCode and its
#'  value as either '0' or '1' (unchecked or checked, respectively). For
#'  example, for a checkbox field with variable name 'icecream', it would be
#'  imported as EAV with the field_name as 'icecream___4' having a value of '1'
#'  in order to set the option coded with '4' (which might be 'Chocolate') as '
#'  checked'.
#' @param date_format MDY, DMY, YMD \[default\] - the format of values being
#' imported for dates or datetime fields (understood with M representing
#' 'month', D as 'day', and Y as 'year') - NOTE: The default format is Y-M-D
#' (with dashes), while MDY and DMY values should always be formatted as M/D/Y
#' or D/M/Y (with slashes), respectively.
#' @param csv_delimiter Set the delimiter used to separate values in the CSV
#' data file (for CSV format only). Options include: comma ',' (default), 'tab',
#'  semi-colon ';', pipe '|', or caret '^'. Simply provide the value in quotes
#'  for this parameter.
#' @param return_content count \[default\] - the number of records imported, ids
#'  - a list of all record IDs that were imported, auto_ids = (used only when
#'  forceAutoNumber=true) a list of pairs of all record IDs that were imported,
#'  includes the new ID created and the ID value that was sent in the API
#'  request (e.g., 323,10).
#' @param return_format csv, json, xml - specifies the format of error messages.
#'  If you do not pass in this flag, it will select the default format for you
#'  passed based on the 'format' flag you passed in or if no format flag was
#'  passed in, it will default to 'xml'.
#'
#' @return httr::response() object containing the number of records deleted.
#' @export
#'
#' @examples
#' \dontrun{
#' ## csv eav example to edit a single data point
#' ### write data to disk and read back in to get correctly parsed csv string
#' csv_data <- data.frame(
#'   record = 2,
#'   field_name = "text_box",
#'   value = "a new value"
#' )
#' csv_file <- tempfile("data", fileext = ".csv")
#' write.csv(csv_data, csv_file, row.names = FALSE)
#' data <- paste(readLines(csv_file), collapse = "\n")
#'
#' ### retreive credentials
#' path <- system.file("misc/example.credentials", package = "REDCapR")
#' p1 <- REDCapR::retrieve_credential_local(path, 153L)
#'
#' ### submit api request and check response
#' httr::content(
#'   redcap_import_records(
#'     token = p1$token,
#'     format = "csv",
#'     type = "eav",
#'     data = data,
#'     return_format = "json"
#'   )
#' )
#' }
redcap_import_records <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           format = c("xml", "csv", "json", "odm"),
           type = c("flat", "eav"),
           overwrite_behavior = c("normal", "overwrite"),
           force_auto_number = FALSE,
           data,
           date_format = c("YMD", "MDY", "DMY"),
           csv_delimiter = c(",", "tab", ";", "|", "^"),
           return_content = c("count", "ids", "auto_ids"),
           return_format = c("xml", "csv", "json")) {
    format <- match.arg(format)
    type <- match.arg(type)
    overwrite_behavior <- match.arg(overwrite_behavior)
    checkmate::assert_logical(force_auto_number)
    date_format <- match.arg(date_format)
    csv_delimiter <- match.arg(csv_delimiter)
    return_content <- match.arg(return_content)
    return_format <- match.arg(return_format)

    body <- list(
      "token" = token,
      "content" = "record",
      "format" = format,
      "type" = type,
      "overwriteBehavior" = overwrite_behavior,
      "forceAutoNumber" = tolower(as.character(force_auto_number)),
      "data" = data,
      "dateFormat" = date_format,
      "csvDelimiter" = csv_delimiter,
      "returnContent" = return_content,
      "returnFormat" = return_format
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Export Project Information
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format format of the response content
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return httr::response() object containing the project information
#' @export
redcap_export_project_information <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           format = c("xml", "csv", "json"),
           return_format = c("xml", "csv", "json")) {
    format <- match.arg(format)
    return_format <- match.arg(return_format)

    body <- list(
      "token" = token,
      "content" = "project",
      "format" = format,
      "returnFormat" = return_format
    )

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Export a File
#'
#' @description This method allows you to download a document that has been
#' attached to an individual record for a File Upload field. Please note that
#' this method may also be used for Signature fields (i.e. File Upload fields
#' with 'signature' validation type).
#'
#' @note Please be aware that Data Export user rights will be applied to this
#' API request. For example, if you have 'No Access' data export rights in the
#' project, then the API file export will fail and return an error. And if you
#' have 'De-Identified' or 'Remove all tagged Identifier fields' data export
#' rights, then the API file export will fail and return an error *only if* the
#' File Upload field has been tagged as an Identifier field. To make sure that
#' your API request does not return an error, you should have 'Full Data Set'
#' export rights in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param record the record ID
#' @param field the name of the field that contains the file
#' @param event the unique event name - only for longitudinal projects
#' @param repeat_instance (only for projects with repeating instruments/events)
#' The repeat instance number of the repeating event (if longitudinal) or the
#' repeating instrument (if classic or longitudinal). Default value is '1'.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return httr::response() object containing the path to the file on disk
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_export_file(
#'   redcap_uri =
#'     "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
#'   token = my_project_token,
#'   record = 1,
#'   field = "file_field_name"
#' )
#' }
redcap_export_file <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           record,
           field,
           event,
           repeat_instance,
           return_format = c("xml", "csv", "json")) {
    return_format <- match.arg(return_format)

    body <- list(
      "token" = token,
      "content" = "file",
      "action" = "export",
      "record" = record,
      "field" = field,
      "returnFormat" = return_format
    )

    if (missing(event) || is.null(event)) {
      event <- ""
    } else {
      checkmate::assert(checkmate::check_character(event))
    }
    body <- append(body, list("event" = event))

    if (missing(repeat_instance) || is.null(repeat_instance)) {
      repeat_instance <- NULL
    } else {
      checkmate::assert(checkmate::check_integer(repeat_instance))
    }
    body <- append(body, list("repeat_instance" = repeat_instance))

    # write to temp file because do not know name yet
    tmp <- tempfile()
    r <- httr::POST(
      redcap_uri,
      body = body,
      encode = "form",
      httr::write_disk(tmp)
    )

    # retrieve the desired file name from header and construct absolute path
    fname <- parse_content_type(r[["headers"]][["content-type"]])[["name"]]
    fpath <- file.path(tempdir(), fname)

    # rename temp file to desired
    file.rename(r$content[[1]], fpath)

    content_type <- sub(basename(tmp), fname, r[["headers"]][["content-type"]])
    attr(fpath, "class") <- "path"

    # edit httr response object to reflect new download name
    r[["headers"]][["content-type"]] <- content_type
    r[["all_headers"]][[1]][["headers"]][["content-type"]] <- content_type
    r[["content"]] <- fpath
    r[["request"]][["output"]][["path"]] <- fpath[[1]]

    r
  }

#' Import a File
#'
#' @description This method allows you to upload a document that will be
#' attached to an individual record for a File Upload field. Please note that
#' this method may NOT be used for Signature fields (i.e. File Upload fields
#' with 'signature' validation type) because a signature can only be captured
#' and stored using the web interface.
#'
#' @note To use this method, you must have API Import/Update privileges in the
#' project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param record the record ID
#' @param field the name of the field that contains the file
#' @param event the unique event name - only for longitudinal projects
#' @param repeat_instance (only for projects with repeating instruments/events)
#' The repeat instance number of the repeating event (if longitudinal) or the
#' repeating instrument (if classic or longitudinal). Default value is '1'.
#' @param file path to the file to upload
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return httr::response() object reporting empty body on successful upload
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_import_file(
#'   redcap_uri =
#'     "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
#'   token = my_project_token,
#'   record = 1,
#'   field = "file_field_name"
#' )
#' }
redcap_import_file <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           record,
           field,
           event,
           repeat_instance,
           file,
           return_format = c("xml", "csv", "json")) {
    return_format <- match.arg(return_format)

    body <- list(
      "token" = token,
      "content" = "file",
      "action" = "import",
      "record" = record,
      "field" = field,
      "file" = httr::upload_file(file),
      "returnFormat" = return_format
    )

    if (missing(event) || is.null(event)) {
      event <- ""
    } else {
      checkmate::assert(checkmate::check_character(event))
    }
    body <- append(body, list("event" = event))

    if (missing(repeat_instance) || is.null(repeat_instance)) {
      repeat_instance <- NULL
    } else {
      checkmate::assert(checkmate::check_integer(repeat_instance))
    }
    body <- append(body, list("repeat_instance" = repeat_instance))

    httr::POST(redcap_uri, body = body, encode = "multipart")
  }

#' Migrate a File from One Project to Another
#'
#' @param redcap_uri_src The URI (uniform resource identifier) of the source
#' REDCap project.
#' @param redcap_uri_dst The URI (uniform resource identifier) of the
#' destination REDCap project.
#' @param token_src The API token specific to your source REDCap project and
#' username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param token_dst The API token specific to your destination REDCap project
#' and username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param record the record ID
#' @param field the name of the field that contains the file
#' @param event the unique event name - only for longitudinal projects
#' @param repeat_instance (only for projects with repeating instruments/events)
#' The repeat instance number of the repeating event (if longitudinal) or the
#' repeating instrument (if classic or longitudinal). Default value is '1'.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return a pair of httr response objects: one for the export and one for the
#' import
#' @export
redcap_migrate_file <- function(redcap_uri_src,
                                redcap_uri_dst,
                                token_src,
                                token_dst,
                                record,
                                field,
                                event,
                                repeat_instance,
                                return_format = c("xml", "csv", "json")) {
  return_format <- match.arg(return_format)

  r_export <- redcap_export_file(
    redcap_uri = redcap_uri_src,
    token = token_src,
    record = record,
    field = field,
    event = event,
    repeat_instance = repeat_instance,
    return_format = return_format
  )

  r_import <- redcap_import_file(
    token = token_dst,
    redcap_uri = redcap_uri_dst,
    record = record,
    field = field,
    event = event,
    repeat_instance = repeat_instance,
    file = r_export$content[[1]],
    return_format = return_format
  )

  list(
    "export" = r_export,
    "import" = r_import
  )
}

#' Migrate all Files from One Project to Another
#'
#' @param redcap_uri_src The URI (uniform resource identifier) of the source
#' REDCap project.
#' @param redcap_uri_dst The URI (uniform resource identifier) of the
#' destination REDCap project.
#' @param token_src The API token specific to your source REDCap project and
#' username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param token_dst The API token specific to your destination REDCap project
#' and username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return for each file identified, a pair of httr response objects: one for
#' the export and one for the import
#' @export
redcap_migrate_files <- function(redcap_uri_src,
                                 redcap_uri_dst,
                                 token_src,
                                 token_dst,
                                 return_format = c("xml", "csv", "json")) {
  return_format <- match.arg(return_format)

  # identify if project is longitudinal and/or has repeating events
  # so that we can deal with such project designs
  redcap_export_project_information(
    redcap_uri = redcap_uri_src,
    token = token_src,
    format = "json"
  ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() -> project_info

  # download entire data dictionary
  # 1. to identify record_id field name
  # 2. to identify names of fields of type file
  REDCapR::redcap_metadata_read(
    redcap_uri = redcap_uri_src,
    token = token_src
  )$data -> data_dictionary

  # get record_id field name (in case of customization)
  data_dictionary[[1, 1]] -> field_names_record_id

  # vector of key (i.e., record identifying) variables
  c(
    field_names_record_id,
    dplyr::if_else(
      project_info$is_longitudinal == 1,
      "redcap_event_name",
      NULL
    ),
    dplyr::if_else(
      project_info$has_repeating_instruments_or_events == 1,
      "redcap_repeat_instance",
      NULL
    )
  ) -> field_names_keys

  # get names of fields of type file
  data_dictionary %>%
    dplyr::filter(field_type == "file") %>% # (signatures are also files)
    dplyr::pull(field_name) -> field_names_files

  # download data for necessary fields only
  REDCapR::redcap_read(
    redcap_uri = redcap_uri_src,
    token = token_src,
    fields_collapsed = paste(
      c(field_names_keys, field_names_files),
      collapse = ","
    )
  )$data -> d

  # for each file field migrate each file
  purrr::map(
    field_names_files,
    ~ {
      # for each file field
      file_field_name <- .

      # identify files that need to be migrated
      d %>%
        dplyr::select(dplyr::any_of(c(field_names_keys, file_field_name))) %>%
        dplyr::filter(!is.na(.data[[file_field_name]])) -> d_files_to_migrate

      # and migrate each file
      purrr::map(
        1:nrow(d_files_to_migrate),
        ~ redcap_migrate_file(
          redcap_uri_src = redcap_uri_src,
          redcap_uri_dst = redcap_uri_dst,
          token_src = token_src,
          token_dst = token_dst,
          record = d_files_to_migrate[[., field_names_record_id]],
          field = file_field_name,
          event = d_files_to_migrate[[., "redcap_event_name"]],
          repeat_instance = as.integer(
            d_files_to_migrate[[., "redcap_repeat_instance"]]
          )
        )
      )
    }
  )
}

#' Create A New Project
#'
#' @description  This method allows you to create a new REDCap project. A
#' 64-character Super API Token is required for this method (as opposed to
#' project-level API methods that require a regular 32-character token
#' associated with the project-user). In the API request, you must minimally
#' provide the project attributes 'project_title' and 'purpose' (with numerical
#' value 0=Practice/Just for fun, 1=Other, 2=Research, 3=Quality Improvement,
#' 4=Operational Support) when creating a project.
#'
#' When a project is created with this method, the project will automatically be
#' given all the project-level defaults just as if you created a new empty
#' project via the web user interface, such as a automatically creating a
#' single data collection instrument seeded with a single Record ID field and
#' Form Status field, as well as (for longitudinal projects) one arm with one
#' event. And if you intend to create your own arms or events immediately after
#' creating the project, it is recommended that you utilize the override=1
#' parameter in the 'Import Arms' or 'Import Events' method, respectively, so
#' that the default arm and event are removed when you add your own. Also, the
#' user creating the project will automatically be added to the project as a
#' user with full user privileges and a project-level API token, which could
#' then be used for subsequent project-level API requests.
#'
#' NOTE: Only users with Super API Tokens can utilize this method. Users can
#' only be granted a super token by a REDCap administrator (using the API Tokens
#' page in the REDCap Control Center). Please be advised that users with a
#' Super API Token can create new REDCap projects via the API without any
#' approval needed by a REDCap administrator. If you are interested in
#' obtaining a super token, please contact your local REDCap administrator.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The Super API Token specific to a user
#' @param format format of the data argument
#' @param data Contains the attributes of the project to be created, in which
#' they are provided in the specified format. While the only required attributes
#' are 'project_title' and 'purpose', the fields listed below are all the
#' possible attributes that can be provided in the 'data' parameter. The
#' 'purpose' attribute must have a numerical value (0=Practice/Just for fun,
#' 1=Other, 2=Research, 3=Quality Improvement, 4=Operational Support), in which
#' 'purpose_other' is only required to have a value (as a text string) if
#' purpose=1. The attributes is_longitudinal (0=False, 1=True; Default=0),
#' surveys_enabled (0=False, 1=True; Default=0), and
#' record_autonumbering_enabled (0=False, 1=True; Default=1) are all boolean.
#' Please note that either is_longitudinal=1 or surveys_enabled=1 does not add
#' arms/events or surveys to the project, respectively, but it merely enables
#' those settings which are seen at the top of the project's Project Setup page.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#' @param odm default: NULL - The 'odm' parameter must be an XML string in CDISC
#' ODM XML format that contains project metadata (fields, forms, events, arms)
#' and might optionally contain data to be imported as well. The XML contained
#' in this parameter can come from a REDCap Project XML export file from REDCap
#' itself, or may come from another system that is capable of exporting
#' projects and data in CDISC ODM format. If the 'odm' parameter is included in
#' the API request, it will use the XML to import its contents into the newly
#' created project. This will allow you not only to create the project with the
#' API request, but also to import all fields, forms, and project attributes
#' (and events and arms, if longitudinal) as well as record data all at the same
#' time.
#'
#' @return an httr response object containing a 32-character project-level API
#' token (associated with both the project and user creating the project).
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_create_project(
#'   token = my_super_token,
#'   format = "json",
#'   data = paste0(
#'     "[",
#'     jsonlite::toJSON(
#'       list(
#'         "project_title" = "My New REDCap Project",
#'         "purpose" = "0"
#'       ),
#'       auto_unbox = TRUE
#'     ),
#'     "]"
#'   )
#' ) %>%
#'   httr::content(as = "text") -> my_project_token
#' }
redcap_create_project <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           format = c("xml", "csv", "json"),
           data,
           return_format = c("xml", "csv", "json"),
           odm) {
    return_format <- match.arg(return_format)

    body <- list(
      "token" = token,
      "content" = "project",
      "format" = format,
      "data" = data,
      "returnFormat" = return_format
    )

    if (missing(odm) || is.null(odm)) {
      odm <- NULL
    } else {
      checkmate::assert(checkmate::check_character(odm))
    }
    body <- append(body, list("odm" = odm))

    httr::POST(redcap_uri, body = body, encode = "form")
  }

#' Create Project from ODM
#'
#' @description  This method allows you to create a new REDCap project. A
#' 64-character Super API Token is required for this method (as opposed to
#' project-level API methods that require a regular 32-character token
#' associated with the project-user). In the API request, you must minimally
#' provide the project attributes 'project_title' and 'purpose' (with numerical
#' value 0=Practice/Just for fun, 1=Other, 2=Research, 3=Quality Improvement,
#' 4=Operational Support) when creating a project.
#'
#' When a project is created with this method, the project will automatically be
#' given all the project-level defaults just as if you created a new empty
#' project via the web user interface, such as a automatically creating a
#' single data collection instrument seeded with a single Record ID field and
#' Form Status field, as well as (for longitudinal projects) one arm with one
#' event. And if you intend to create your own arms or events immediately after
#' creating the project, it is recommended that you utilize the override=1
#' parameter in the 'Import Arms' or 'Import Events' method, respectively, so
#' that the default arm and event are removed when you add your own. Also, the
#' user creating the project will automatically be added to the project as a
#' user with full user privileges and a project-level API token, which could
#' then be used for subsequent project-level API requests.
#'
#' NOTE: Only users with Super API Tokens can utilize this method. Users can
#' only be granted a super token by a REDCap administrator (using the API Tokens
#' page in the REDCap Control Center). Please be advised that users with a
#' Super API Token can create new REDCap projects via the API without any
#' approval needed by a REDCap administrator. If you are interested in
#' obtaining a super token, please contact your local REDCap administrator.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The Super API Token specific to a user
#' @param project_title (required) title of the project
#' @param purpose (required) purpose of the project
#' @param purpose_other (required if purpose is "Other") custom purpose
#' @param project_notes (optional) notes describing the project
#' @param is_longitudinal is the project to be longitudinal
#' @param surveys_enabled is the project to contain surveys
#' @param record_autonumbering_enabled should the records be autonumbered
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#' @param odm path to file containing an XML string in CDISC ODM XML format that
#' contains project metadata (fields, forms, events, arms) and might optionally
#' contain data to be imported as well. The XML contained in this parameter
#' can come from a REDCap Project XML export file from REDCap itself, or may
#' come from another system that is capable of exporting projects and data in
#' CDISC ODM format. If the 'odm' parameter is included in the API request, it
#' will use the XML to import its contents into the newly created project.
#' This will allow you not only to create the project with the API request,
#' but also to import all fields, forms, and project attributes (and events
#' and arms, if longitudinal) as well as record data all at the same time.
#'
#' @return an httr response object containing a 32-character project-level API
#' token (associated with both the project and user creating the project).
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_create_project_from_odm(
#'   token = my_super_token,
#'   project_title = "redcap_create_project_from_odm test",
#'   purpose = "Practice/Just for fun",
#'   odm = "odm.xml"
#' ) %>%
#'   httr::content(r, as = "text") -> my_project_token
#' }
redcap_create_project_from_odm <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           project_title,
           purpose = c(
             "Practice/Just for fun",
             "Other",
             "Research",
             "Quality Improvement",
             "Operational Support"
           ),
           purpose_other,
           project_notes,
           is_longitudinal = FALSE,
           surveys_enabled = FALSE,
           record_autonumbering_enabled = FALSE,
           return_format = c("xml", "csv", "json"),
           odm) {
    purpose <- which(
      match.arg(purpose) == c(
        "Practice/Just for fun",
        "Other",
        "Research",
        "Quality Improvement",
        "Operational Support"
      )
    ) - 1

    data <- list(
      "project_title" = project_title,
      "purpose" = as.character(purpose)
    )

    if (purpose == 1) {
      if (missing(purpose_other) || is.null(purpose_other)) {
        purpose_other <- NULL
      } else {
        checkmate::assert(checkmate::check_character(purpose_other))
      }
      data <- append(data, list("purpose_other" = purpose_other))
    }

    if (missing(project_notes) || is.null(project_notes)) {
      project_notes <- NULL
    } else {
      checkmate::assert(checkmate::check_character(project_notes))
    }
    body <- append(body, list("project_notes" = project_notes))

    data <- append(data, list(
      "is_longitudinal" = as.character(as.integer(is_longitudinal)),
      "surveys_enabled" = as.character(as.integer(surveys_enabled)),
      "record_autonumbering_enabled" = as.character(
        as.integer(record_autonumbering_enabled)
      )
    ))

    redcap_create_project(
      redcap_uri = redcap_uri,
      token = token,
      format = "json",
      data = paste0("[", jsonlite::toJSON(data, auto_unbox = TRUE), "]"),
      return_format = match.arg(return_format),
      odm = paste(readLines(odm), collapse = "")
    )
  }
