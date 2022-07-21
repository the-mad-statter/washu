# REDCapR EXTENSIONS -----------------------------------------------------------

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
    redcap_export_project_info(
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

#' Edit tokens and other credentials in a file
#'
#' @param path_credential The file path to the CSV containing the credentials.
#' Required.
#'
#' @return Target path, invisibly.
#' @export
edit_credential_local <- function(path_credential = "~/.REDCapR") {
  r_user <- gsub("\\\\", "/", normalizePath("~"))
  path_credential <- sub("~", r_user, path_credential)
  usethis::edit_file(path_credential)
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

# UTILITIES --------------------------------------------------------------------

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

#' Parse Content Type
#'
#' @param content_type content-type header from REDCap to be parsed
#' @return list containing type, name, and charset
parse_content_type <- function (content_type) {
  parts <- stringr::str_remove_all(
    stringr::str_trim(strsplit(content_type, ";")[[1]]),
    '"'
  )

  list(
    "type" = parts[1],
    "name" = stringr::str_remove(parts[2], "name="),
    "charset" = stringr::str_remove(parts[3], "charset=")
  )
}

#' Generate Project XML Filename
#'
#' @param project_title name of the project
#'
#' @return a character representation of what REDCap would named the xml file
#' if it had been exported via the web interface
#'
#' @export
project_xml_filename <- function(project_title) {
  sprintf(
    "%s_%s.REDCap.xml",
    gsub(" ", "", project_title),
    format(Sys.time(), "%Y-%m-%d_%H%M")
  )
}

#' Read Project XML
#'
#' @param odm path to a file
#'
#' @return collapsed character representation of the project xml file
#'
#' @export
read_project_xml <- function(odm) {
  paste(readLines(odm), collapse = "")
}

#' Write Project XML
#'
#' @param r httr response obejct from redcap_export_project_xml()
#' @param file file or connection to write to
#'
#' @export
write_project_xml <- function(r, file) {
  r %>%
    httr::content() %>%
    as.character() %>%
    writeLines(con = file)
}

# API ENDPOINTS ----------------------------------------------------------------

## Data Access Groups ----------------------------------------------------------

#' Export DAGs
#'
#' @description This method allows you to export the Data Access Groups for a
#' project
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml [default]
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return httr::response() object containing DAGs for the project in the
#' format specified
#' @export
redcap_export_dags <- function(
  redcap_uri = "https://redcap.wustl.edu/redcap/api/",
  token,
  format = c("xml", "csv", "json"),
  return_format = c("xml", "csv", "json")
) {
  body <- list(
    "token" = token,
    "content" = "dag",
    "format" = match.arg(format),
    "returnFormat" = match.arg(return_format)
  )

  httr::POST(redcap_uri, body = body, encode = "form")
}

#' Import DAGs
#'
#' @description This method allows you to import new DAGs (Data Access Groups)
#' into a project or update the group name of any existing DAGs.
#'
#' @note DAGs can be renamed by simply changing the group name
#' (data_access_group_name). DAG can be created by providing group name value
#' while unique group name should be set to blank. To use this method, you must
#' have API Import/Update privileges in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml [default]
#' @param data Contains the attributes 'data_access_group_name'
#' (referring to the group name) and 'unique_group_name'
#' (referring to the auto-generated unique group name) of each DAG to be
#' created/modified, in which they are provided in the specified format.
#' 1. JSON Example:
#' \[{"data_access_group_name":"CA Site","unique_group_name":"ca_site"}
#' {"data_access_group_name":"FL Site","unique_group_name":"fl_site"},
#' {"data_access_group_name":"New Site","unique_group_name":""}\]
#' 2. CSV Example:
#' data_access_group_name,unique_group_name
#' "CA Site",ca_site
#' "FL Site",fl_site
#' "New Site",
#' XML Example:
#' <?xml version="1.0" encoding="UTF-8" ?>
#' <dags>
#'   <item>
#'   <data_access_group_name>CA Site</data_access_group_name>
#'   <unique_group_name>ca_site</unique_group_name>
#'   </item>
#'   <item>
#'   <data_access_group_name>FL Site</data_access_group_name>
#'   <unique_group_name>fl_site</unique_group_name>
#'   </item>
#'   <item>
#'   <data_access_group_name>New Site</data_access_group_name>
#'   <unique_group_name></unique_group_name>
#'   </item>
#' </dags>
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return httr::response() object containing number of DAGs added or updated
#' @export
redcap_import_dags <- function(
  redcap_uri = "https://redcap.wustl.edu/redcap/api/",
  token,
  format = c("xml", "csv", "json"),
  data,
  return_format = c("xml", "csv", "json")
) {
  body <- list(
    "token" = token,
    "content" = "dag",
    "action" = "import",
    "format" = match.arg(format),
    "data" = data,
    "returnFormat" = match.arg(return_format)
  )

  httr::POST(redcap_uri, body = body, encode = "form")
}

## Field Names -----------------------------------------------------------------

#' Export List of Export Field Names
#'
#' @description This method returns a list of the export/import-specific
#' version of field names for all fields (or for one field, if desired) in a
#' project. This is mostly used for checkbox fields because during data exports
#' and data imports, checkbox fields have a different variable name used than
#' the exact one defined for them in the Online Designer and Data Dictionary,
#' in which *each checkbox option* gets represented as its own export field
#' name in the following format: field_name + triple underscore + converted
#' coded value for the choice. For non-checkbox fields, the export field name
#' will be exactly the same as the original field name. Note: The following
#' field types will be automatically removed from the list returned by this
#' method since they cannot be utilized during the data import process: 'calc',
#' 'file', and 'descriptive'.
#'
#' The list that is returned will contain the three following attributes for
#' each field/choice: 'original_field_name', 'choice_value', and
#' 'export_field_name'. The choice_value attribute represents the raw coded
#' value for a checkbox choice. For non-checkbox fields, the choice_value
#' attribute will always be blank/empty. The export_field_name attribute
#' represents the export/import-specific version of that field name.
#'
#' @note To use this method, you must have API Export privileges in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param format csv, json, xml [default]
#' @param field A field's variable name. By default, all fields are returned,
#' but if field is provided, then it will only the export field name(s) for
#' that field. If the field name provided is invalid, it will return an error.
#' @param return_format csv, json, xml - specifies the format of error messages.
#' If you do not pass in this flag, it will select the default format for you
#' passed based on the 'format' flag you passed in or if no format flag was
#' passed in, it will default to 'xml'.
#'
#' @return httr::response() object containing Returns a list of the
#' export/import-specific version of field names for all fields (or for one
#' field, if desired) in a project in the format specified and ordered by their
#' field order . The list that is returned will contain the three following
#' attributes for each field/choice: 'original_field_name', 'choice_value',
#' and 'export_field_name'. The choice_value attribute represents the raw
#' coded value for a checkbox choice. For non-checkbox fields, the choice_value
#' attribute will always be blank/empty. The export_field_name attribute
#' represents the export/import-specific version of that field name.
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_export_field_names(
#'   token = my_token,
#'   format = "csv",
#'   field = "checkboxes"
#'  ) %>%
#'  httr::content(
#'    as = "text",
#'    encoding = "UTF-8",
#'    content_type = "text/csv"
#'  ) %>%
#'  readr::read_csv() %>%
#'  dplyr::pull(export_field_name)
#' }
redcap_export_field_names <- function(
  redcap_uri = "https://redcap.wustl.edu/redcap/api/",
  token,
  format = c("xml", "csv", "json"),
  field,
  return_format = c("xml", "csv", "json")
) {
  body <- list(
    "token" = token,
    "content" = "exportFieldNames",
    "format" = match.arg(format),
    "returnFormat" = match.arg(return_format)
  )

  if (missing(field) || is.null(field)) {
    field <- NULL
  } else {
    checkmate::assert(checkmate::check_character(field))
  }
  body <- append(body, list("field" = field))

  httr::POST(redcap_uri, body = body, encode = "form")
}

## Files -----------------------------------------------------------------------

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
    body <- list(
      "token" = token,
      "content" = "file",
      "action" = "export",
      "record" = record,
      "field" = field,
      "returnFormat" = match.arg(return_format)
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
    fpath <- gsub("\\\\", "/", file.path(tempdir(), fname))

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
    body <- list(
      "token" = token,
      "content" = "file",
      "action" = "import",
      "record" = record,
      "field" = field,
      "file" = httr::upload_file(file),
      "returnFormat" = match.arg(return_format)
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

## Projects --------------------------------------------------------------------

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
    body <- list(
      "token" = token,
      "content" = "project",
      "format" = format,
      "data" = data,
      "returnFormat" = match.arg(return_format)
    )

    if (missing(odm) || is.null(odm)) {
      odm <- NULL
    } else {
      checkmate::assert(checkmate::check_character(odm))
    }
    body <- append(body, list("odm" = odm))

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
redcap_export_project_info <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           format = c("xml", "csv", "json"),
           return_format = c("xml", "csv", "json")) {
    body <- list(
      "token" = token,
      "content" = "project",
      "format" = match.arg(format),
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
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
#'   exportFiles = TRUE
#' )
#'
#' ## full export from mother
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 6785)$token
#' redcap_export_project_xml(
#'   token,
#'   exportFiles = TRUE
#' )
#'
#' ## one record and two fields from static
#' token <- REDCapR::retrieve_credential_local("~/.REDCapR", 7842)$token
#' redcap_export_project_xml(
#'   token,
#'   records = redcap_array("records", 16227),
#'   fields = redcap_array("fields", c("id", "updatedate"))
#' )
#' }
redcap_export_project_xml <-
  function(redcap_uri = "https://redcap.wustl.edu/redcap/api/",
           token,
           return_metadata_only = FALSE,
           records,
           fields,
           events,
           return_format = c("xml", "json", "csv"),
           export_survey_fields = FALSE,
           export_data_access_groups = FALSE,
           filter_logic = NULL,
           export_files = FALSE) {
    # https://community.projectredcap.org/questions/81879/api-project-xml-export-missing-redcapsurveysgroup.html
    # - bug reported in v9.7.7 saying xml file missing a lot of the project settings
    # - essentially this endpoint can only export metadata + data only in versions prior to v9

    body <- list(
      token = token,
      content = "project_xml",
      format = "xml"
    )

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

    httr::POST(redcap_uri, body = body, encode = "form")
  }

## Records ---------------------------------------------------------------------

#' Export Records
#'
#' @description This method allows you to export a set of records for a project.
#'
#' @note Note about export rights: Please be aware that Data Export user rights
#' will be applied to this API request. For example, if you have 'No Access'
#' data export rights in the project, then the API data export will fail and
#' return an error. And if you have 'De-Identified' or 'Remove all tagged
#' Identifier fields' data export rights, then some data fields *might* be
#' removed and filtered out of the data set returned from the API. To make sure
#' that no data is unnecessarily filtered out of your API request, you should
#' have 'Full Data Set' export rights in the project.
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The Super API Token specific to a user
#' @param format csv, json, xml [default], odm ('odm' refers to CDISC ODM XML
#' format, specifically ODM version 1.3.1)
#' @param type * 'record' refers to the record ID for the project
#' \itemize{
#'   \item{flat}{- output as one record per row [default]}
#'   \item{eav}{- output as one data point per row
#'     \item{Non-longitudinal:}{record*, field_name, value}
#'     \item{Longitudinal:}{record*, field_name, value, redcap_event_name}
#'   }
#' }
#' @param records an array of record names specifying specific records you wish
#' to pull (by default, all records are pulled)
#' @param fields an array of field names specifying specific fields you wish to
#' pull (by default, all fields are pulled)
#' @param forms an array of form names you wish to pull records for. If the
#' form name has a space in it, replace the space with an underscore (by
#' default, all records are pulled)
#' @param events an array of unique event names that you wish to pull records
#' for - only for longitudinal projects
#' @param raw_or_label raw [default], label - export the raw coded values or
#' labels for the options of multiple choice fields
#' @param raw_or_label_headers raw [default], label - (for 'csv' format 'flat'
#' type only) for the CSV headers, export the variable/field names (raw) or the
#' field labels (label)
#' @param export_checkbox_label true, false [default] - specifies the format of
#' checkbox field values specifically when exporting the data as labels (i.e.,
#' when rawOrLabel=label) in flat format (i.e., when type=flat). When exporting
#' labels, by default (without providing the exportCheckboxLabel flag or if
#' exportCheckboxLabel=false), all checkboxes will either have a value
#' 'Checked' if they are checked or 'Unchecked' if not checked. But if
#' exportCheckboxLabel is set to true, it will instead export the checkbox
#' value as the checkbox option's label (e.g., 'Choice 1') if checked or it
#' will be blank/empty (no value) if not checked. If rawOrLabel=false or if
#' type=eav, then the exportCheckboxLabel flag is ignored. (The
#' exportCheckboxLabel parameter is ignored for type=eav because 'eav' type
#' always exports checkboxes differently anyway, in which checkboxes are
#' exported with their true variable name (whereas the 'flat' type exports them
#' as variable___code format), and another difference is that 'eav' type
#' *always* exports checkbox values as the choice label for labels export, or
#' as 0 or 1 (if unchecked or checked, respectively) for raw export.)
#' @param return_format csv, json, xml - specifies the format of error
#' messages. If you do not pass in this flag, it will select the default format
#' for you passed based on the 'format' flag you passed in or if no format flag
#' was passed in, it will default to 'xml'.
#' @param export_survey_fields true, false [default] - specifies whether or not
#' to export the survey identifier field (e.g., 'redcap_survey_identifier') or
#' survey timestamp fields (e.g., instrument+'_timestamp') when surveys are
#' utilized in the project. If you do not pass in this flag, it will default to
#' 'false'. If set to 'true', it will return the redcap_survey_identifier field
#' and also the survey timestamp field for a particular survey when at least
#' one field from that survey is being exported. NOTE: If the survey identifier
#' field or survey timestamp fields are imported via API data import, they will
#' simply be ignored since they are not real fields in the project but rather
#' are pseudo-fields.
#' @param export_data_access_groups true, false [default] - specifies whether
#' or not to export the 'redcap_data_access_group' field when data access
#' groups are utilized in the project. If you do not pass in this flag, it will
#' default to 'false'. NOTE: This flag is only viable if the user whose token
#' is being used to make the API request is *not* in a data access group. If
#' the user is in a group, then this flag will revert to its default value.
#' @param filter_logic String of logic text (e.g., [age] > 30) for filtering
#' the data to be returned by this API method, in which the API will only
#' return the records (or record-events, if a longitudinal project) where the
#' logic evaluates as TRUE. This parameter is blank/null by default unless a
#' value is supplied. Please note that if the filter logic contains any
#' incorrect syntax, the API will respond with an error message.
#' @param date_range_begin To return only records that have been created or
#' modified *after* a given date/time, provide a timestamp in the format
#' YYYY-MM-DD HH:MM:SS (e.g., '2017-01-01 00:00:00' for January 1, 2017 at
#' midnight server time). If not specified, it will assume no begin time.
#' @param date_range_end To return only records that have been created or
#' modified *before* a given date/time, provide a timestamp in the format
#' YYYY-MM-DD HH:MM:SS (e.g., '2017-01-01 00:00:00' for January 1, 2017 at
#' midnight server time). If not specified, it will use the current server time.
#' @param csv_delimiter Set the delimiter used to separate values in the CSV
#' data file (for CSV format only). Options include: comma ',' (default),
#' 'tab', semi-colon ';', pipe '|', or caret '^'. Simply provide the value in
#' quotes for this parameter.
#' @param decimal_character If specified, force all numbers into same decimal
#' format. You may choose to force all data values containing a decimal to have
#' the same decimal character, which will be applied to all calc fields and
#' number-validated text fields. Options include comma ',' or dot/full stop '.',
#'  but if left blank/null, then it will export numbers using the fields'
#'  native decimal format. Simply provide the value of either ',' or '.' for
#'  this parameter.
#'
#' @return httr::response() object
#' @export
#'
#' @examples \dontrun{
#' redcap_export_records(token = my_token)
#' }
redcap_export_records <- function(
  redcap_uri = "https://redcap.wustl.edu/redcap/api/",
  token,
  format = c("xml", "csv", "json", "odm"),
  type = c("flat", "eav"),
  records,
  fields,
  forms,
  events,
  raw_or_label = c("raw", "label"),
  raw_or_label_headers = c("raw", "label"),
  export_checkbox_label = FALSE,
  return_format = c("xml", "csv", "json"),
  export_survey_fields = FALSE,
  export_data_access_groups = FALSE,
  filter_logic,
  date_range_begin,
  date_range_end,
  csv_delimiter = c(",", "\t", ";", "|", "^"),
  decimal_character = c("native", ",", ".")
) {
  body <- list(
    "token" = token,
    "content" = "record",
    "format" = match.arg(format),
    "type" = match.arg(type),
    "rawOrLabel" = match.arg(raw_or_label),
    "rawOrLabelHeaders" = match.arg(raw_or_label_headers),
    "exportCheckboxLabel" = export_checkbox_label,
    "returnFormat" = match.arg(return_format),
    "exportSurveyFields" = export_survey_fields,
    "exportDataAccessGroups" = export_data_access_groups,
    "csvDelimiter" = match.arg(csv_delimiter),
    "decimalCharacter" = match.arg(decimal_character)
  )

  if (missing(records) || is.null(records))
    records <- NULL
  else
    records <- redcap_array(records)
  body <- append(body, list("records" = records))

  if (missing(fields) || is.null(fields))
    fields <- NULL
  else
    fields <- redcap_array(fields)
  body <- append(body, list("fields" = fields))

  if (missing(forms) || is.null(forms))
    forms <- NULL
  else {
    forms <- redcap_array(gsub(" ", "_", forms))
  }
  body <- append(body, list("forms" = forms))

  if (missing(events) || is.null(events))
    events <- NULL
  else
    events <- redcap_array(events)
  body <- append(body, list("events" = events))

  if (missing(filter_logic) || is.null(filter_logic)) {
    filter_logic <- NULL
  } else {
    checkmate::assert(checkmate::check_character(filter_logic))
  }
  body <- append(body, list("filterLogic" = filter_logic))

  if (missing(date_range_begin) || is.null(date_range_begin)) {
    date_range_begin <- NULL
  } else {
    checkmate::assert(checkmate::check_character(date_range_begin))

  }
  body <- append(body, list("dateRangeBegin" = date_range_begin))

  if (missing(date_range_end) || is.null(date_range_end)) {
    date_range_end <- NULL
  } else {
    checkmate::assert(checkmate::check_character(date_range_end))
  }
  body <- append(body, list("dateRangeEnd" = date_range_end))


  httr::POST(redcap_uri, body = body, encode = "form")
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
    body <- list(
      "token" = token,
      "content" = "record",
      "format" = match.arg(format),
      "type" = match.arg(type),
      "overwriteBehavior" = match.arg(overwrite_behavior),
      "forceAutoNumber" = unname(
        redcap_logical("forceAutoNumber", force_auto_number)
      ),
      "data" = data,
      "dateFormat" = match.arg(date_format),
      "csvDelimiter" = match.arg(csv_delimiter),
      "returnContent" = match.arg(return_content),
      "returnFormat" = match.arg(return_format)
    )

    httr::POST(redcap_uri, body = body, encode = "form")
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

# COMPLEX FUNCTIONS ------------------------------------------------------------

#' Create Project
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
redcap_create_project2 <-
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
redcap_migrate_file <- function(
  redcap_uri_src = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
  redcap_uri_dst = "https://redcap.wustl.edu/redcap/api/",
  token_src,
  token_dst,
  record,
  field,
  event,
  repeat_instance,
  return_format = c("xml", "csv", "json")
) {
  redcap_export_file(
    redcap_uri = redcap_uri_src,
    token = token_src,
    record = record,
    field = field,
    event = event,
    repeat_instance = repeat_instance,
    return_format = match.arg(return_format)
  ) -> r_export

  base_name <- basename(r_export$content[[1]])

  message(sprintf("Downloading file: %s...", base_name))

  redcap_import_file(
    token = token_dst,
    redcap_uri = redcap_uri_dst,
    record = record,
    field = field,
    event = event,
    repeat_instance = repeat_instance,
    file = r_export$content[[1]],
    return_format = match.arg(return_format)
  ) -> r_import

  message(sprintf("Uploading file: %s...", base_name))

  list(
    "export" = r_export,
    "import" = r_import
  )
}

#' Migrate All Files from One Project to Another
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
#' @export
redcap_migrate_files <- function(
  redcap_uri_src = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
  redcap_uri_dst = "https://redcap.wustl.edu/redcap/api/",
  token_src,
  token_dst,
  return_format = c("xml", "csv", "json")
) {
  message("Reading source project data dictionary...")
  REDCapR::redcap_metadata_read(
    redcap_uri = redcap_uri_src,
    token = token_src
  )$data -> data_dictionary

  message("Reading source project records...")
  REDCapR::redcap_read(
    redcap_uri = redcap_uri_src,
    token = token_src,
    export_survey_fields = TRUE,
    export_data_access_groups = TRUE
  )$data -> project_records

  # temporarily change signature fields into regular upload fields
  message("Disabling destination project signature constraints...")
  REDCapR::redcap_metadata_write(
    ds = data_dictionary %>%
      dplyr::mutate(
        text_validation_type_or_show_slider_number =
          dplyr::if_else(
            text_validation_type_or_show_slider_number == "signature",
            NA_character_,
            text_validation_type_or_show_slider_number
          )
      ),
    redcap_uri = redcap_uri_dst,
    token = token_dst
  )

  # get names of fields of type file
  data_dictionary %>%
    dplyr::filter(field_type == "file") %>%
    dplyr::pull(field_name) -> file_field_names

  # for each file field find records requiring a file migration and do it
  for(file_field_name in file_field_names) {
    message(sprintf("Transfering files for field: %s...", file_field_name))

    # find records with files in the current field
    project_records %>%
      dplyr::select(
        dplyr::any_of(
          c(
            data_dictionary[[1, 1]],
            "redcap_event_name",
            "redcap_repeat_instance",
            file_field_name
          )
        )
      ) %>%
      dplyr::filter(
        !is.na(.data[[file_field_name]])
      ) -> files_for_migration

    # migrate the files
    for(k in 1:nrow(files_for_migration)) {
      redcap_migrate_file(
        redcap_uri_src = redcap_uri_src,
        redcap_uri_dst = redcap_uri_dst,
        token_src = token_src,
        token_dst = token_dst,
        record = files_for_migration[[k, data_dictionary[[1, 1]]]],
        field = file_field_name,
        event = files_for_migration[[k, "redcap_event_name"]],
        repeat_instance = as.integer(
          files_for_migration[[k, "redcap_repeat_instance"]]
        ),
        return_format = match.arg(return_format)
      )
    }
  }

  # add signature constraint back to original signature fields
  message("Enabling destination project signature constraints...")
  REDCapR::redcap_metadata_write(
    ds = data_dictionary,
    redcap_uri = redcap_uri_dst,
    token = token_dst
  ) -> r
}

#' Reset Completion Flags
#'
#' @description When exporting records containing instrument completion flag
#' fields from REDCap (regardless of format), REDCap maps both
#' grey/no sql table record/"Incomplete (no data saved)" and
#' red/1/"Incomplete" to red/1/"Incomplete". Because it is difficult to
#' determine which records should have remained
#' grey/no sql table record/"Incomplete (no data saved)", this function
#' assumes instruments with no data present should be set to
#' grey/no sql table record/"Incomplete (no data saved)"
#'
#' @param redcap_uri The URI (uniform resource identifier) of the REDCap
#' project.
#' @param token The API token specific to your REDCap project and username (each
#'  token is unique to each user for each project). See the section on the
#'  left-hand menu for obtaining a token for a given project.
#' @param data_dictionary (optional) If supplied should be data object from a
#' call to REDCapR::redcap_metadata_read(). If not supplied the function will
#' call REDCapR::redcap_metadata_read().
#' @param project_records (optional) If supplied should be the data object from
#' a call to REDCapR::redcap_read(). If not supplied the function will call
#' REDCapR::redcap_read().
redcap_reset_completion_flags <- function(
  redcap_uri = "https://redcap.wustl.edu/redcap/api/",
  token,
  data_dictionary,
  project_records
) {
  warning(paste0(
    "This function does not work as intended.\n",
    "See function definition for more information."
  ))

  # the logic of this function works and the api accepts the write, but REDCap
  # v10 reverts to red and not grey as intended
  #
  # the following test code can be used to see the effect on a single record
  #
  # tmp <- tempfile(fileext = ".csv")
  # dplyr::tibble(
  #   record = 3,
  #   field_name = "instrument_1_complete",
  #   value = "", # "": Incomplete; 0: Incomplete; 1: Unverified; 2: Complete
  #   redcap_event_name = "event_1_arm_1",
  #   redcap_repeat_instrument = "instrument_1",
  #   redcap_repeat_instance = 1
  # ) %>%
  #   readr::write_csv(tmp)
  #
  # washu::redcap_import_records(
  #   redcap_uri = "https://redcap.wustl.edu/redcap/api/",
  #   token = "211F2459A749042B342EC8A4DB569F4E",
  #   format = "csv",
  #   type = "eav",
  #   overwrite_behavior = "overwrite",
  #   data = paste(readLines(tmp), collapse = "\n"),
  #   return_content = "ids",
  #   return_format = "xml"
  # ) %>%
  #   httr::content() %>%
  #   as.character()

  # and here are pseudo sql queries that might work to do it in the backend
  # DELETE D
  # FROM redcap_data D
  # LEFT JOIN redcap_events_metadata E ON D.event_id=E.event_id
  # WHERE D.project_id='13006' AND D.record='3' AND D.field_name='instrument_1_complete' AND E.descrip='Event 1' AND D.instance IS NULL
  #
  # DELETE D
  # FROM redcap_data D
  # LEFT JOIN redcap_events_metadata E ON D.event_id=E.event_id
  # WHERE D.project_id='13006' AND D.record='3' AND D.field_name='instrument_1_complete' AND E.descrip='Event 1' AND D.instance='2'

  if(missing(data_dictionary) || is.null(data_dictionary)) {
    REDCapR::redcap_metadata_read(
      redcap_uri = redcap_uri,
      token = token
    )$data -> data_dictionary
  }

  if(missing(project_records) || is.null(project_records)) {
    REDCapR::redcap_read(
      redcap_uri = redcap_uri,
      token = token,
      export_survey_fields = TRUE,
      export_data_access_groups = TRUE
    )$data -> project_records
  }

  for(form in unique(data_dictionary$form_name)) {
    # generate vector of checkbox field names for form
    data_dictionary %>%
      dplyr::filter(form_name == form) %>%
      dplyr::filter(field_type == "checkbox") %>%
      dplyr::mutate(
        n_choices = stringr::str_count(select_choices_or_calculations, "\\|")
      ) %>%
      dplyr::select(field_name, n_choices) %>%
      purrr::pmap(
        function(field_name, n_choices) {
          paste0(field_name, "___", 0:n_choices)
        }
      ) %>%
      unlist() -> checkbox_field_names

    # generate vector of non-checkbox field names for form
    data_dictionary %>%
      dplyr::filter(form_name == form) %>%
      dplyr::filter(
        field_name != data_dictionary[[1, 1]],
        field_type != "checkbox",
        field_type != "descriptive"
      ) %>%
      dplyr::pull(field_name) -> noncheckbox_field_names

    # count not missing for form for record-event-instance
    project_records %>%
      dplyr::select(
        dplyr::all_of(
          c(
            noncheckbox_field_names,
            checkbox_field_names
          )
        )
      ) %>%
      dplyr::mutate_at(
        dplyr::all_of(checkbox_field_names),
        ~ dplyr::if_else(. == 0, NA_real_, .)
      ) %>%
      dplyr::mutate(n_nna = rowSums(!is.na(.))) %>%
      dplyr::pull(n_nna) -> n_nna

    # make dataframe of flags to set to grey "Incomplete (no data saved)"
    project_records %>%
      dplyr::mutate(n_nna = n_nna) %>%
      dplyr::filter(n_nna == 0) %>%
      dplyr::select(
        dplyr::any_of(
          c(
            data_dictionary[[1, 1]],
            "redcap_event_name",
            "redcap_repeat_instance",
            paste0(form, "_complete")
          )
        )
      ) -> d

    if(nrow(d) > 0) {
      d[paste0(form, "_complete")] <- ""

      # write flags to REDCap
      REDCapR::redcap_write(
        ds_to_write = d,
        redcap_uri = redcap_uri,
        token = token
      ) -> r
    }
  }
}

#' Project Migration App
#'
#' @description This shiny app migrates most project settings from one REDCap
#' instance to another by using API calls. It was written to help transfer
#' projects on a v7 instance to a v10 instance where version inconsistencies as
#' well as technical and labor issues prevented a simple CDISC ODM XML port.
#'
#' @param redcap_uri_src The URI (uniform resource identifier) of the source
#' REDCap project.
#' @param redcap_uri_dst The URI (uniform resource identifier) of the
#' destination REDCap project.
#' @param token_src The API token specific to your source REDCap project and
#' username (each token is unique to each user for each project). See the
#' section on the left-hand menu for obtaining a token for a given project.
#' @param token_spr The Super API Token specific to a user
#' @param redcap_dag_uri_src The base URI for the source DAG page
#' @param redcap_dag_uri_dst The base URI for the destination DAG page
#'
#' @export
redcap_project_migration_app <- function(
  redcap_uri_src = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
  redcap_uri_dst = "https://redcap.wustl.edu/redcap/api/",
  token_src = "",
  token_spr = "",
  redcap_dag_uri_src = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/redcap_v7.3.5/DataAccessGroups/index.php?pid=",
  redcap_dag_uri_dst = "https://redcap.wustl.edu/redcap/redcap_v10.6.28/index.php?route=DataAccessGroupsController:index&pid="
) {
  ui <- function() {
    shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$script("
          Shiny.addCustomMessageHandler('close-window', function(x) {
            window.open('', '_self', '').close();
          });
        ")
      ),
      shiny::titlePanel("REDCap Project Migration App"),

      shiny::verticalLayout(
        shiny::passwordInput(
          "token_src",
          "Source Project Token",
          token_src,
          width = "100%"
        ),
        shiny::passwordInput(
          "token_spr",
          "Destination Super Token",
          token_spr,
          width = "100%"
        ),
        shiny::actionButton("migrate", "Migrate")
      )
    )
  }

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    rvs <- shiny::reactiveValues(
      token_dst = NULL # api token created during destination project creation
    )

    shiny::observeEvent(input$migrate, {
      shiny::withProgress({

        shiny::setProgress(0.00, "Initiating migration...")
        Sys.sleep(3)

        # these attributes will be copied to the new project during creation
        shiny::setProgress(0.05, "Exporting attributes from source...")
        redcap_export_project_info(
          redcap_uri = redcap_uri_src,
          token = input$token_src,
          format = "json"
        ) %>%
          httr::content() -> project_info_src

        # cdisc odm is the most complete/portable representation of the project
        # this will be used during the new project creation
        shiny::setProgress(0.10, "Exporting metadata from source...")
        redcap_export_project_xml(
          redcap_uri_src,
          input$token_src,
          return_metadata_only = TRUE
        ) %>%
          httr::content() %>%
          as.character() -> cdisc_odm_xml

        # create the new project and make a note of the project specific token
        shiny::setProgress(0.20, "Creating project at destination...")
        redcap_create_project(
          redcap_uri = redcap_uri_dst,
          token = input$token_spr,
          format = "json",
          data = paste0(
            "[", jsonlite::toJSON(project_info_src, auto_unbox = TRUE), "]"
          ),
          odm = cdisc_odm_xml
        ) %>%
          httr::content(as = "text", encoding = "UTF-8") -> rvs$token_dst

        # extract attributes from the new project to identify pid
        shiny::setProgress(0.30, "Exporting attributes from destination...")
        redcap_export_project_info(
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst,
          format = "json"
        ) %>%
          httr::content() -> project_info_dst

        Sys.sleep(3)
      })

      # data access groups definitions cannot be exported from v7 via the api
      # here we help the user manually define them as much as possible
      # if we allow database access here is sql query for use in conjunction
      # with redcap_import_dags():
      # SELECT * FROM redcap_data_access_groups WHERE project_id='6648'
      shinyalert::shinyalert(
        title = "DAGs?",
        text = paste0(
          "<p>Because data access groups (DAGs) definitions cannot be exported ",
          "via the API from REDCap v7, they must be manually migrated.</p><br/>",
          "<p>View the definitions in the source project ",
          "<a href=\"",
          redcap_dag_uri_src, project_info_src$project_id,
          "\" target = \"_blank\">here</a>.</p><br/>",
          "<p>Enter the definitions in the destination project ",
          "<a href=\"",
          redcap_dag_uri_dst, project_info_dst$project_id,
          "\" target = \"_blank\">here</a>.</p><br/>",
          "<p>Click OK after all DAGs have been manually migrated.</p><br/>"
        ),
        type = "info",
        html = TRUE,
        inputId = "dags_defined"
      )
    })

    # after dags have been dealt with
    shiny::observeEvent(input$dags_defined, {
      shiny::withProgress({
        # export records
        shiny::setProgress(0.35, "Exporting records from source...")
        REDCapR::redcap_read(
          redcap_uri = redcap_uri_src,
          token = input$token_src,
          export_survey_fields = TRUE,
          export_data_access_groups = TRUE
        )$data -> project_records

        # import records
        shiny::setProgress(0.45, "Importing records to destination...")
        REDCapR::redcap_write(
          ds_to_write = project_records,
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst
        )

        # read the data dictionary to:
        # a) identify fields of type file (this includes uploads and signatures)
        # b) to temporarily change signature fields to regular upload fields as
        #    the api does not allow signatures to be uploaded, but we can trick
        #    it by setting signature fields to be regular upload fields while
        #    uploading files and then reverting after the fact
        # Note: while signatures are also files, they are not importable:
        # <?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<hash>\n  <error>The field
        # 'var_8' is a signature field, which cannot be imported using the API but
        # can only be created using the web interface. However, it can be
        # downloaded or deleted using the API.</error>\n</hash>\n
        shiny::setProgress(0.60, "Reading data dictionary...")
        REDCapR::redcap_metadata_read(
          redcap_uri = redcap_uri_src,
          token = input$token_src
        )$data -> data_dictionary

        # temporarily change signature fields into regular upload fields
        shiny::setProgress(0.60, "Disabling signature constraints...")
        REDCapR::redcap_metadata_write(
          ds = data_dictionary %>%
            dplyr::mutate(
              text_validation_type_or_show_slider_number =
                dplyr::if_else(
                  text_validation_type_or_show_slider_number == "signature",
                  NA_character_,
                  text_validation_type_or_show_slider_number
                )
            ),
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst
        )

        # get names of fields of type file
        data_dictionary %>%
          dplyr::filter(field_type == "file") %>%
          dplyr::pull(field_name) -> field_names_files

        # compute a delta for updating the progress bar
        progress_field_delta <- (0.90 - 0.60) / length(field_names_files)

        # for each file field find records requiring a file migration and do it
        for(j in 1:length(field_names_files)) {
          shiny::setProgress(
            0.6 + j * progress_field_delta,
            sprintf("Transfering files for field: %s...", field_names_files[j]),
            ""
          )

          # find records with files in the current field
          project_records %>%
            dplyr::select(
              dplyr::any_of(
                c(
                  data_dictionary[[1, 1]],
                  "redcap_event_name",
                  "redcap_repeat_instance",
                  field_names_files[j]
                )
              )
            ) %>%
            dplyr::filter(
              !is.na(.data[[field_names_files[j]]])
            ) -> d_files_to_migrate

          # migrate the files
          for(k in 1:nrow(d_files_to_migrate)) {
            redcap_export_file(
              redcap_uri = redcap_uri_src,
              token = input$token_src,
              record = d_files_to_migrate[[k, data_dictionary[[1, 1]]]],
              field = field_names_files[j],
              event = d_files_to_migrate[[k, "redcap_event_name"]],
              repeat_instance = as.integer(
                d_files_to_migrate[[k, "redcap_repeat_instance"]]
              )
            ) -> r_export

            shiny::setProgress(
              message = sprintf(
                "Transfering files for field: %s...", field_names_files[j]
              ),
              detail = sprintf(
                "Downloading file: %s...", basename(r_export$content[[1]])
              )
            )

            redcap_import_file(
              token = rvs$token_dst,
              redcap_uri = redcap_uri_dst,
              record = d_files_to_migrate[[k, data_dictionary[[1, 1]]]],
              field = field_names_files[j],
              event = d_files_to_migrate[[k, "redcap_event_name"]],
              repeat_instance = as.integer(
                d_files_to_migrate[[k, "redcap_repeat_instance"]]
              ),
              file = r_export$content[[1]]
            ) -> r_import

            shiny::setProgress(
              message = sprintf(
                "Transfering files for field: %s...", field_names_files[j]
              ),
              detail = sprintf(
                "Uploading file: %s...", basename(r_export$content[[1]])
              )
            )
          }
        }

        # add signature constraint back to original signature fields
        shiny::setProgress(0.90, "Enabling signature constraints...")
        REDCapR::redcap_metadata_write(
          ds = data_dictionary,
          redcap_uri = redcap_uri_dst,
          token = rvs$token_dst
        )

        shiny::setProgress(1.00, "Completing migration...", "")
        Sys.sleep(3)
      })

      shinyalert::shinyalert(
        title = "Migration Complete",
        text = "Project migration has completed.",
        type = "info",
        inputId = "migration_complete"
      )
    })

    shiny::observeEvent(input$migration_complete, {
      session$sendCustomMessage("close-window", "")
    })
  }

  shiny::runGadget(ui(), server, viewer = shiny::browserViewer())
}

redcap_project_migration_app_test <- function() {
  redcap_project_migration_app(
    token_src = washu::retrieve_credential_local(8432)$token,
    token_spr = washu::retrieve_credential_local(0)$token
  )
}
