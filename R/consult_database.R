# TEMPLATE ---------------------------------------------------------------------

#' Consult Database Install from Template
#'
#' @param overwrite logical; should existing \code{\link{cdb_file}} be overwritten?
#' @return logical indicating if the copy succeeded
#' @export
cdb_template_install <- function(overwrite = FALSE) {
  file.copy(
    washu:::find_resource("template_resource", "consult_report", "consult_database.xlsx"),
    washu:::cdb_file(),
    overwrite
  )
}

#' Consult Database New Consult Data Frame
#' @param k consult id
#' @return an "empty" consult data.frame
cdb_template_consult <- function(k) {
  data.frame(
    c_id = k,
    c_title = "",
    c_src = "Other",
    c_src_ref = "",
    c_type = "Other",
    c_rqst_0 = Sys.Date(),
    c_rqst_1 = Sys.Date(),
    c_real_0 = Sys.Date(),
    c_real_1 = Sys.Date(),
    c_hours = "0",
    c_cmplt = "No"
  )
}

#' Consult Database New Consult Data Frame Column Labels
#' @return character vector representing column labels for \code{\link{cdb_template_consult}}
cdb_template_consult_column_labels <- function() {
  c(
    "ID",
    "Title",
    "Source",
    "Source Record",
    "Type",
    "Requested Start",
    "Requested End",
    "Realized Start",
    "Realized End",
    "Hours",
    "Complete"
  )
}

#' Consult Database New Personnel Data Frame
#' @return an "empty" personnel data.frame
cdb_template_personnel <- function() {
  data.frame(
    personalTitle = character(),
    givenName = character(),
    sn = character(),
    title = character(),
    department = character(),
    telephoneNumber = character(),
    mail = character(),
    icts_member = character(),
    role = character()
  )
}

#' Consult Database New Personnel Data Frame Column Labels
#' @return character vector representing column labels for \code{\link{cdb_template_personnel}}
cdb_template_personnel_column_labels <- function() {
  c(
    "Personal Title",
    "Given Name",
    "Surname",
    "Title",
    "Department",
    "Phone",
    "Mail",
    "ICTS Member",
    "Consult Role"
  )
}



# UTILITIES --------------------------------------------------------------------

#' Consult Database File
#'
#' @return path to the "consult_database.xlsx" file
#' @export
cdb_file <- function() {
  file.path(Sys.getenv("WU_CONSULT_DIR"), "consult_database.xlsx")
}

#' Consult Database Upsert
#'
#' @details inserts or updates the database sheet depending on whether or not the key value in x already exists in the sheet
#' @param x data frame or data frame extension representing rows to insert or update
#' @param sheetName a character string with the sheet name
cdb_sheet_upsert <- function(x, sheetName = c("consult", "personnel", "consult_personnel")) {
  sheetName <- match.arg(sheetName)

  by <- switch(sheetName,
               "consult" = "c_id",
               "personnel" = "mail",
               "consult_personnel" = c("c_id", "mail"))

  readxl::read_xlsx(cdb_file(), sheetName) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) -> x_0

  dplyr::as_tibble(x) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) -> x_1

  dplyr::rows_upsert(x_0, x_1, by) %>%
    xlsx_write.xlsx(cdb_file(), sheetName, forceAppend = TRUE)
}

#' Consult Database Get Principal Investigator
#'
#' @param k consult id
#' @return tibble read from \code{\link{cdb_file}} representing the principal investigator for the given consult
cdb_get_principal_investigator <- function(k) {
  readxl::read_xlsx(cdb_file(), "consult_personnel") %>%
    dplyr::filter(c_id == k, role == "Principal investigator") %>%
    dplyr::pull(mail) -> pi_mail

  readxl::read_xlsx(cdb_file(), "personnel") %>%
    dplyr::filter(mail == pi_mail)
}

#' Consult Database Check if Consult Exists
#'
#' @param k consult id
#' @return logical indicating whether the given consult already exists in \code{\link{cdb_file}}
cdb_consult_exists <- function(k) {
  readxl::read_xlsx(cdb_file(), "consult") %>%
    dplyr::filter(c_id == k) %>%
    nrow() == 1
}

#' Consult Database Current Consult
#'
#' @return the consult id derived from the open source editor file name
cdb_current_consult <- function() {
  source_file_abs <- rstudioapi::getSourceEditorContext()$path
  source_file <- basename(source_file_abs)
  source_file_sans_ext <- tools::file_path_sans_ext(source_file)
  if(valid_project_name(source_file_sans_ext, FALSE) || valid_project_name(source_file_sans_ext, TRUE))
    sub("-v\\d+$", "", source_file_sans_ext)
  else
    stop("Current source document does not appear to be a consult document.")
}



# SHINY EDIT APP ---------------------------------------------------------------

#' Consult Database Edit App
#'
#' @param k consult id
#' @param new logical; is this a new consult or should the consult data be read from \code{\link{cdb_file}}?
#'
#' @export
cdb_edit_app <- function(k = cdb_current_consult(), new = FALSE) {
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$script("
        Shiny.addCustomMessageHandler('close-window', function(x) {
          window.open('', '_self', '').close();
        });
    ")),
    shinyalert::useShinyalert(),
    shiny::titlePanel("Edit Consult Database"),
    shiny::navlistPanel(
      well = FALSE,
      widths = c(1, 11),
      shiny::tabPanel("Consult", shiny::uiOutput("dte_consult")),
      shiny::tabPanel("Personnel", shiny::uiOutput('dte_personnel'))
    ),
    shiny::actionButton("update_consult_database", "Submit")
  )

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    df_consult <- if(new) {
       cdb_template_consult(k)
    } else {
      readxl::read_xlsx(cdb_file(), "consult") %>%
        dplyr::filter(c_id == k) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
        as.data.frame() %>%
        dplyr::mutate(
          c_rqst_0 = as.Date(c_rqst_0),
          c_rqst_1 = as.Date(c_rqst_1),
          c_real_0 = as.Date(c_real_0),
          c_real_1 = as.Date(c_real_1)
        )
    }

    consult.insert.callback <- function(data, row) {
      # 'data' contains the dataframe *after* the row has been inserted/added
      # 'row' is the row number where data has been inserted
      df_consult <<- rbind(df_consult, data[row,])
      return(df_consult)
    }

    consult.update.callback <- function(data, olddata, row) {
      # 'data' contains the dataframe *after* the row has been updated
      # 'row' is the row number where data has been updated
      # 'olddata' is the previous version of the data
      df_consult[row,] <<- data[row,]
      return(df_consult)
    }

    consult.delete.callback <- function(data, row) {
      # 'data' contains the dataframe *before* the row has been deleted
      # 'row' is the row number where data is to be deleted
      df_consult <<- df_consult[-row,]
      return(df_consult)
    }

    dtedit(
      input, output,
      name = 'dte_consult',
      thedata = df_consult,
      edit.cols = names(df_consult)[-1],
      edit.label.cols = cdb_template_consult_column_labels()[-1],
      input.types = c(
        c_src = "selectInput",
        c_type = "selectInput",
        c_rqst_0 = "dateInput",
        c_rqst_1 = "dateInput",
        c_real_0 = "dateInput",
        c_real_1 = "dateInput",
        c_cmplt = "selectInput"
      ),
      input.choices = list(
        c_src = c("BERD", "Ortho", "Other"),
        c_type = c("Proposal",
                   "Undergrad",
                   "Graduate",
                   "Publication",
                   "Funded",
                   "Quality Improvement",
                   "Other"),
        c_cmplt = c("No", "Yes")
      ),
      show.delete = FALSE,
      show.insert = FALSE,
      show.copy = FALSE,
      callback.delete = consult.delete.callback,
      callback.update = consult.update.callback,
      callback.insert = consult.insert.callback,
      datatable.call = function(...) {DT::datatable(..., colnames = cdb_template_consult_column_labels())}
    )

    df_personnel <- if(new) {
      cdb_template_personnel()
    } else {
      readxl::read_xlsx(cdb_file(), "personnel") %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) -> df_personnel

      readxl::read_xlsx(cdb_file(), "consult_personnel") %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
        dplyr::filter(c_id == k) %>%
        dplyr::select(-c_id) -> df_consult_personnel

      dplyr::right_join(df_personnel, df_consult_personnel, "mail") %>%
        dplyr::select(dplyr::everything(), role) %>%
        as.data.frame()
    }

    personnel.insert.callback <- function(data, row) {
      data[row, "mail"] <- tolower(data[row, "mail"])
      # 'data' contains the dataframe *after* the row has been inserted/added
      # 'row' is the row number where data has been inserted
      df_personnel <<- rbind(df_personnel, data[row,])
      # in this case, 'mydata' should just be the same as 'data'
      return(df_personnel)
    }

    personnel.update.callback <- function(data, olddata, row) {
      data[row, "mail"] <- tolower(data[row, "mail"])
      # 'data' contains the dataframe *after* the row has been updated
      # 'row' is the row number where data has been updated
      # 'olddata' is the previous version of the data
      df_personnel[row,] <<- data[row,]
      # in this case, 'mydata' should just be the same as 'data'
      return(df_personnel)
    }

    personnel.delete.callback <- function(data, row) {
      # 'data' contains the dataframe *before* the row has been deleted
      # 'row' is the row number where data is to be deleted
      df_personnel <<- df_personnel[-row,]
      # in this case, 'mydata' should just be the same as data[-c(row),]
      return(df_personnel)
    }

    dtedit(
      input, output,
      name = 'dte_personnel',
      thedata = df_personnel,
      edit.label.cols = cdb_template_personnel_column_labels(),
      input.types = c(
        icts_member = "selectInput",
        role = "selectInput"
      ),
      input.choices = list(
        icts_member = c("No", "Yes"),
        role = c("Principal investigator",
                 "Co-investigator",
                 "Graduate",
                 "Undergraduate",
                 "Coordinator",
                 "Consultant")
      ),
      show.search = TRUE,
      callback.delete = personnel.delete.callback,
      callback.update = personnel.update.callback,
      callback.insert = personnel.insert.callback,
      datatable.call = function(...) {DT::datatable(..., colnames = cdb_template_personnel_column_labels())}
    )

    shiny::observeEvent(input$update_consult_database, {
      df_personnel %>%
        dplyr::filter(role == "Principal investigator") %>%
        nrow() == 1 -> has_pi

      if(!has_pi) {
        shinyalert::shinyalert("Principal Investigator Required", "Please list one principal investigator.", "warning")
      } else if(any(df_personnel$mail == "")) {
        shinyalert::shinyalert("Personnel mail Required", "Please ensure all personnel have a value for mail.", "warning")
      } else {
        # 1/3 (consult)
        cdb_sheet_upsert(df_consult, "consult")
        # 2/3 (personnel)
        dplyr::select(df_personnel, -role) %>%
          cdb_sheet_upsert("personnel")
        # 3/3 (consult_personnel)
        # - cannot just upsert because may wish to remove personnel from project
        df_consult_personnel <- dplyr::bind_cols(
          dplyr::select(df_consult, c_id),
          dplyr::select(df_personnel, mail, role)
        )
        readxl::read_xlsx(cdb_file(), "consult_personnel") %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
          dplyr::filter(c_id != df_consult[[1, "c_id"]]) %>%
          dplyr::bind_rows(df_consult_personnel) %>%
          dplyr::arrange(c_id, mail) %>%
          xlsx_write.xlsx(cdb_file(), "consult_personnel", forceAppend = TRUE)

        # Step 4
        session$sendCustomMessage("close-window", "")
        #shiny::stopApp()
      }
    })
  }

  shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}



# SHINY SEARCH APP --------------------------------------------------------------

#' Consult Database Search App
#'
#' @export
cdb_search_app <- function() {
  # internal function to make onclick method for consult id links
  mk_c_id_link_onclick <- function(k) {
    sprintf('Shiny.setInputValue("c_id_link", "%s", {priority: "event"});',
            file.path(gsub("\\\\", "/", Sys.getenv("WU_CONSULT_DIR")), k)
    )
  }

  # internal function to make links that open local consult directories
  mk_c_id_link <- function(k) {
    htmltools::doRenderTags(
      shiny::tags$a(k, href = "#", onclick = mk_c_id_link_onclick(k))
    )
  }

  # internal function to make email links
  mk_mailto_link <- function(mail, subject = "", body = "") {
    subject <- utils::URLencode(htmltools::htmlEscape(subject))
    body <-  utils::URLencode(htmltools::htmlEscape(body))
    href <- sprintf("mailto:%s?subject=%s&body=%s", mail, subject, body)
    htmltools::doRenderTags(shiny::tags$a(mail, href = href))
  }

  ui <- miniUI::miniPage(
    shiny::titlePanel("Search Consult Database"),
    miniUI::miniContentPanel(
      DT::dataTableOutput("consult_database"),
      padding = 0))

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    shiny::observeEvent(input$c_id_link, {
      dir.open(input$c_id_link)
    })

    output$consult_database <- DT::renderDataTable({
      sheet_names <- c("consult_personnel", "consult", "personnel")
      names(sheet_names) <- sheet_names
      purrr::map(sheet_names, ~ {
        readxl::read_xlsx(cdb_file(), .) %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      }) -> tbls

      tbls$consult_personnel %>%
        dplyr::left_join(tbls$consult, "c_id") %>%
        dplyr::left_join(tbls$personnel, "mail") %>%
        dplyr::mutate(
          c_id = purrr::map_chr(.data$c_id, mk_c_id_link),
          mail = purrr::pmap_chr(.,
            function(mail, c_src, c_src_ref, c_title, personalTitle, sn, ...) {
              mk_mailto_link(
                mail = mail,
                subject = sprintf("%s #%s - %s", c_src, c_src_ref, c_title),
                body = sprintf("Hi %s %s,\n\n\n", personalTitle, sn)
              )
          })
        ) %>%
        dplyr::select(dplyr::starts_with("c_"), dplyr::everything())
    },
    colnames = c(
      cdb_template_consult_column_labels(),
      cdb_template_personnel_column_labels()[c(7, 9, 1:6, 8)]
    ),
    escape = FALSE)
  }

  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
