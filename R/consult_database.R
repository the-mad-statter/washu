################################################################################
# USER TABLE FUNCTIONS
################################################################################

#' Populate a tibble with new user information from LDAP
#'
#' @param email mail of the user
#' @param user_id primary key of user
#' @param icts integer indicating icts membership status
#' @param app_role role of the user
#'
#' @return a tibble with populated user information
db_new_user <- function(email, user_id, icts, app_role = c("user", "admin")) {
  app_role <- match.arg(app_role)
  
  q <- washu::wu_ldap_query("mail", email)
  v <- c("user_id",
         "cn",
         "sn",
         "title",
         "physicalDeliveryOfficeName",
         "telephoneNumber",
         "givenName",
         "displayName",
         "department",
         "streetAddress",
         "personalTitle",
         "name",
         "sAMAccountName",
         "userPrincipalName",
         "mail",
         "eduPersonNickname",
         "eduPersonPrimaryAffiliation",
         "app_role",
         "icts")
  for(m in setdiff(v, names(q))) {
    q[m] <- NA
  }
  q <- q[v]
  q$user_id <- user_id
  q$app_role <- app_role
  q$icts <- icts
  q
}

#' Construct SQL insert syntax for a new user
#'
#' @inheritParams db_new_user
#'
#' @return SQL syntax to insert a new user
db_sql_insert_user_single <- function(email, user_id, icts, app_role = "user") {
  q <- db_new_user(email, user_id, icts, app_role)
  sprintf("INSERT INTO wustl_consults.user VALUES ('%s');", paste(q, collapse = "', '"))
}

#' Construct SQL insert syntax for new users
#'
#' @inheritParams db_new_user
#'
#' @return SQL syntax to insert new users
db_sql_insert_user <- function(email, user_id, icts, app_role) {
  d <- dplyr::tibble(email, user_id, icts, app_role)
  purrr::pmap_chr(d, db_sql_insert_user_single)
}

#' Copy SQL insert syntax for new users to the clipboard
#'
#' @inheritParams db_new_user
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' db_sql_insert_user_clip(email = c("poo@wustl.edu", "piglet@wustl.edu"),
#'                         user_id = 1:2,
#'                         icts = c(1, 0),
#'                         app_role = c("admin", "user"))
#' }
db_sql_insert_user_clip <- function(email, user_id, icts, app_role) {
  sql <- db_sql_insert_user(email, user_id, icts, app_role)
  clipr::write_clip(sql)
}

#' Copy data in SQL insert syntax for new users to the clipboard
#' 
#' @description Look up a user in ldap by email and copy data to clipboard to
#' paste into the consult database.
#' 
#' @inheritParams db_new_user
#' 
#' @export
db_clip_user <- function(email) {
  .Deprecated("db_sql_insert_user_clip")
  u <- db_new_user(email, 1, 1)
  clipr::write_clip(u)
}



################################################################################
# USER_CONSULT TABLE FUNCTIONS
################################################################################

#' Construct SQL insert syntax for a new user-consult pairing
#'
#' @param user_consult_id primary key for table
#' @param consult_id consult id including version number
#' @param user_id foreign key for user from user table
#' @param role role of the user on the consult
#'
#' @return SQL syntax to insert a new user-consult pairing
db_sql_insert_user_consult_single <- function(user_consult_id, consult_id, user_id, role = c("principal", "statistician", "student", "resident", "coinvestigator")) {
  role <- match.arg(role)
  d <- c(user_consult_id, consult_id, user_id, role)
  sprintf("INSERT INTO wustl_consults.user_consult VALUES ('%s');", paste(d, collapse = "', '"))
}

#' Construct SQL insert syntax for new user-consult pairings
#'
#' @inheritParams db_sql_insert_user_consult_single
#'
#' @return SQL syntax to insert new users
db_sql_insert_user_consult <- function(user_consult_id, consult_id, user_id, role) {
  purrr::pmap_chr(dplyr::tibble(user_consult_id, consult_id, user_id, role), db_sql_insert_user_consult_single)
}

#' Copy SQL insert syntax for new user-consult pairings to the clipboard
#'
#' @inheritParams db_sql_insert_user_consult_single
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' db_sql_insert_user_consult_clip(user_consult_id = 57:58, 
#'                                 consult_id = "2020-11-01-1-v1", 
#'                                 user_id = c(1, 14), 
#'                                 role = c("statistician", "principal"))
#' }
db_sql_insert_user_consult_clip <- function(user_consult_id, consult_id, user_id, role) {
  sql <- db_sql_insert_user_consult(user_consult_id, consult_id, user_id, role)
  clipr::write_clip(sql)
}



################################################################################
# SHINY APP
################################################################################

#' Run shiny app to search consult database
#'
#' @param db location of the database
#'
#' @export
db_search_consults <- function(db = Sys.getenv("WU_CONSULT_DB")) {
  # internal function to make onclick method for consult id links
  mk_consult_id_onclick <- function(consult_id) {
    sprintf('Shiny.setInputValue("consult_id_link", "%s", {priority: "event"});',
            file.path(dirname(db), sub("-v[0-9]+$", "", consult_id))
    )
  }

  # internal function to make links that open local consult directories
  mk_consult_id_link <- function(consult_id) {
    htmltools::doRenderTags(
      shiny::tags$a(
        consult_id,
        href="#",
        onclick = mk_consult_id_onclick(consult_id)
      )
    )
  }

  # internal function to make cloud share links
  mk_cloud_share_link <- function(cloud_share) {
    ifelse(nchar(cloud_share) == 0, "",
           htmltools::doRenderTags(
             shiny::tags$a(cloud_share, href = cloud_share, target = "_blank"))
    )
  }

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      DT::dataTableOutput("consult_database"),
      padding = 0))

  server <- function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
    shiny::observeEvent(input$consult_id_link, {
      dir.open(input$consult_id_link)
    })

    output$consult_database <- DT::renderDataTable({
      readxl::read_xlsx(db, "user_consult") %>%
        dplyr::left_join(readxl::read_xlsx(db, "user"), "user_id") %>%
        dplyr::left_join(readxl::read_xlsx(db, "consult"), "consult_id") %>%
        dplyr::mutate(
          consult_id = purrr::map_chr(.data$consult_id, mk_consult_id_link),
          cloud_share = purrr::map_chr(.data$cloud_share, mk_cloud_share_link)
        )
    }, escape = FALSE)
  }

  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
