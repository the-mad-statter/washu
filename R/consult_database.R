db_write <- function(x, sheetName, db) {
  tbl_user <- xlsx::read.xlsx(db, sheetName = "user")
  tbl_consult <- xlsx::read.xlsx(db, sheetName = "consult")
  tbl_user_consult <- xlsx::read.xlsx(db, sheetName = "user_consult")
  switch(sheetName,
         "user" =         x -> tbl_user,
         "consult" =      x -> tbl_consult,
         "user_consult" = x -> tbl_user_consult)
  xlsx::write.xlsx(tbl_user,         db, "user",         row.names = FALSE,                showNA = FALSE)
  xlsx::write.xlsx(tbl_consult,      db, "consult",      row.names = FALSE, append = TRUE, showNA = FALSE)
  xlsx::write.xlsx(tbl_user_consult, db, "user_consult", row.names = FALSE, append = TRUE, showNA = FALSE)
}

# returns numeric(0) if not found
db_get_user_id <- function(email, db = Sys.getenv("WU_CONSULT_DB")) {
  tbl_user <- xlsx::read.xlsx(db, sheetName = "user")
  row <- which(tbl_user$mail == email)
  tbl_user$user_id[row]
}

db_max_user_id <- function(db = Sys.getenv("WU_CONSULT_DB")) {
  tbl_user <- xlsx::read.xlsx(db, sheetName = "user")
  row <- which.max(tbl_user$user_id)
  tbl_user$user_id[row]
}

db_add_user <- function(email, db = Sys.getenv("WU_CONSULT_DB")) {
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
         "app_role")
  for(m in setdiff(v, names(q))) {
    q[m] <- switch(m,
                   "user_id" = db_max_user_id(db) + 1,
                   "app_role" = "user",
                   NA)
  }
  q <- q[v]

  tbl_user <- rbind(xlsx::read.xlsx(db, sheetName = "user"), q)
  db_write(tbl_user, "user", db = db)
}

db_add_consult <- function(consult_id,
                           due = "",
                           referal = "",
                           berd_record_id = "",
                           berd_study_type = "",
                           consult_title = "",
                           cloud_share = "",
                           award_pre_work = "",
                           award_post_work = "",
                           publication_work = "",
                           award_funding_agency = "",
                           award_funding_vehicle = "",
                           publication_outlet = "",
                           award_funded = "",
                           award_direct = "",
                           award_indirect = "",
                           publication_published = "",
                           note = "",
                           db = Sys.getenv("WU_CONSULT_DB")) {
  tbl_consult <- rbind(
    xlsx::read.xlsx(db, sheetName = "consult"),
    data.frame(consult_id = consult_id,
               due = due,
               referal = referal,
               berd_record_id = berd_record_id,
               berd_study_type = berd_study_type,
               consult_title = consult_title,
               cloud_share = cloud_share,
               award_pre_work = award_pre_work,
               award_post_work = award_post_work,
               publication_work = publication_work,
               award_funding_agency = award_funding_agency,
               award_funding_vehicle = award_funding_vehicle,
               publication_outlet = publication_outlet,
               award_funded = award_funded,
               award_direct = award_direct,
               award_indirect = award_indirect,
               publication_published = publication_published,
               note = note
    )
  )
  db_write(tbl_consult, "consult", db = db)
}

db_add_user_consult <- function(consult_id, user_id, role, db = Sys.getenv("WU_CONSULT_DB")) {
  tbl_user_consult <-
    rbind(xlsx::read.xlsx(db, sheetName = "user_consult"),
          data.frame(consult_id = consult_id,
                     user_id = user_id,
                     role = role))
  db_write(tbl_user_consult, "user_consult", db = db)
}

db_get_user_consults <- function(email, db = Sys.getenv("WU_CONSULT_DB")) {
  readxl::read_xlsx(db, "user_consult") %>%
    dplyr::filter(.data$user_id == db_get_user_id(email, db)) %>%
    dplyr::pull(.data$consult_id) -> consult_ids

  readxl::read_xlsx(db, "consult") %>%
    dplyr::filter(.data$consult_id %in% consult_ids)
}

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