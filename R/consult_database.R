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
