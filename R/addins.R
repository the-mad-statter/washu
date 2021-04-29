addin_push_slashes <- function() {
  ctx_src <- rstudioapi::getSourceEditorContext()
  sel_rng <- ctx_src[[4]][[1]][[1]]
  str_org <- ctx_src[[4]][[1]][[2]]
  str_rpl <- push_slashes(str_org)
  rstudioapi::modifyRange(sel_rng, str_rpl)
}

addin_open_win_charmap <- function() {
  open_win_charmap()
}

addin_search_consults <- function() {
  db_search_consults()
}

addin_insert_sas_code_chunk <- function() {
  text <- paste(
    c('```{r switch_to_sas_engine}',
      'library(SASmarkdown)',
      '',
      'saspath <- "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe"',
      'sasopts <- "-nosplash -ls 75"',
      'knitr::opts_chunk$set(',
      '  engine = "sas",',
      '  engine.path = saspath,',
      '  engine.opts = sasopts,',
      '  comment = ""',
      ')',
      '```',
      '',
      '```{sas}',
      '',
      '```',
      '',
      '```{r switch_to_r_engine, engine = "R"}',
      'knitr::opts_chunk$set(',
      '  engine = "R", ',
      '  engine.path = NULL, ',
      '  engine.opts = NULL, ',
      '  comment = NA',
      ')',
      '```'
    ), 
    collapse = "\n"
  )
  
  id <- rstudioapi::getSourceEditorContext()$id
  
  rstudioapi::insertText(text = text, id = id)
}
