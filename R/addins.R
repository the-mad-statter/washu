addin_push_slashes <- function() {
  ctx_src <- rstudioapi::getSourceEditorContext()
  sel_rng <- ctx_src[[4]][[1]][[1]]
  str_org <- ctx_src[[4]][[1]][[2]]
  str_rpl <- push_slashes(str_org)
  rstudioapi::modifyRange(sel_rng, str_rpl)
}

addin_search_consults <- function() {
  db_search_consults()
}
