#' Add R package to bibliography
#' @param bibliography path to bibliography file
#' @param ... additional parameters passed to \code{\link[utils]{citation}}
#' @inheritParams utils::citation
#' @export
bib_add_pkg_local <- function(package = "base", bibliography = bib_bibliography(), ...) {
  old <- readLines(bibliography)
  add <- utils::capture.output(utils::citation(package, ...))
  add <- add[which(grepl("@Manual", add)):which(grepl("}$", add))]
  add[1] <- sub(",", paste0("r_pkg_", package, ","), add[1])
  add[length(add)-1] <- sub("},", "}", add[length(add)-1])
  add <- substring(add, 3)
  writeLines(c(old, "", add), bibliography)
}

#' Use citation style language
#' @param csl citation style language
#' @param bib_path path to bibliography directory
#' @export
#' @references \href{https://github.com/citation-style-language/styles}{Citation Style Language Styles}
bib_use_csl <- function(csl = c("american-medical-association", "apa"), bib_path = bib_bibliography(dirname = TRUE)) {
  csl <- match.arg(csl)

  file.remove(file.path(bib_path, list.files(bib_path, "csl$")))
  file.copy(find_resource("template_resource", "consult_report", sprintf("%s.csl", csl)),
            file.path(bib_path, sprintf("%s.csl", csl)))

  source_editor_contents <- rstudioapi::getSourceEditorContext()$contents
  csl_row <- which(grepl("^csl: .+\\.csl$", source_editor_contents))
  csl_start <- rstudioapi::document_position(csl_row, 1)
  csl_end <- rstudioapi::document_position(
    csl_row,
    nchar(source_editor_contents[csl_row]) + 1
  )
  rstudioapi::modifyRange(
    rstudioapi::document_range(csl_start, csl_end),
    sprintf("csl: %s.csl",
            ifelse(nchar(bib_path) > 0, file.path(bib_path, csl), csl)
    )
  )
}

#' Determine Zotero collection name from open RMarkdown file name
#' @return name of Zotero collection
#' @export
bib_zotero_collection <- function() {
  sub("-v.+", "", basename(rstudioapi::getSourceEditorContext()$path))
}

#' Determine bibliography from RMarkdown YAML
#'
#' @param dirname flag to return the bibliography directory instead of the full relative path
#'
#' @return full bibliography path specified in the RMarkdown YAML or parent directory
#' @export
bib_bibliography <- function(dirname = FALSE) {
  source_editor_contents <- rstudioapi::getSourceEditorContext()$contents
  bib_row <- which(grepl("^bibliography: .+$", source_editor_contents))
  bibliography <- sub("bibliography: ", "", source_editor_contents[bib_row])
  if(dirname)
    dirname(bibliography)
  else
    bibliography
}

#' Download Zotero collection
#' @param download_pdfs flag to download pdfs (if any)
#' @param collection_name name of the collection to download from
#' @param bibliography path to the bibliography file
#' @param ... additional parameters passed to httr-based functions
#' @export
bib_sync_zotero <- function(download_pdfs = TRUE,
                            collection_name = bib_zotero_collection(),
                            bibliography = bib_bibliography(),
                            ...) {
  title <- href <- file_path <- NULL

  collection_key <- zt_lookup_user_collection_key(
    collection_name = collection_name,
    ...)
  zt_get_users_collections_items_top(collection_key = collection_key,
                                     query = list(format = "bibtex"),
                                     ...) %>%
    httr::content() %>%
    rawToChar() %>%
    c("") %>%
    writeLines(bibliography)

  zt_get_users_collections_items(collection_key = collection_key, ...) %>%
    httr::content() %>%
    lapply(function(x) x[["links"]][["enclosure"]]) %>%
    lapply(function(x) unlist(x)) %>%
    dplyr::bind_rows() -> tbl_pdf

  if(download_pdfs & nrow(tbl_pdf) > 0) {
    tbl_pdf %>%
      dplyr::mutate(file_path = file.path(dirname(bibliography), title)) %>%
      dplyr::select(href, file_path) %>%
      purrr::pmap(function(href, file_path) {
        zt_get(url = href,
               config = httr::write_disk(path = file_path, overwrite = TRUE))
      })
  }
}

#' @rdname zt_add_pkg
#' @export
bib_add_pkg_zotero <- function(package,
                               collection_name = bib_zotero_collection(),
                               ...) {
  zt_add_pkg(package = package,
             collection_name = collection_name,
             ...)
}

#' @rdname zt_create_collection
#' @export
bib_use_zotero <- function(collection_name = bib_zotero_collection(),
                           parent_collection_key = NULL,
                           ...) {
  zt_create_collection(collection_name = collection_name,
                       parent_collection_key = parent_collection_key,
                       ...)
}
