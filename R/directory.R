#' Extract directory field
#'
#' Extract a field from a WashU directory search
#'
#' @param v character vector intermixing keys and values for a single result
#' from a directory search
#' @param k key to search for
#'
#' @return desired value or NA
extract_directory_field <- function(v, k) {
  i <- which(v == k)
  ifelse(length(i) == 0, NA, v[i + 1])
}

#' Parse Directory Entry
#'
#' Parse an entry from a WashU directory search
#'
#' @param e html node list representing a single result from a directory search
#'
#' @return data frame representing a single result from a directory search
parse_directory_entry <- function(e) {
  v <- e %>%
    rvest::html_nodes("span") %>%
    rvest::html_text()

  tibble::tibble(
    name       = extract_directory_field(v, "Name:"),
    address    = extract_directory_field(v, "Address:"),
    phone      = extract_directory_field(v, "Phone:"),
    fax        = extract_directory_field(v, "Fax:"),
    title      = extract_directory_field(v, "Title:"),
    department = extract_directory_field(v, "Department:"),
    email      = extract_directory_field(v, "Email:")
  )
}

#' WU directory search
#'
#' Search the WashU directory
#'
#' @param search_name name query
#' @param email email query
#' @param phone phone query
#'
#' @return data frame of results
#'
#' @note Will return an empty data frame if there are none or too many results
#'
#' @seealso \url{https://wustl.edu/directory/}
#'
#' @export
#'
#' @examples
#' wu_directory("schuelke")
wu_directory <- function(search_name = "", email = "", phone = "") {
  httr::POST(
    "https://wustl.edu/wp-content/themes/wustl_edu/directoryWrapper.php",
    body = list(
      search_name = search_name,
      email = email,
      phone = phone
    )
  ) %>%
    xml2::read_html() %>%
    rvest::html_nodes(".entry") %>%
    lapply(parse_directory_entry) %>%
    dplyr::bind_rows()
}

#' ICTS membership directory search
#'
#' @param query search query
#'
#' @return tibble of results
#' @export
icts_directory <- function(query) {
  httr::GET("https://icts.wustl.edu/wp-json/wp/v2/people",
    query = list(
      per_page = 50,
      order = "asc",
      orderby = "washu_ppi_last",
      page = 1,
      search = query,
      `_fields` =
        "id,title,meta,featured_image,type,featured_media,people_type,link",
      washu_people_type = 9,
      `_locale` = "user"
    )
  ) %>%
    httr::content() %>%
    purrr::map_dfr(~ dplyr::bind_cols(
      dplyr::as_tibble(.x[-which(names(.x) %in% c(
        "title", "meta",
        "people_type"
      ))]),
      dplyr::tibble(title = .x$title$rendered),
      dplyr::as_tibble(.x$meta[-which(names(.x$meta) == "washu_ppi_map")]),
      dplyr::tibble(people_type = paste(.x$people_type, collapse = ",")),
      dplyr::tibble(washu_ppi_map = paste(.x$meta$washu_ppi_map,
        collapse = ","
      ))
    ))
}
