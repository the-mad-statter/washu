#' Zotero GET
#'
#' @param api_key Zotero API Key
#' @inheritParams httr::GET
#'
#' @return \link[httr]{response} object
zt_get <- function(url = "https://api.zotero.org",
                   api_key = Sys.getenv("ZOTERO_API_KEY"),
                   ...) {
  httr::GET(
    url = url,
    httr::add_headers("Zotero-API-Key" = api_key),
    ...
  )
}

#' Zotero GET /users/<api_user>
#'
#' @param path optional additional path
#' @param api_user Zotero API user
#' @inheritParams zt_get
#'
#' @return \link[httr]{response} object
zt_get_users <- function(path = "",
                         api_user = Sys.getenv("ZOTERO_API_USER"),
                         ...) {
  zt_get(path = sprintf("users/%s/%s", api_user, path), ...)
}

#' Zotero GET /users/<api_user>/collections
#'
#' @inheritParams zt_get_users
#'
#' @return \link[httr]{response} object
zt_get_users_collections <- function(path = "", ...) {
  zt_get_users(path = sprintf("collections/%s", path), ...)
}

#' Zotero GET /users/<api_user>/collections/<collection_key>/items
#'
#' @param collection_key Zotero collection key
#' @inheritParams zt_get_users_collections
#'
#' @return \link[httr]{response} object
zt_get_users_collections_items <- function(collection_key, path = "", ...) {
  zt_get_users_collections(
    path = sprintf("%s/items/%s", collection_key, path),
    ...
  )
}

#' Zotero GET /users/<api_user>/collections/<collection_key>/items/top
#'
#' @inheritParams zt_get_users_collections_items
#'
#' @return \link[httr]{response} object
zt_get_users_collections_items_top <- function(collection_key, ...) {
  zt_get_users_collections_items(
    collection_key = collection_key,
    path = "top",
    ...
  )
}

#' Zotero POST
#'
#' @param api_key Zotero API key
#' @inheritParams httr::POST
#'
#' @return \link[httr]{response} object
zt_post <- function(url = "https://api.zotero.org",
                    api_key = Sys.getenv("ZOTERO_API_KEY"),
                    ...) {
  httr::POST(
    url = url,
    httr::add_headers("Zotero-API-Key" = api_key),
    ...
  )
}

#' Zotero POST /users/<api_user>
#'
#' @param path optional path
#' @param api_user Zotero API user
#' @inheritParams zt_post
#'
#' @return \link[httr]{response} object
zt_post_users <- function(path = "",
                          api_user = Sys.getenv("ZOTERO_API_USER"),
                          ...) {
  zt_post(path = sprintf("users/%s/%s", api_user, path), ...)
}

#' Zotero POST /users/<api_user>/items
#'
#' @inheritParams zt_post_users
#'
#' @return \link[httr]{response} object
zt_post_users_items <- function(path = "", ...) {
  zt_post_users(path = sprintf("items/%s", path), ...)
}

#' Zotero POST /users/<api_user>/collections
#'
#' @inheritParams zt_post_users
#'
#' @return \link[httr]{response} object
zt_post_users_collections <- function(path = "", ...) {
  zt_post_users(path = sprintf("collections/%s", path), ...)
}

#' Zotero lookup name-key pairs for a user's collections
#'
#' @inheritParams zt_get_users_collections
#'
#' @return named character vector
zt_lookup_user_collections_name_key_pairs <- function(...) {
  collections <- httr::content(zt_get_users_collections(...))
  names <- vapply(collections, function(x) x[["data"]][["name"]], character(1))
  keys <- vapply(collections, function(x) x[["key"]], character(1))
  names(keys) <- names
  keys
}

#' Zotero lookup collection key by collection name
#'
#' @param collection_name name of the collection
#' @inheritParams zt_lookup_user_collections_name_key_pairs
#'
#' @return collection key
zt_lookup_user_collection_key <- function(collection_name, ...) {
  unname(zt_lookup_user_collections_name_key_pairs(...)[collection_name])
}

#' Zotero Add Package
#'
#' @param package a character string with the name of a single package
#' @param collection_name name of collection to add citation to
#' @inheritParams zt_post_users_items
zt_add_pkg <- function(package,
                       collection_name,
                       ...) {
  citation <- NULL

  r_pkg_citation <- citation(package)

  zt_get(path = "items/new", query = list(itemType = "book"), ...) %>%
    httr::content() -> zt_template

  r_pkg_citation_author_given <- r_pkg_citation$author$given
  r_pkg_citation_author_family <- r_pkg_citation$author$family
  if (is.null(r_pkg_citation_author_family)) {
    r_pkg_citation_author_family <- r_pkg_citation_author_given
    r_pkg_citation_author_given <- ""
  }

  zt_template$title <- r_pkg_citation$title
  zt_template$creators <- list(
    list(
      "creatorType" = "author",
      "firstName" = r_pkg_citation_author_given,
      "lastName" = r_pkg_citation_author_family
    )
  )
  zt_template$publisher <- match_arg(r_pkg_citation$organization, "")
  zt_template$place <- match_arg(r_pkg_citation$address, "")
  zt_template$date <- r_pkg_citation$year
  zt_template$url <- match_arg(r_pkg_citation$url, "")
  zt_template$collections <-
    list(zt_lookup_user_collection_key(collection_name = collection_name, ...))

  zt_post_users_items(
    body = rjson::toJSON(list(zt_template)),
    encode = "json",
    ...
  )
}

#' Zotero create collection
#'
#' @param collection_name name of the collection
#' @param parent_collection_key collection key of the parent collections
#' @inheritParams zt_post_users_collections
zt_create_collection <- function(collection_name,
                                 parent_collection_key = NULL,
                                 ...) {
  json <- rjson::toJSON(
    list(
      list(
        name = collection_name,
        parentCollection = parent_collection_key
      )
    )
  )

  zt_post_users_collections(body = json, encode = "json", ...)
}
