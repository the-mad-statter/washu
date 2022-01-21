#' Find resource
#' @param type type of resource to find
#' @param fork specify which subdirectory to search in the overall type path
#' @param file file to locate
#' @return absolute path to the resource
find_resource <- function(type = c("template_resource",
                                   "template_skeleton",
                                   "global_resource",
                                   "global_font_path"),
                          fork,
                          file) {
  type <- match.arg(type)

  relative_path <- switch(type,
    "template_resource" =
      file.path("rmarkdown", "templates", fork, "resources", file),
    "template_skeleton" =
      file.path("rmarkdown", "templates", fork, "skeleton", "skeleton.Rmd"),
    "global_resource" =
      file.path("resources", fork, file),
    "global_font_path" =
      file.path("resources", "fonts"))

  absolute_path <- system.file(relative_path, package = "washu")

  if (absolute_path == "")
    stop("Couldn't find resource ", relative_path, call. = FALSE)

  absolute_path
}

#' TeX setmainfont
#' @param font desired font
#' @param ext font extention
#' @return character vector of TeX code
tex_setmainfont <- function(font = "LibreBaskerville", ext = "ttf") {
  c("\\usepackage{fontspec}",
    sprintf("\\setmainfont{%s}[",
            font),
    sprintf("  Path = %s%s ,",
            find_resource("global_font_path"),
            .Platform$file.sep),
    sprintf("  Extension = .%s,",
            ext),
    "  UprightFont = *-Regular,",
    "  ItalicFont = *-Italic,",
    "  BoldFont = *-Bold]")
}

#' TeX new washulogo command
#' @return character vector of TeX code
tex_washusomlogo <- function() {
  sprintf("\\newcommand{\\washusomlogo}{%s}",
          find_resource("global_resource", "img", "washu_som_logo.eps"))
}

#' TeX new pigletsignature command
#' @return character vector of TeX code
tex_pigletsignature <- function() {
  sprintf("\\newcommand{\\pigletsignature}{%s}",
          find_resource("template_resource", "letter", "piglet.png"))
}

#' TeX runtime header additions
#' @param x character vector of TeX code
#' @return path to temporary file containing the TeX code
tex_runtime_in_header <- function(x) {
  in_header_path <- file.path(tempdir(), "in_header.tex")
  in_header_con <- file(in_header_path)
  writeLines(x, in_header_con)
  close(in_header_con)
  in_header_path
}

#' Letter document
#' Format for creating a WashU letter on School of Medicine letterhead
#' @param template Pandoc template to use for rendering. Passed value ignored in favor of default.
#' @param latex_engine LaTeX engine for producing PDF output. Passed value ignored in favor of default.
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{\link[rmarkdown]{pdf_document}}
#' @export
letter_document <- function(template = find_resource("template_resource",
                                                     "letter",
                                                     "template.tex"),
                            latex_engine = "xelatex",
                            includes,
                            ...) {
  runtime_in_header <- tex_runtime_in_header(c(tex_setmainfont(),
                                               tex_washusomlogo(),
                                               tex_pigletsignature()))

  if(missing(includes))
    includes <- rmarkdown::includes(runtime_in_header)
  else
    includes$in_header <- append(includes$in_header, runtime_in_header)

  rmarkdown::pdf_document(template = find_resource("template_resource",
                                                   "letter",
                                                   "template.tex"),
                          latex_engine = "xelatex",
                          includes = includes,
                          ...)
}

#' TeX Close Letter
#' @return character vector of TeX code
#' @export
tex_close_letter <- function() {
  input <- knitr_current_input()
  yaml <- rmarkdown::yaml_front_matter(input)

  writeLines(
    c(
      yaml$closing,
      "",
      "\\vspace{1\\baselineskip}",
      "",
      sprintf("\\includegraphics[height=1\\baselineskip]{%s}", gsub("\\\\", "/", yaml$signature)),
      "",
      "\\vspace{1\\baselineskip}",
      "",
      sprintf("%s \\ ", yaml$from$name),
      "",
      yaml$from$title
    )
  )
}

#' @inherit knitr::current_input
knitr_current_input <- function(dir = FALSE) {
  if(interactive()) {
    context <- rstudioapi::getSourceEditorContext()
    path <- context$path
    if(!is.null(path))
      ifelse(dir, path, basename(path))
    else
      NULL
  } else {
    knitr::current_input(dir)
  }
}

#' Letter of support body
#' @param title title of the project to be supported
#' @param template path to letter of support body template provided by user
#' @param pattern pattern in the file to replace with title
#' @export
letter_of_support_body <-
  function(title,
           template = Sys.getenv("WU_LETTER_OF_SUPPORT_BODY"),
           pattern = "\\$title\\$") {
    file(template) -> infile

    infile %>%
      readLines() %>%
      paste(collapse = "\n") %>%
      tidy_sub(pattern, title, fixed = FALSE) -> body

    close(infile)

    body
  }

#' Render letter of support
#' @param from_name,from_title,from_department_type,from_department_name,from_department_url,from_campus_box,from_phone,from_email attributes of sender
#' @param to_name,to_address attributes of recipient
#' @param date,salutation,closing letter customization
#' @param body body of letter
#' @param signature path to signature file (empty string for no signature)
#' @inheritParams rmarkdown::render
#' @param keep_input keep the input document
#' @param ... parameters passed to \code{\link[rmarkdown]{render}}
#' @export
#' @examples
#' \dontrun{
#' ## some yaml may require quotes, e.g.:
#' ## - salutation because of the colon
#' ## - signature if spaces in path
#' wu_render_letter_of_support(
#'   "Matthew J. Schuelke, PhD",
#'   "Research Statistician",
#'   "Division",
#'   "Biostatistics",
#'   "https://biostatistics.wustl.edu",
#'   "8067",
#'   "+1 314 362-0000",
#'   "schuelke@wustl.edu",
#'   "Christopher Robin",
#'   c("11 Mallord Street", "LONDON", "SW3 6DT", "UK"),
#'   Sys.Date(),
#'   "\"To Whom It May Concern:\"",
#'   "Sincerely,",
#'   letter_of_support_body("The Woozle Effect"),
#'   paste0("\"", Sys.getenv("WU_SIGNATURE"), "\"")
#' )
#' }
wu_render_letter_of_support <-
  function(from_name,
           from_title,
           from_department_type,
           from_department_name,
           from_department_url,
           from_campus_box,
           from_phone,
           from_email,
           to_name,
           to_address,
           date,
           salutation,
           closing,
           body,
           signature = "",
           input = "LOS.Rmd",
           keep_input = FALSE,
           ...) {
    n_to_address_lines <- length(to_address)
    to_address[2:n_to_address_lines] <-
      paste("    |", to_address[2:n_to_address_lines])
    to_address <- paste(to_address, collapse = "\n")

    signature <- push_slashes(signature)

    find_resource("template_skeleton", "letter_of_support") %>%
      file() -> infile

    input %>%
      file() -> outfile

    infile %>%
      readLines() %>%
      paste(collapse = "\n") %>%
      tidy_sub("Piglet", from_name) %>%
      tidy_sub("Very Small Animal", from_title) %>%
      tidy_sub("Wood", from_department_type) %>%
      tidy_sub("Hundred Acre", from_department_name) %>%
      tidy_sub("https://hundred.acre.wustl.edu", from_department_url) %>%
      tidy_sub("1968", from_campus_box) %>%
      tidy_sub("(314) 362-5000", from_phone) %>%
      tidy_sub("piglet@wustl.edu", from_email) %>%
      tidy_sub("Winnie the Pooh", to_name) %>%
      tidy_sub("1 Pooh Corner", to_address) %>%
      tidy_sub("14 October 1926", date) %>%
      tidy_sub("Hi Pooh,", salutation) %>%
      tidy_sub("Your closest friend,", closing) %>%
      tidy_sub("piglet.png", signature) %>%
      tidy_sub("Thank.+$", "", fixed = FALSE) %>% # remove existing
      paste0(body) %>% # and append to avoid stripping latex backslashes
      writeLines(outfile)

    close(infile)
    close(outfile)

    rmarkdown::render(input = input, ...)

    if(!keep_input)
      if(!file.remove(input))
        stop("Error deleting skeleton")
  }

#' Consult report document
#' Format for converting from R Markdown to a consult report document.
#' @inheritParams rmarkdown::html_document
#' @param ... additional arguments passed to \code{\link[rmarkdown]{html_document}}
#' @details Requires Pandoc version 2.10 or higher for use of --no-check-certificate option.
#' @seealso  \url{https://pandoc.org/installing.html}
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' @export
consult_report_document <- function(toc = TRUE,
                                    toc_float = TRUE,
                                    toc_depth = 3,
                                    css,
                                    includes,
                                    pandoc_args,
                                    ...) {
  # css
  new_css <- find_resource("template_resource",
                           "consult_report",
                           "edu.wustl.biostatistics.css")
  css <- ifelse(missing(css), new_css, append(css, new_css))

  # includes
  before_body_addition <- find_resource("template_resource",
                                        "consult_report",
                                        "before_body.htm")
  if (missing(includes))
    includes <- rmarkdown::includes(before_body = before_body_addition)
  else
    includes$before_body <- append(includes$before_body, before_body_addition)

  # pandoc_args
  # allows get rockwell font on themadstatter.com due to pandoc distrust in sectigo as root ca
  rmarkdown::pandoc_available("2.10", TRUE)
  ncc <- "--no-check-certificate"
  pandoc_args <- ifelse(missing(pandoc_args), ncc, append(pandoc_args, ncc))

  rmarkdown::html_document(
    toc = toc,
    toc_float = toc_float,
    toc_depth = toc_depth,
    css = css,
    includes = includes,
    pandoc_args = pandoc_args,
    ...
  )
}

#' Estimate document
#' Format for creating a WashU estimate on School of Medicine letterhead
#' @param template Pandoc template to use for rendering. Passed value ignored in favor of default.
#' @param latex_engine LaTeX engine for producing PDF output. Passed value ignored in favor of default.
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{\link[rmarkdown]{pdf_document}}
#' @export
estimate_document <- function(template = find_resource("template_resource",
                                                       "estimate",
                                                       "template.tex"),
                              latex_engine = "xelatex",
                              includes,
                              ...) {
  runtime_in_header <- tex_runtime_in_header(c(tex_setmainfont(),
                                               tex_washusomlogo()))

  if(missing(includes))
    includes <- rmarkdown::includes(runtime_in_header)
  else
    includes$in_header <- append(includes$in_header, runtime_in_header)

  rmarkdown::pdf_document(template = find_resource("template_resource",
                                                   "estimate",
                                                   "template.tex"),
                          latex_engine = "xelatex",
                          includes = includes,
                          ...)
}

#' Estimate items
#' @param data A data frame, data frame extention (e.g. a tibble), or a lazy data frame (e.g., from dbplyr or dtplyr).
#' @param service description of line item
#' @param hours total hours for the line item
#' @param rate rate per hour for the line item
#' @param .protect flag whether to escape for TeX output
#' @export
estimate_items <- function(data, service, hours, rate, .protect = TRUE) {
  est_exprs <- list(
    service = rlang::enquo(service) %missing% NA_character_,
    hours = rlang::enquo(hours) %missing% NA_character_,
    rate = rlang::enquo(rate) %missing% NA_character_)

  out <- dplyr::as_tibble(purrr::map(est_exprs, rlang::eval_tidy, data = data))

  structure(out,
            preserve = names(est_exprs),
            protect = .protect,
            class = c("washu_estimate_items", class(data))
  )
}

#' Custom S3 Print for washu_estimate_items
#' @param x A washu_estimate_items object to print
#' @param ... additional arguments
#' @importFrom knitr knit_print
#' @export
knit_print.washu_estimate_items <- function(x, ...) {
  x[is.na(x)] <- ""

  if(!(rlang::`%@%`(x, "protect"))) {
    protect_tex_input <- identity
  }

  out <- paste0("    ",
                "\\estimateitem{<<protect_tex_input(service)>>}",
                "{<<protect_tex_input(hours)>>}",
                "{<<protect_tex_input(rate)>>}")
  out <- glue::glue_data(x, out, .open = "<<", .close = ">>")

  knitr::asis_output(
    glue::glue_collapse(
      c("\\begin{estimate}",
        "  \\begin{estimatetable}",
             out,
        "  \\end{estimatetable}",
        "\\end{estimate}",
        sep = "\n"
      )
    )
  )
}

#' Render estimate
#' @param ref,date,description estimate reference, date, and description
#' @param to_name,to_title,to_campus_box,to_email recipient attributes
#' @param from_name,from_title,from_campus_box,from_email sender attributes
#' @param data path to estimate data object containing service, hours, and rate columns
#' @inheritParams rmarkdown::render
#' @param keep_input keep the input document
#' @param ... parameters passed to \code{\link[rmarkdown]{render}}
#' @export
#' @examples
#' \dontrun{
#' ## some yaml may require quotes
#' wu_render_estimate(
#'   paste0(Sys.Date(), "-1"),
#'   Sys.Date(),
#'   "The Woozle Effect",
#'   "Christopher Robin",
#'   "Billy Moon",
#'   "1920",
#'   "robin@wustl.edu",
#'   "Matthew Schuelke, PhD",
#'   "Research Statistician",
#'   "8067",
#'   "schuelke@wustl.edu",
#'   "dat/est.csv"
#' )
#' }
wu_render_estimate <-
  function(ref,
           date,
           description,
           to_name,
           to_title,
           to_campus_box,
           to_email,
           from_name,
           from_title,
           from_campus_box,
           from_email,
           data,
           input = "Estimate.Rmd",
           keep_input = FALSE,
           ...) {
    find_resource("template_skeleton", "estimate") %>%
      file() -> infile

    input %>%
      file() -> outfile

    infile %>%
      readLines() %>%
      paste(collapse = "\n") %>%
      tidy_sub("1926-10-14-1", ref) %>%
      tidy_sub("1926-10-14", date) %>%
      tidy_sub("\"Nothing Every Day: An Inquiry into the Habits of Pooh Bears\"", description) %>%
      tidy_sub("Winnie the Pooh", to_name) %>%
      tidy_sub("Anthropomorphic Bear", to_title) %>%
      tidy_sub("1966", to_campus_box) %>%
      tidy_sub("pooh@wustl.edu", to_email) %>%
      tidy_sub("Piglet", from_name) %>%
      tidy_sub("Very Small Animal", from_title) %>%
      tidy_sub("1968", from_campus_box) %>%
      tidy_sub("piglet@wustl.edu", from_email) %>%
      tidy_sub("\n```\\{r\\}\n.+$", "", fixed = FALSE) %>%
      append(paste(c("```{r, message=FALSE}",
               sprintf("read_csv(\"%s\") %%>%%", data),
               "  estimate_items(service, hours, rate)",
               "```"), collapse = "\n")) %>%
      writeLines(outfile)

    close(infile)
    close(outfile)

    rmarkdown::render(input = input, ...)

    if(!keep_input)
      if(!file.remove(input))
        stop("Error deleting skeleton")
  }

#' Insert css code chunk to make consult report document wide
#'
#' @param width_main width in pixels of the main container
#' @param width_tocify width in pixes of the table of contents container
#'
#' @export
css_insert_wide_chunk <- function(width_main = 15000, width_tocify = 400) {
  fmt_lines <- c(
    '```{css, echo = FALSE}',
    'div.main-container {',
    '  max-width: %ipx;',
    '}',
    '',
    'div.tocify {',
    '  max-width: %ipx;',
    '}',
    '```'
  )
  text <- sprintf(paste(fmt_lines, collapse = '\n'), width_main, width_tocify)
  id <- rstudioapi::getSourceEditorContext()$id
  invisible(rstudioapi::insertText(text = text, id = id))
}
