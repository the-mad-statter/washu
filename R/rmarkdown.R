#' Find template resource
#' @param template template to look under
#' @param file file to search for in the given template resources directory
#' @return absolute path to the desired file
find_template_resource <- function(template, file = 'template.tex') {
  relative_path <- file.path("rmarkdown", "templates", template, "resources", file)
  absolute_path <- system.file(relative_path, package = "washu")
  if (absolute_path == "")
    stop("Couldn't find template file ", relative_path, call. = FALSE)
  absolute_path
}

#' Find template skeleton
#' @param template template to look under
#' @return absolute path to the template skeleton
find_template_skeleton <- function(template) {
  relative_path <- file.path("rmarkdown", "templates", template, "skeleton", "skeleton.Rmd")
  absolute_path <- system.file(relative_path, package = "washu")
  if (absolute_path == "")
    stop("Couldn't find skeleton file ", relative_path, call. = FALSE)
  absolute_path
}

#' Find package font path
#' @return Absolute path to the package font directory
find_package_font_path <- function() {
  path <- system.file("resources", "fonts", package = "washu")
  if (path == "")
    stop("Couldn't find package font path", call. = FALSE)
  paste0(path, .Platform$file.sep)
}

#' Push slashes
#' @param x text to modify
#' @return x with backslashes replaced with forward slashes
push_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

#' Letter runtime header additions
#' @return path to a temporary header file
#' @details Package installation paths differ from system to system making
#' the location of some package resources unknown. Examples include the
#' location of package font files and the WashU SOM logo. This function
#' generates and writes the required tex to a temporary file and returns
#' the path to that file to be used as a header inclusion.
letter_runtime_header_additions <- function() {
  in_header_path <- file.path(tempdir(), "in_header.tex")
  in_header_con <- file(in_header_path)
  in_header_contents <-
    c("\\usepackage{fontspec}",
      "\\setmainfont{LibreBaskerville}[",
      sprintf("  Path = %s ,", find_package_font_path()),
      "  Extension = .ttf,",
      "  UprightFont = *-Regular,",
      "  ItalicFont = *-Italic,",
      "  BoldFont = *-Bold]",
      sprintf("\\newcommand{\\washulogo}{%s}",
              find_template_resource("letter",
                                     "washu_som_logo.eps")),
      sprintf("\\newcommand{\\pigletsignature}{%s}",
              find_template_resource("letter",
                                     "piglet.png")))
  writeLines(in_header_contents, in_header_con)
  close(in_header_con)
  in_header_path
}

#' Letter
#' Format for creating a WashU letter on School of Medicine letterhead
#' @param template Pandoc template to use for rendering. Passed value ignored in favor of default.
#' @param latex_engine LaTeX engine for producing PDF output. Passed value ignored in favor of default.
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{\link[rmarkdown]{pdf_document}}
#' @export
letter <- function(template = find_template_resource("letter"),
                   latex_engine = "xelatex",
                   includes, ...) {
  runtime_header_additions <- letter_runtime_header_additions()

  if(missing(includes)) {
    includes <- rmarkdown::includes(runtime_header_additions)
  }
  else {
    if(!is.null(includes$in_header))
      includes$in_header <- append(includes$in_header, runtime_header_additions)
    else
      includes$in_header <- runtime_header_additions
  }

  rmarkdown::pdf_document(template = find_template_resource("letter"),
                          latex_engine = "xelatex",
                          includes = includes,
                          ...)
}

#' Tidy Sub
#' Utility function to make sub() work better with pipes
#' @param x a character vector where matches are sought
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector.
#' @param replacement a replacement for matched pattern in sub and gsub.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' @param ... additional parameters passed to sub
tidy_sub <- function(x, pattern, replacement, fixed = TRUE, ...) {
  sub(pattern, replacement, x, fixed = fixed, ...)
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

#' Render Letter of Support
#' @param from_name full name
#' @param from_title position title
#' @param from_department_type department type
#' @param from_department_name department name
#' @param from_department_url department url
#' @param from_campus_box campus box
#' @param from_phone phone
#' @param from_email email
#' @param to_name full name
#' @param to_address character vector of address lines
#' @param date date
#' @param salutation salutation
#' @param closing closing
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

    find_template_skeleton("letter_of_support") %>%
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

#' Consult report
#' Format for converting from R Markdown to a consult report document.
#' @inheritParams rmarkdown::html_document
#' @param ... additional arguments passed to \code{\link[rmarkdown]{html_document}}
#' @details Requires Pandoc version 2.10 or higher for use of --no-check-certificate option.
#' @seealso  \url{https://pandoc.org/installing.html}
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' @export
consult_report <- function(toc = TRUE,
                           toc_float = TRUE,
                           toc_depth = 3,
                           css,
                           includes,
                           pandoc_args,
                           ...) {
  # css
  css <-
    ifelse(
      missing(css),
      find_template_resource("consult_report", "edu.wustl.biostatistics.css"),
      append(
        css,
        find_template_resource("consult_report", "edu.wustl.biostatistics.css")
      )
    )

  # includes
  before_body_additions <-
    find_template_resource("consult_report", "before_body.htm")
  if (missing(includes)) {
    includes <- rmarkdown::includes(before_body = before_body_additions)
  }
  else {
    if (!is.null(includes$before_body)) {
      includes$before_body <-
        append(includes$before_body, before_body_additions)
    } else {
      includes$before_body <- before_body_additions
    }
  }

  # pandoc_args
  # allows get rockwell font on themadstatter.com due to pandoc distrust in sectigo as root ca
  rmarkdown::pandoc_available("2.10", TRUE)
  pandoc_args <- ifelse(
    missing(pandoc_args),
    "--no-check-certificate",
    append(pandoc_args, "--no-check-certificate")
  )

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
