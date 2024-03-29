#' WU colors
#'
#' WashU Color Palettes
#'
#' @seealso \url{https://publicaffairs.wustl.edu/assets/color-palettes/}
#'
#' @export
wu_colors <- list(
  red           = "#a51417",
  dark_gray     = "#6c7373",
  dark_gray_web = "#3c3c3d",
  light_gray    = "#c8c8c8",
  white         = "#ffffff",
  accent_green  = "#007360",
  dark_green    = "#173e3a",
  light_green   = "#789b4a",
  dark_purple   = "#172752",
  dark_blue     = "#005f85",
  light_blue    = "#67c8c7",
  beige         = "#e1c4ac",
  gray          = "#d8d2c5",
  orange        = "#d15f27",
  yellow        = "#f8be15",
  light_purple  = "#622466"
)

#' RGB value pad
#'
#' Pad RGB values with leading 0's
#'
#' @param x integer vector of length 3 with integers between 0 and 255 inclusive
#'
#' @return character vector with three characters per element
#'
#' @examples
#' \dontrun{
#' rgb_value_pad(c(0, 10, 100))
#' }
rgb_value_pad <- function(x) {
  vapply(x, function(y) {
    y <- as.character(y)
    paste(c(rep(" ", 3 - nchar(y)), y), collapse = "")
  }, character(1))
}

#' Print RGB color
#'
#' Print a color as an RGB string
#'
#' @param col vector of any of the three kinds of R color specifications (i.e.,
#' name, hex, or integer)
#'
#' @return character string representation of the rgb color
#'
#' @examples
#' \dontrun{
#' print_rgb(wu_colors$red)
#' }
print_rgb <- function(col) {
  rgb <- as.vector(grDevices::col2rgb(col))
  paste0("rgb(", paste(rgb_value_pad(rgb), collapse = ", "), ")")
}

#' WU colors plot
#'
#' Draw a WashU color palettes plot
#'
#' @seealso \url{https://publicaffairs.wustl.edu/assets/color-palettes/}
#'
#' @export
#'
#' @examples
#' wu_colors_plot()
wu_colors_plot <- function() {
  .data <- data.frame(
    x1 = rep(1, length(wu_colors)),
    x2 = rep(2, length(wu_colors)),
    y1 = rev(seq_along(wu_colors)),
    y2 = (length(wu_colors) + 1):2,
    fil = letters[seq_along(wu_colors)],
    col = names(wu_colors),
    hex = unlist(wu_colors, use.names = FALSE),
    rgb = sapply(wu_colors, print_rgb, USE.NAMES = FALSE)
  )

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = .data,
      mapping = ggplot2::aes(
        xmin = .data$x1,
        xmax = .data$x2,
        ymin = .data$y1,
        ymax = .data$y2,
        fill = .data$fil
      ),
      color = "black"
    ) +
    ggplot2::geom_text(
      data = .data,
      mapping = ggplot2::aes(
        x = .data$x1 + 1 * (.data$x2 - .data$x1) / 4,
        y = .data$y1 + (.data$y2 - .data$y1) / 2,
        label = .data$col
      ),
      size = 4
    ) +
    ggplot2::geom_text(
      data = .data,
      mapping = ggplot2::aes(
        x = .data$x1 + 2 * (.data$x2 - .data$x1) / 4,
        y = .data$y1 + (.data$y2 - .data$y1) / 2,
        label = .data$hex
      ),
      size = 4
    ) +
    ggplot2::geom_text(
      data = .data,
      mapping = ggplot2::aes(
        x = .data$x1 + 3 * (.data$x2 - .data$x1) / 4,
        y = .data$y1 + (.data$y2 - .data$y1) / 2,
        label = .data$rgb
      ),
      size = 4
    ) +
    ggplot2::scale_fill_manual(values = .data$hex) +
    ggplot2::theme_void() +
    ggplot2::guides(fill = FALSE)
}
