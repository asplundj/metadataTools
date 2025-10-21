#' Add description attributes to data frame columns
#'
#' Adds a "description" attribute to specified columns. If the description contains
#' square brackets (e.g., "Length `[cm]`"), the content inside the brackets is stored
#' as a "unit" attribute.
#'
#' @param .data A data frame.
#' @param ... Named arguments where names are column names and values are description strings.
#'
#' @return The modified data frame with description (and possibly unit) attributes.
#'
#' @examples
#' library(dplyr)
#' df <- iris %>%
#'   add_desc(Sepal.Length = "Sepal length", Sepal.Width = "Sepal width [cm]") #cm will be stored as unit for sepal width, while no unit will be stored for sepal length
#'
#' @export
add_desc <- function(.data, ...) {
  stopifnot(is.data.frame(.data))
  descs <- list(...)
  nm <- names(descs)
  if (is.null(nm) || any(!nzchar(nm))) {
    stop("All arguments must be named with column names.")
  }
  for (v in nm) {
    if (v %in% names(.data)) {
      full_desc <- descs[[v]]
      # Extract unit if present in square brackets
      if (grepl("\\[.*\\]", full_desc)) {
        unit <- sub(".*\\[(.*)\\].*", "\\1", full_desc)
        desc <- trimws(sub("\\s*\\[.*\\]", "", full_desc))
        attr(.data[[v]], "unit") <- unit
      } else {
        desc <- full_desc
      }
      attr(.data[[v]], "description") <- desc
    } else {
      warning(sprintf("Column '%s' not found; skipping.", v), call. = FALSE)
    }
  }
  .data
}