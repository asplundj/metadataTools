#' Add unit attributes to data frame columns
#'
#' This function allows you to add "unit" attributes to specified columns in a data frame.
#' It is designed to work with the tidyverse and can be used with the pipe (`%>%`) operator.
#'
#' @param .data A data frame.
#' @param ... Named arguments where names are column names and values are unit strings.
#'
#' @return The modified data frame with unit attributes added to specified columns.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' df <- iris %>%
#'   add_unit(Sepal.Length = "cm", Sepal.Width = "cm")
#'
#' str(df)
#' }

#' @export
add_unit <- function(.data, ...) {
  stopifnot(is.data.frame(.data))
  units <- list(...)
  nm <- names(units)
  if (is.null(nm) || any(!nzchar(nm))) {
    stop("All arguments must be named with column names.")
  }
  for (v in nm) {
    if (v %in% names(.data)) {
      attr(.data[[v]], "unit") <- units[[v]]
    } else {
      warning(sprintf("Column '%s' not found; skipping.", v), call. = FALSE)
    }
  }
  .data
}