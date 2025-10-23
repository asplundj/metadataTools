#' Add description attributes to data frame columns
#'
#' Adds a "description" attribute to specified columns. If the description contains
#' square brackets (e.g., "Length `[cm]`"), the content inside the brackets is stored
#' as a "unit" attribute.
#'
#' @param .data A data frame.
#' @param ... Column descriptions, either as named arguments (e.g., Sepal.Length = "Sepal length") or as a single named vector/list (e.g., c(Sepal.Length = "Sepal length", Sepal.Width = "Sepal width [cm]")).
#'
#' @return The modified data frame with description (and possibly unit) attributes.
#'
#' @examples
#' library(dplyr)
#' df <- iris %>%
#'   add_desc(Sepal.Length = "Sepal length", Sepal.Width = "Sepal width `[cm]`") #cm will be stored as unit for sepal width, while no unit will be stored for sepal length
#'
#' @export


add_desc <- function(.data, ...) {
  stopifnot(is.data.frame(.data))

  descs <- list(...)

  # If a single unnamed argument is passed and it's a named vector/list, use it
  if (length(descs) == 1 && is.null(names(descs))) {
    descs <- descs[[1]]
  }

  # Validate input
  if (!is.list(descs) && !is.character(descs)) {
    stop("Descriptions must be provided as named arguments or a named character vector/list.")
  }
  if (is.null(names(descs)) || any(!nzchar(names(descs)))) {
    stop("All descriptions must be named with column names.")
  }

  # Apply attributes
  for (col in names(descs)) {
    if (!col %in% names(.data)) {
      warning(sprintf("Column '%s' not found; skipping.", col), call. = FALSE)
      next
    }

    full_desc <- descs[[col]]
    if (grepl("\\[.*\\]", full_desc)) {
      attr(.data[[col]], "unit") <- sub(".*\\[(.*)\\].*", "\\1", full_desc)
      full_desc <- trimws(sub("\\s*\\[.*\\]", "", full_desc))
    }
    attr(.data[[col]], "description") <- full_desc
  }

  .data
}
