#' Create a metadata flextable for a dataset
#'
#' Builds a formatted metadata table summarizing the structure and content of a dataset.
#' The table includes variable names, types, units, descriptions, and summary statistics.
#' It is designed to render well in HTML using the `flextable` package.
#'
#' The dataset can include metadata as attributes:
#' - A `"description"` attribute on the dataset itself
#' - `"description"` and `"unit"` attributes on individual columns
#'
#' @param df A data.frame with optional metadata attributes.
#' @param fontname Optional font name to apply to the entire table.
#' @param fontsize Optional font size to apply to the entire table.
#' @return A `flextable` object summarizing the dataset.
#'
#' @details
#' This function assumes that variable descriptions and units have been added as attributes using
#' [add_desc()] and [add_unit()], and that dataset-level descriptions have been set using [base::attr()].
#'
#' @examples
#' # Load required packages
#' library(flextable)
#' library(dplyr)
#'
#' # Create dataset and define descriptions and units for variables
#' df1 <- iris %>%
#'   add_desc(Sepal.Length = "Sepal length [cm]",  # units in square brackets are stored as unit attributes
#'            Sepal.Width  = "Sepal width",        # unit can be added separately
#'            Petal.Length = "Petal.Length [cm]",
#'            Petal.Width  = "Petal.Width [cm]",
#'            Species      = "Four Iris species") %>%
#'   add_unit(Sepal.Width = "cm")  # add unit if not set by add_desc
#'
#' # Add dataset-level description
#' attr(df1, "description") <- "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
#'
#' # Create metadata table
#' ft <- create_metadata_table(df1)
#' ft
#'
#' # Save table as HTML
#' flextable::save_as_html(ft, path = "df1_metadata.html")
#'
#' @seealso [add_unit()],[add_desc()], [flextable::flextable()]
#' @export


create_metadata_table <- function(
    df,
    fontname = NULL,
    fontsize = NULL
) {
  stopifnot(is.data.frame(df))

  scalarize <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    paste(as.character(unlist(x, recursive = TRUE, use.names = FALSE)), collapse = "; ")
  }
  pluralize <- function(n, singular, plural = paste0(singular, "s")) {
    if (isTRUE(all.equal(n, 1))) singular else plural
  }

  n_rows <- nrow(df); n_cols <- ncol(df)
  description <- scalarize(attr(df, "description"))
  has_desc <- nzchar(description)

  # Human-readable size, e.g. "150 observations of 5 variables"
  size_readable <- sprintf(
    "%s %s of %s %s",
    format(n_rows, big.mark = " "),
    pluralize(n_rows, "observation"),
    format(n_cols, big.mark = " "),
    pluralize(n_cols, "variable")
  )

  summary_note <- "Summary shows: numeric = mean ± SD (min–max) with NA count; factor = levels; character = unique count."

  df_meta <- do.call(rbind, lapply(names(df), function(var) {
    value <- df[[var]]
    var_type <- class(value)[1]
    desc <- scalarize(attr(value, "description"))
    unit <- scalarize(attr(value, "unit"))

    if (is.factor(value)) {
      summary_info <- paste(levels(value), collapse = ", ")
    } else if (is.numeric(value)) {
      m    <- mean(value, na.rm = TRUE)
      s    <- stats::sd(value, na.rm = TRUE)
      rng  <- range(value, na.rm = TRUE)
      n_na <- sum(is.na(value))
      summary_info <- sprintf("%.2f ± %.2f (%.2f–%.2f), NA: %d",
                              m, s, rng[1], rng[2], n_na)
    } else if (is.character(value)) {
      summary_info <- sprintf("Unique: %d", length(unique(value)))
    } else if (is.logical(value)) {
      summary_info <- sprintf("TRUE: %d, FALSE: %d, NA: %d",
                              sum(value == TRUE,  na.rm = TRUE),
                              sum(value == FALSE, na.rm = TRUE),
                              sum(is.na(value)))
    } else {
      n_na <- sum(is.na(value))
      summary_info <- sprintf("Class: %s, NA: %d", var_type, n_na)
    }

    data.frame(
      Variable    = var,
      Type        = var_type,
      Unit        = unit,
      Summary     = summary_info,
      Description = desc,
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(df_meta) || length(df_meta) == 0) {
    df_meta <- data.frame(
      Variable = character(),
      Type = character(),
      Unit = character(),
      Summary = character(),
      Description = character(),
      stringsAsFactors = FALSE
    )
  }

  ft <- flextable::flextable(df_meta)

  # Header lines (keep your original structure)
  dataset_name <- deparse(substitute(df))
  line1 <- paste0("Metadata for dataset ", dataset_name)
  line2 <- paste(
    if (has_desc) description else NULL,
    summary_note,
    size_readable,
    collapse = " "
  )

  ft <- flextable::add_header_lines(ft, values = c(line1, line2))

  # Alignment & styling
  ft <- flextable::align(ft, part = "header", align = "left")
  ft <- flextable::align(ft, part = "body",   align = "left")
  ft <- flextable::valign(ft, part = "body",  valign = "top")
  ft <- flextable::bold(ft, i = 1, part = "header", bold = TRUE)

  # remove only the very top rule and the rule between caption lines
  ft <- flextable::hline_top(ft, part = "header", border = officer::fp_border(width = 0))
  ft <- flextable::hline(ft, i = 1, part = "header", border = officer::fp_border(width = 0))

  # Optional global font controls (only if you pass values)
  if (!is.null(fontname)) ft <- flextable::font(ft, part = "all", fontname = fontname)
  if (!is.null(fontsize)) ft <- flextable::fontsize(ft, part = "all", size = fontsize)

  # Keep autofit last so widths are computed automatically
  ft <- flextable::autofit(ft)

  ft
}

