#' Create a plain-text README for one or more datasets
#'
#' Generates a structured `README.txt` file documenting one or more datasets, including general
#' information, methodology, variable descriptions, and metadata. The format follows the example
#' given in <https://site.uit.no/dataverseno/deposit/prepare/#how-to-describe-your-data>.
#'
#' Default contact information (name, institution, email, ORCID) can be set
#' using [set_metadata_defaults()], which stores values in R options. These
#' defaults are used if the corresponding arguments are not explicitly provided.
#'
#' @param file_path Output file path (default "00_README.txt").
#' @param title Title of the dataset.
#' @param doi DOI or other identifier.
#' @param contact_name,contact_institution,contact_email,contact_orcid Contact
#'   person info. Defaults come from R options `metadataTools.*` if not supplied.
#' @param dataset_description A paragraph describing the dataset.
#' @param methodology Brief description of data collection methods.
#' @param datasets A named list of data.frames. You may also pass a single
#'   data.frame as the first argument for convenience.
#' @return Invisibly returns `file_path`.
#'
#' @details
#' This function assumes that variable descriptions and units have been added as attributes using
#' [add_desc()] and [add_unit()], and that dataset-level descriptions have been set using [base::attr()].
#' If no such attributes are present, the README file will still be created, and descriptions can
#' be added manually in the generated text file.
#'
#' If the `datasets` argument does not explicitly include a filename, the function will use the
#' name of the dataset with a `.csv` extension as the default filename.
#'
#' @examples
#' # Load required package
#' library(dplyr)
#'
#' # Create dataset and define descriptions and units for variables
#' df1 <- iris %>%
#'   add_desc(Sepal.Length = "Sepal length [cm]",  # units in square brackets are stored as unit attributes
#'            Sepal.Width = "Sepal width",         # unit can be added separately
#'            Petal.Length = "Petal.Length [cm]",
#'            Petal.Width = "Petal.Width [cm]",
#'            Species = "Four Iris species") %>%
#'   add_unit(Sepal.Width = "cm")  # add unit if not set by add_desc
#'
#' # Add dataset-level description
#' attr(df1, "description") <- "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
#'
#' # Create a second dataset
#' df2 <- df1 %>%
#'   mutate(SepalLtoW = Sepal.Length / Sepal.Width) %>%
#'   add_desc(SepalLtoW = "Sepal length relative to its Width [cm/cm]") %>%
#'   select(SepalLtoW, Species)
#'
#' attr(df2, "description") <- "Sepal length relative to width of four Iris species from the Fisher's dataset"
#'
#' # Set default contact info (optional)
#' set_metadata_defaults(contact_name = "Navn Navnesen",
#'                       contact_institution = "NMBU",
#'                       contact_email = "navn.navnesen@nmbu.no",
#'                       contact_orcid = "0000-0000-0000-0000")
#'
#' # Generate README using defaults
#' create_readme(df1, df2)
#'
#' # Generate README with full metadata
#' create_readme(
#'   file_path = "00_README.txt",
#'   title = "Iris Flower Measurements",
#'   doi = "10.1234/example.doi",
#'   contact_name = "Navn Navnesen",
#'   contact_institution = "NMBU",
#'   contact_email = "navn.navnesen@nmbu.no",
#'   contact_orcid = "0000-0000-0000-0000",
#'   dataset_description = "These datasets contain measurements of iris flowers collected by Edgar Anderson and used by R.A. Fisher.",
#'   methodology = "Measurements were taken manually for sepal and petal dimensions across three iris species.",
#'   datasets = list("df1.csv" = df1, "df2.xlsx" = df2)
#' )

#'
#' @seealso [set_metadata_defaults()], [add_unit()], [add_desc]
#' @export
create_readme <- function(...,
                          file_path = "00_README.txt",
                          title = "",
                          doi = "",
                          contact_name = getOption("metadataTools.contact_name", ""),
                          contact_institution = getOption("metadataTools.contact_institution", ""),
                          contact_email = getOption("metadataTools.contact_email", ""),
                          contact_orcid = getOption("metadataTools.contact_orcid", ""),
                          dataset_description = "",
                          methodology = "",
                          datasets = NULL) {
  # Handle datasets from ... or from named argument
  dots <- list(...)
  if (!is.null(datasets)) {
    data_list <- datasets
  } else if (length(dots) > 0) {
    # If unnamed, use deparsed names
    if (is.null(names(dots)) || any(names(dots) == "")) {
      arg_names <- as.list(match.call(expand.dots = FALSE)$...)
      names(dots) <- vapply(arg_names, function(x) deparse(x), character(1))
    }
    data_list <- dots
  } else {
    stop("Please supply one or more data.frames either via `...` or `datasets = list(...)`.")
  }

  # Validate all are data.frames
  if (!all(vapply(data_list, is.data.frame, logical(1)))) {
    stop("All inputs must be data.frames.")
  }

  # Ensure dataset names and file names
  if (is.null(names(data_list)) || any(!nzchar(names(data_list)))) {
    names(data_list) <- paste0("dataset_", seq_along(data_list))
  }
  file_names <- names(data_list)
  file_names <- ifelse(grepl("\\.", file_names), file_names, paste0(file_names, ".csv"))

  scalarize <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    paste(as.character(unlist(x, recursive = TRUE, use.names = FALSE)), collapse = "; ")
  }

  lines <- list()
  add_lines <- function(...) lines <<- append(lines, list(...))

  # General Information
  add_lines(
    "-------------------",
    "GENERAL INFORMATION",
    "-------------------",
    if (length(title) == 1 && nzchar(title)) paste0("// Title of Dataset: ", title) else "// Title of Dataset:",
    paste0("// DOI: ", doi),
    "// Contact Information",
    paste0(" // Name: ", contact_name),
    paste0(" // Institution: ", contact_institution),
    paste0(" // Email: ", contact_email),
    paste0(" // ORCID: ", contact_orcid),
    "",
    "// Description of dataset:",
    dataset_description,
    ""
  )

  # DataverseNO Metadata Information
  add_lines(
    "<Whenever applicable, the following information should be registered in the metadata schema of DataverseNO. In the text below, remove fields/lines that are not applicable, and leave the rest unchanged. >",
    "// Contributors: See metadata field Contributor.",
    "// Kind of data: See metadata field Kind of Data.",
    "// Date of data collection/generation: See metadata field Date of Collection.",
    "// Geographic location: See metadata section Geographic Coverage.",
    "// Funding sources: See metadata section Grant Information.",
    ""
  )

  # Methodology
  add_lines(
    "--------------------------",
    "METHODOLOGICAL INFORMATION",
    "--------------------------",
    methodology,
    ""
  )

  # File overview
  add_lines(
    "--------------------",
    "DATA & FILE OVERVIEW",
    "--------------------",
    "// File List:"
  )

  descriptions <- vapply(data_list, function(df) {
    desc <- attr(df, "description")
    if (is.null(desc)) "No description provided." else as.character(desc)[1]
  }, character(1))

  max_file_len <- max(nchar(file_names))
  file_lines <- mapply(function(name, desc) sprintf("%-*s\t%s", max_file_len, name, desc),
                       file_names, descriptions, SIMPLIFY = TRUE)
  lines <- append(lines, as.list(file_lines))

  # Per-dataset column list
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    fname <- file_names[i]
    header <- paste("DATA-SPECIFIC INFORMATION FOR:", fname)
    separator <- strrep("-", nchar(header))
    add_lines("", separator, header, separator, "// Column List:")

    var_desc <- lapply(df, function(x) scalarize(attr(x, "description")))
    var_unit <- lapply(df, function(x) scalarize(attr(x, "unit")))

    col_names <- names(df)
    max_len <- max(nchar(col_names))

    desc_with_unit <- mapply(function(d, u) {
      d <- if (is.null(d)) "" else d
      u <- if (is.null(u)) "" else u
      if (nzchar(u)) {
        if (nzchar(d)) paste0(d, " (", u, ")") else paste0("(", u, ")")
      } else d
    }, var_desc, var_unit, SIMPLIFY = TRUE)

    col_lines <- mapply(function(col, descu) sprintf("%-*s\t%s", max_len, col, descu),
                        col_names, desc_with_unit, SIMPLIFY = TRUE)
    lines <- append(lines, as.list(col_lines))
  }

  writeLines(unlist(lines), file_path)
  invisible(file_path)
}