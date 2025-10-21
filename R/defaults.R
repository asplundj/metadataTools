#' Set default metadata contact information
#'
#' Stores default contact fields in R options so you don't have to repeat them.
#' Values can be retrieved with `getOption()` and are not shipped in the package.
#' If `persistent = TRUE`, the defaults are also written to the user's `.Rprofile`
#' so they persist across R sessions.
#'
#' @param contact_name,contact_institution,contact_email,contact_orcid Character
#'   scalars with your details. Use `NULL` to leave unchanged.
#' @param persistent Logical. If `TRUE`, saves the defaults to `.Rprofile`.
#' @return Invisibly returns a named list of the options set.
#' @examples
#' set_metadata_defaults(contact_name = "FirstName LastName",
#'                       contact_institution = "NMBU",
#'                       contact_email = "name@nmbu.no",
#'                       contact_orcid = "0000-0000-0000-0000")
#' @export
set_metadata_defaults <- function(contact_name = NULL,
                                  contact_institution = NULL,
                                  contact_email = NULL,
                                  contact_orcid = NULL,
                                  persistent = TRUE) {
  opts <- list()
  if (!is.null(contact_name))        opts$metadataTools.contact_name <- contact_name
  if (!is.null(contact_institution)) opts$metadataTools.contact_institution <- contact_institution
  if (!is.null(contact_email))       opts$metadataTools.contact_email <- contact_email
  if (!is.null(contact_orcid))       opts$metadataTools.contact_orcid <- contact_orcid

  if (length(opts)) do.call(options, opts)

  if (persistent) {
    rprofile_path <- path.expand("~/.Rprofile")
    lines <- character()
    if (file.exists(rprofile_path)) {
      lines <- readLines(rprofile_path, warn = FALSE)
    }

    # Remove any existing metadataTools options
    lines <- lines[!grepl("^options\\(.*metadataTools\\.", lines)]

    # Escape quotes in values
    escape_quotes <- function(x) gsub('"', '\\"', x)

    # Add new options
    new_lines <- paste0("options(", paste(
      sprintf('%s = "%s"', names(opts), escape_quotes(unlist(opts, use.names = FALSE))),
      collapse = ", "
    ), ")")

    writeLines(c(lines, new_lines), rprofile_path)
    message("Metadata defaults saved to .Rprofile and will persist across sessions.")
  }

  invisible(opts)
}