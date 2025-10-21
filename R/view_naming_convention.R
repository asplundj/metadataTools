#' View a naming convention in a readable table
#'
#' Displays the naming convention tibble using flextable formatting.
#'
#' @param naming_object A tibble with columns: name, mandatory, valid_values, default_description
#' @return A flextable object
#' @examples
#' ft<-view_naming_convention(EcoForest_naming)
#' print(ft)
#' @export
view_naming_convention <- function(naming_object) {
  format_valid <- function(x) {
    if (is.null(x) || all(is.na(x))) return("")
    paste(x, collapse = ", ")
  }

  naming_object %>%
    dplyr::mutate(
      valid_values = sapply(valid_values, format_valid),
      mandatory = ifelse(mandatory, "Yes", "No")
    ) %>%
    flextable::flextable() %>%
    flextable::fontsize(size = 8, part = "all") %>%
    flextable::width(j = "valid_values", width = 3) %>%
    flextable::set_table_properties(layout = "autofit")
}
