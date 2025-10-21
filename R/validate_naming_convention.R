#' Validate and annotate a dataset using a naming convention
#'
#' Adds description attributes to columns and checks for missing mandatory variables and invalid values.
#'
#' @param df A data frame to validate.
#' @param naming_object A data frame with columns:
#'   - `name` (character): column name in `df`
#'   - `mandatory` (logical): whether the column is required
#'   - `valid_values` (list-column): allowed values for each variable (or NULL for no check)
#'   - `default_description` (character): description to store as a column attribute
#' @return The annotated data frame.
#' @examples
#' # Create dataset
#' df1 <- data.frame(
#'   Site_ID = rep(c("BLA", "BRA", "GUL", "HAL", "HEM", "LAN", "MRK", "OYT", "SAR", "SKO", "STR", "TRE"), each = 2),
#'   Forest_Type = rep(c("CC", "NN"), times = 12)) %>% 
#'   mutate(Plot_ID = paste(Site_ID, Forest_Type, sep = "_"))
#'
#' # Check if all mandatory variables are included with only valid entries.
#' df1 <- validate_naming_convention(df1, EcoForest_naming)
#'
#' #Check attribute added to Site_ID 
#' attributes(df1$Site_ID)$description
#' @export
validate_naming_convention <- function(df, naming_object) {
  # Identify missing mandatory variables
  mandatory_flags <- ifelse(is.na(naming_object$mandatory), FALSE, naming_object$mandatory)
  missing_vars <- naming_object$name[mandatory_flags & !(naming_object$name %in% names(df))]

  # Add description attributes
  for (i in seq_len(nrow(naming_object))) {
    var <- naming_object$name[i]
    if (var %in% names(df)) {
      attr(df[[var]], "description") <- naming_object$default_description[i]
    }
  }

  # Validate values
  issues <- character()
  for (i in seq_len(nrow(naming_object))) {
    var <- naming_object$name[i]
    if (var %in% names(df)) {
      valid <- naming_object$valid_values[[i]]
      if (!is.null(valid) && length(valid) > 0 && !all(is.na(valid))) {
        invalid <- setdiff(unique(as.character(df[[var]])), as.character(valid))
        if (length(invalid) > 0) {
          issues <- c(issues, sprintf("Variable '%s' contains invalid values: %s", var, paste(invalid, collapse = ", ")))
        }
      }
    }
  }

  # Report results
  if (length(missing_vars) == 0 && length(issues) == 0) {
    message("✅ All mandatory variables are present and valid. Descriptions added.")
  } else {
    if (length(missing_vars) > 0) {
      message("❌ Missing mandatory variables: ", paste(missing_vars, collapse = ", "))
    }
    if (length(issues) > 0) {
      message("⚠️ Issues found:\n", paste(issues, collapse = "\n"))
    }
  }

  invisible(df)
}