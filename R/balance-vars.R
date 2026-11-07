#' Describe a categorical balance variable
#'
#' @param levels Character vector of at least two distinct levels. Levels are
#'   trimmed and lower-cased.
#'
#' @return An object of class `grace_var`.
#' @export
#' @examples
#' categorical_var(c("Male", "Female"))
categorical_var <- function(levels) {
  if (!is.character(levels)) {
    cli::cli_abort("{.arg levels} must be a character vector.", class = "grace_error_design")
  }
  levels <- tolower(trimws(levels))
  if (length(levels) < 2) {
    cli::cli_abort(
      "{.arg levels} must contain at least 2 levels.",
      class = "grace_error_design"
    )
  }
  if (anyDuplicated(levels) > 0) {
    cli::cli_abort(
      "{.arg levels} must be unique after trimming and lower-casing.",
      class = "grace_error_design"
    )
  }
  structure(list(type = "categorical", levels = levels), class = "grace_var")
}

#' Describe a continuous balance variable
#'
#' @return An object of class `grace_var`.
#' @export
#' @examples
#' continuous_var()
continuous_var <- function() {
  structure(list(type = "continuous"), class = "grace_var")
}
