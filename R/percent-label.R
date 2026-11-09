percent_label <- function(x, digits = 1) {
  valid_digits <- is.numeric(digits) &&
    length(digits) == 1 &&
    is.finite(digits) &&
    digits >= 0 &&
    digits <= 15 &&
    digits == round(digits)
  if (!valid_digits) {
    cli::cli_abort(
      "{.arg digits} must be a single whole number from 0 to 15.",
      class = "grace_error_percent_label"
    )
  }
  label <- sprintf("%.*f%%", as.integer(digits), 100 * x)
  label[is.na(x)] <- NA_character_
  label
}
