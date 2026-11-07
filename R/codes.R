#' Read a file of treatment codes
#'
#' The file is a CSV with columns `code` and `arm`. Every code must be non-empty
#' and unique, and every arm must be one of `arms`.
#'
#' @param file Path to the CSV file.
#' @param arms Character vector of valid arm labels.
#'
#' @return A tibble with columns `code`, `arm`, `status` (all `"unused"`), `id`,
#'   and `drawn_event`.
#' @export
#' @examples
#' read_codes(system.file("extdata", "example_codes.csv", package = "grace"), c("A", "B"))
read_codes <- function(file, arms) {
  raw <- utils::read.csv(file, colClasses = "character", strip.white = TRUE)
  missing_columns <- setdiff(c("code", "arm"), names(raw))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The codes file is missing column{?s}: {.field {missing_columns}}.",
      class = "grace_error_codes"
    )
  }
  if (anyNA(raw$code) || any(raw$code == "")) {
    cli::cli_abort("Every code must be non-empty.", class = "grace_error_codes")
  }
  if (anyDuplicated(raw$code) > 0) {
    cli::cli_abort("Codes must be unique.", class = "grace_error_codes")
  }
  unknown_arms <- setdiff(raw$arm, arms)
  if (length(unknown_arms) > 0) {
    cli::cli_abort(
      "Unknown arm{?s} in the codes file: {.val {unknown_arms}}.",
      class = "grace_error_codes"
    )
  }
  tibble::tibble(
    code = raw$code,
    arm = raw$arm,
    status = "unused",
    id = NA_character_,
    drawn_event = NA_integer_
  )
}

draw_code <- function(codes, arm, event) {
  pool <- which(codes$arm == arm & codes$status == "unused")
  if (length(pool) == 0) {
    cli::cli_abort(
      "No unused treatment codes are left for arm {.val {arm}}.",
      class = "grace_error_no_codes"
    )
  }
  row <- pool[sample.int(length(pool), 1)]
  codes$status[row] <- "used"
  codes$drawn_event[row] <- as.integer(event)
  list(code = codes$code[row], codes = codes)
}

void_code <- function(codes, code) {
  row <- match(code, codes$code)
  if (is.na(row) || codes$status[row] != "used") {
    cli::cli_abort(
      "Code {.val {code}} is not a used code, so it cannot be voided.",
      class = "grace_error_codes"
    )
  }
  codes$status[row] <- "voided"
  codes
}
