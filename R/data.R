#' Example participants
#'
#' Forty made-up participants for trying out the package with
#' [example_design()]. They were drawn with [example_generator_iid()] after
#' `set.seed(20260611)`.
#'
#' @format A tibble with 40 rows and 4 columns:
#' \describe{
#'   \item{id}{Participant identifier, `"P001"` to `"P040"`.}
#'   \item{sex}{`"male"` or `"female"`.}
#'   \item{race}{`"white"`, `"black"`, or `"other"`.}
#'   \item{age}{Age in years, rounded to one decimal.}
#' }
#' @source Simulated; not real participants.
#' @examples
#' head(example_participants)
"example_participants"
