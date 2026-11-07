#' Override a participant's assigned arm
#'
#' Changes the arm of one randomized participant and records why. If treatment
#' codes are used, the old code is voided and a new code is drawn for the new
#' arm. The original assignment stays in the audit log, so
#' [replay_assignment()] can still check the original draw. Nothing is saved if
#' any step fails.
#'
#' @param path Path to the trial file.
#' @param id Identifier of a participant who has been randomized.
#' @param arm The new arm. It must differ from the current arm.
#' @param reason A non-blank string explaining the override.
#' @param passphrase Trial file passphrase.
#'
#' @return The updated record, as a one-row tibble, invisibly.
#' @export
#' @examples
#' path <- tempfile(fileext = ".rds")
#' design <- grace_design(
#'   n_total = 20,
#'   balance_vars = list(sex = categorical_var(c("male", "female")))
#' )
#' withr::with_envvar(c(GRACE_PASSPHRASE = "made-up passphrase"), {
#'   create_trial(path, design, seed = 1)
#'   assignment <- randomize(path, "P001", list(sex = "male"))
#'   new_arm <- setdiff(design$arms, assignment$arm)
#'   override_assignment(path, "P001", new_arm, "entered in error")
#' })
override_assignment <- function(path, id, arm, reason, passphrase = trial_passphrase()) {
  trial <- read_trial(path, passphrase)
  row <- check_override(trial, id, arm, !missing(reason) && is_valid_reason(reason))
  old_arm <- trial$records$arm[[row]]
  old_code <- trial$records$code[[row]]
  event <- max(trial$audit$event) + 1L
  swap <- swap_code(trial, arm, id, old_code, event)
  trial$rng <- swap$rng
  trial["codes"] <- list(swap$codes)
  trial$records$arm[[row]] <- arm
  trial$records$code[[row]] <- swap$code
  trial$audit <- append_audit(
    trial$audit,
    action = "override",
    id = id,
    old = sprintf("arm=%s; code=%s", old_arm, old_code),
    new = sprintf("arm=%s; code=%s", arm, swap$code),
    reason = reason
  )
  write_trial(trial, path, passphrase)
  invisible(trial$records[row, ])
}

is_valid_reason <- function(reason) {
  is.character(reason) && length(reason) == 1 && !is.na(reason) && nzchar(trimws(reason))
}

is_single_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

check_override <- function(trial, id, arm, reason_is_valid) {
  row <- if (is_single_string(id)) match(id, trial$records$id) else NA_integer_
  if (is.na(row)) {
    cli::cli_abort(
      "{.arg id} must be the identifier of a randomized participant.",
      class = "grace_error_override"
    )
  }
  if (!is_single_string(arm) || !arm %in% trial$design$arms) {
    cli::cli_abort(
      "{.arg arm} must be one of {.val {trial$design$arms}}.",
      class = "grace_error_override"
    )
  }
  if (identical(arm, trial$records$arm[[row]])) {
    cli::cli_abort(
      "Participant {.val {id}} is already in arm {.val {arm}}.",
      class = "grace_error_override"
    )
  }
  if (!reason_is_valid) {
    cli::cli_abort(
      "{.arg reason} must be a non-blank string.",
      class = "grace_error_override"
    )
  }
  row
}

swap_code <- function(trial, arm, id, old_code, event) {
  if (is.null(trial$codes)) {
    return(list(code = NA_character_, codes = NULL, rng = trial$rng))
  }
  voided <- void_code(trial$codes, old_code)
  drawn <- with_trial_rng(trial$rng, draw_code(voided, arm, event))
  list(
    code = drawn$value$code,
    codes = finish_codes(drawn$value$codes, drawn$value$code, id),
    rng = drawn$rng
  )
}
