#' Randomize one participant
#'
#' Assigns the next participant to an arm, saves the record, the treatment code
#' (if codes are used), and an audit row in the trial file, and returns the
#' assignment. Nothing is saved if any step fails.
#'
#' @param path Path to the trial file.
#' @param id A single non-empty participant identifier that is not already used.
#' @param values Balance variable values for the participant, as a named list or
#'   a one-row data frame. A missing value (`NA`) triggers a warning but the
#'   participant is still randomized.
#' @param passphrase Trial file passphrase.
#'
#' @return A list of class `grace_assignment` with elements `id`, `arm`, `code`,
#'   `probs`, `phase`, `n_randomized`, and `status` (a `grace_status` report that
#'   includes this participant).
#' @export
#' @examples
#' path <- tempfile(fileext = ".rds")
#' design <- grace_design(
#'   n_total = 20,
#'   balance_vars = list(
#'     sex = categorical_var(c("male", "female")),
#'     age = continuous_var()
#'   )
#' )
#' withr::with_envvar(c(GRACE_PASSPHRASE = "made-up passphrase"), {
#'   create_trial(path, design, seed = 1)
#'   randomize(path, "P001", list(sex = "male", age = 54))
#' })
randomize <- function(path, id, values, passphrase = trial_passphrase()) {
  trial <- read_trial(path, passphrase)
  check_new_id(id, trial)
  x <- clean_values(values, trial$design)
  warn_missing_values(x)
  state <- state_from_records(trial$records, trial$design)
  result <- mmsb_probs(state, x, trial$design)
  event <- max(trial$audit$event) + 1L
  pre_state <- trial$rng$state
  drawn <- with_trial_rng(trial$rng, draw_assignment(result$probs, trial$codes, event))
  arm <- drawn$value$arm
  code <- drawn$value$code
  trial$rng <- drawn$rng
  trial["codes"] <- list(finish_codes(drawn$value$codes, code, id))
  trial$records <- rbind(
    trial$records,
    new_record(trial$records, id, x, arm, code, result, pre_state, event)
  )
  trial$audit <- append_audit(
    trial$audit,
    action = "randomize",
    id = id,
    new = sprintf("arm=%s; code=%s", arm, code)
  )
  write_trial(trial, path, passphrase)
  structure(
    list(
      id = id,
      arm = arm,
      code = code,
      probs = result$probs,
      phase = result$phase,
      n_randomized = nrow(trial$records),
      status = build_status(trial$records, trial$design)
    ),
    class = "grace_assignment"
  )
}

check_new_id <- function(id, trial) {
  valid <- is.character(id) && length(id) == 1 && !is.na(id) && nzchar(trimws(id))
  if (!valid) {
    cli::cli_abort(
      "{.arg id} must be a single non-empty string.",
      class = "grace_error_randomize"
    )
  }
  if (id %in% trial$records$id) {
    cli::cli_abort(
      "Participant {.val {id}} has already been randomized.",
      class = "grace_error_randomize"
    )
  }
  if (nrow(trial$records) >= trial$design$n_total) {
    cli::cli_abort(
      "Enrollment is full: all {trial$design$n_total} participants are randomized.",
      class = "grace_error_randomize"
    )
  }
}

warn_missing_values <- function(x) {
  missing_names <- names(x)[vapply(x, is.na, logical(1))]
  if (length(missing_names) > 0) {
    cli::cli_warn(
      "Missing value{?s} for {.field {missing_names}}.",
      class = "grace_warning_missing"
    )
  }
}

draw_arm <- function(probs) {
  names(probs)[sample.int(length(probs), 1, prob = probs)]
}

draw_assignment <- function(probs, codes, event) {
  arm <- draw_arm(probs)
  if (is.null(codes)) {
    return(list(arm = arm, code = NA_character_, codes = NULL))
  }
  draw <- draw_code(codes, arm, event)
  list(arm = arm, code = draw$code, codes = draw$codes)
}

finish_codes <- function(codes, code, id) {
  if (!is.null(codes)) {
    codes$id[match(code, codes$code)] <- id
  }
  codes
}

new_record <- function(records, id, x, arm, code, result, pre_state, event) {
  tibble::as_tibble(
    c(
      list(seq = nrow(records) + 1L, id = id),
      x,
      list(
        arm = arm,
        code = code,
        probs = list(result$probs),
        phase = result$phase,
        rng_state = list(pre_state),
        event = event,
        time = utc_now()
      )
    )
  )
}
