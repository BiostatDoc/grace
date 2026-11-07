#' Replay a past assignment
#'
#' Redraws the arm and treatment code of one participant from the random number
#' state and probabilities stored with the record, and compares them with what
#' was recorded. If the record was later overridden, the comparison uses the
#' original arm and code kept in the audit log.
#'
#' @param path Path to the trial file.
#' @param id Participant identifier.
#' @param passphrase Trial file passphrase.
#'
#' @return A one-row tibble with columns `id`, `arm`, `arm_replayed`, `code`,
#'   `code_replayed`, and `match`.
#' @export
#' @examples
#' path <- tempfile(fileext = ".rds")
#' design <- grace_design(
#'   n_total = 20,
#'   balance_vars = list(sex = categorical_var(c("male", "female")))
#' )
#' withr::with_envvar(c(GRACE_PASSPHRASE = "made-up passphrase"), {
#'   create_trial(path, design, seed = 1)
#'   randomize(path, "P001", list(sex = "male"))
#'   replay_assignment(path, "P001")
#' })
replay_assignment <- function(path, id, passphrase = trial_passphrase()) {
  trial <- read_trial(path, passphrase)
  row <- match(id, trial$records$id)
  if (is.na(row)) {
    cli::cli_abort(
      "Participant {.val {id}} is not in the trial file.",
      class = "grace_error_replay"
    )
  }
  record <- trial$records[row, ]
  original <- original_assignment(trial$audit, record)
  rng <- list(kind = trial$rng$kind, state = record$rng_state[[1]])
  replayed <- with_trial_rng(
    rng,
    replay_draw(record$probs[[1]], trial$codes, record$event)
  )$value
  tibble::tibble(
    id = id,
    arm = original$arm,
    arm_replayed = replayed$arm,
    code = original$code,
    code_replayed = replayed$code,
    match = identical(original$arm, replayed$arm) &&
      identical(original$code, replayed$code)
  )
}

original_assignment <- function(audit, record) {
  overrides <- audit[audit$action == "override" & audit$id %in% record$id, ]
  if (nrow(overrides) == 0) {
    return(list(arm = record$arm, code = record$code))
  }
  parsed <- regmatches(
    overrides$old[[1]],
    regexec("^arm=([^;]*); code=(.*)$", overrides$old[[1]])
  )[[1]]
  code <- parsed[[3]]
  list(arm = parsed[[2]], code = if (identical(code, "NA")) NA_character_ else code)
}

replay_draw <- function(probs, codes, event) {
  arm <- draw_arm(probs)
  if (is.null(codes)) {
    return(list(arm = arm, code = NA_character_))
  }
  available <- is.na(codes$drawn_event) | codes$drawn_event >= event
  codes$status <- ifelse(available, "unused", "used")
  list(arm = arm, code = draw_code(codes, arm, event)$code)
}
