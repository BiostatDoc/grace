utc_now <- function() {
  .POSIXct(as.numeric(Sys.time()), tz = "UTC")
}

new_audit <- function() {
  tibble::tibble(
    event = integer(0),
    time = .POSIXct(numeric(0), tz = "UTC"),
    action = character(0),
    id = character(0),
    old = character(0),
    new = character(0),
    reason = character(0)
  )
}

append_audit <- function(audit, action, id = NA, old = NA, new = NA, reason = NA) {
  row <- tibble::tibble(
    event = max(audit$event, 0L) + 1L,
    time = utc_now(),
    action = as.character(action),
    id = as.character(id),
    old = as.character(old),
    new = as.character(new),
    reason = as.character(reason)
  )
  rbind(audit, row)
}

#' Read the audit log of a trial
#'
#' The audit log is append-only. Each row records one action on the trial.
#'
#' @param path Path to the trial file.
#' @param passphrase Trial file passphrase.
#'
#' @return A tibble with columns `event`, `time`, `action`, `id`, `old`, `new`,
#'   and `reason`.
#' @export
#' @examples
#' path <- tempfile(fileext = ".rds")
#' design <- grace_design(
#'   n_total = 20,
#'   balance_vars = list(sex = categorical_var(c("male", "female")))
#' )
#' withr::with_envvar(c(GRACE_PASSPHRASE = "made-up passphrase"), {
#'   create_trial(path, design, seed = 1)
#'   audit_log(path)
#' })
audit_log <- function(path, passphrase = trial_passphrase()) {
  read_trial(path, passphrase)$audit
}
