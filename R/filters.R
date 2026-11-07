abort_status <- function(message, .envir = parent.frame()) {
  cli::cli_abort(
    message,
    class = "grace_error_status",
    .envir = .envir,
    call = rlang::caller_env(2)
  )
}

filter_records <- function(records, design, from, to, checkpoint) {
  check_filter_bound(from, "from")
  check_filter_bound(to, "to")
  if (!is.null(checkpoint) && !(is.null(from) && is.null(to))) {
    abort_status("{.arg checkpoint} cannot be combined with {.arg from} or {.arg to}.")
  }
  if (!is.null(from) && !is.null(to) && from > to) {
    abort_status("{.arg from} ({from}) must not be greater than {.arg to} ({to}).")
  }
  if (!is.null(checkpoint)) {
    to <- checkpoint_total(checkpoint, design, nrow(records))
  }
  keep <- rep(TRUE, nrow(records))
  if (!is.null(from)) {
    keep <- keep & records$seq >= from
  }
  if (!is.null(to)) {
    keep <- keep & records$seq <= to
  }
  records[keep, ]
}

check_filter_bound <- function(value, name) {
  if (!is.null(value) && !is_whole(value)) {
    abort_status("{.arg {name}} must be a single whole number or {.code NULL}.")
  }
}

checkpoint_total <- function(checkpoint, design, n_randomized) {
  table <- design$checkpoints
  total <- integer(0)
  if (is_whole(checkpoint) && checkpoint %in% table$n) {
    total <- as.integer(checkpoint)
  } else if (is.character(checkpoint) && length(checkpoint) == 1) {
    total <- table$n[table$type == checkpoint & table$type %in% c("run-in", "final")]
  }
  if (length(total) != 1) {
    abort_status(
      "Unknown {.arg checkpoint}. Use one of {.or {.val {c('run-in', 'final', table$n)}}}."
    )
  }
  if (total > n_randomized) {
    abort_status(
      "Checkpoint {total} has not been reached: {n_randomized} participant{?s} randomized."
    )
  }
  total
}
