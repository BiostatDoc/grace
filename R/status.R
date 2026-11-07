build_status <- function(records, design, vars = NULL) {
  design <- restrict_design(design, vars)
  smd <- smd_by_var(records, design)
  structure(
    list(
      n_randomized = nrow(records),
      group_sizes = group_size_table(records, design),
      balance = balance_table(records, design),
      smd = smd,
      imbalance_plot = imbalance_plot(records, design),
      smd_plot = smd_plot(smd),
      filters = list(from = NULL, to = NULL, checkpoint = NULL, vars = vars)
    ),
    class = "grace_status"
  )
}

restrict_design <- function(design, vars) {
  if (is.null(vars)) {
    return(design)
  }
  known <- names(design$balance_vars)
  valid <- is.character(vars) && length(vars) > 0 && !anyNA(vars)
  if (!valid) {
    abort_status("{.arg vars} must be a character vector of balance variable names.")
  }
  unknown <- setdiff(vars, known)
  if (length(unknown) > 0) {
    abort_status("Unknown balance variable{?s}: {.val {unknown}}.")
  }
  keep <- known[known %in% vars]
  design$balance_vars <- design$balance_vars[keep]
  design$weights <- design$weights[keep]
  design
}

#' Report on the current balance of a trial
#'
#' Reads the trial file and builds group sizes, balance tables, standardized
#' mean differences, and plots without randomizing anyone.
#'
#' @inheritParams randomize
#' @param from,to Optional whole numbers. Only records with `seq >= from` and
#'   `seq <= to` are used.
#' @param checkpoint Optional `"run-in"`, `"final"`, or the enrollment total of
#'   a checkpoint in the design. Only records up to that checkpoint are used.
#'   It cannot be combined with `from` or `to`.
#' @param vars Optional character vector of balance variable names to keep in
#'   the balance outputs.
#'
#' @return A list of class `grace_status` with `n_randomized`, `group_sizes`
#'   ([group_size_table()]), `balance` ([balance_table()]), `smd`
#'   ([smd_by_var()]), `imbalance_plot` ([imbalance_plot()]), `smd_plot`
#'   ([smd_plot()]), and `filters` (the filters that were applied).
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
#'   randomize(path, "P002", list(sex = "female", age = 61))
#'   trial_status(path)
#' })
trial_status <- function(
  path,
  passphrase = trial_passphrase(),
  from = NULL,
  to = NULL,
  checkpoint = NULL,
  vars = NULL
) {
  trial <- read_trial(path, passphrase)
  records <- filter_records(trial$records, trial$design, from, to, checkpoint)
  status <- build_status(records, trial$design, vars)
  status$filters <- list(from = from, to = to, checkpoint = checkpoint, vars = vars)
  status
}
