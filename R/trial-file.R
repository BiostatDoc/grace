#' Create a trial file
#'
#' Creates a new encrypted trial file with no participants and one audit row.
#'
#' @param path Path of the file to create. It must not exist.
#' @param design A [grace_design()] object.
#' @param seed Whole number that seeds the trial random number stream.
#' @param codes `NULL`, the path to a codes CSV file, or a tibble from
#'   [read_codes()].
#' @param passphrase Trial file passphrase.
#'
#' @return `path`, invisibly.
#' @export
#' @examples
#' path <- tempfile(fileext = ".rds")
#' design <- grace_design(
#'   n_total = 20,
#'   balance_vars = list(sex = categorical_var(c("male", "female")))
#' )
#' withr::with_envvar(
#'   c(GRACE_PASSPHRASE = "made-up passphrase"),
#'   create_trial(path, design, seed = 1)
#' )
create_trial <- function(path, design, seed, codes = NULL, passphrase = trial_passphrase()) {
  if (file.exists(path)) {
    cli::cli_abort("{.path {path}} already exists.", class = "grace_error_trial")
  }
  if (!inherits(design, "grace_design")) {
    cli::cli_abort("{.arg design} must be a {.cls grace_design}.", class = "grace_error_trial")
  }
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || seed != round(seed)) {
    cli::cli_abort("{.arg seed} must be a single whole number.", class = "grace_error_trial")
  }
  trial <- structure(
    list(
      format_version = 1L,
      package_version = as.character(utils::packageVersion("graceDR")),
      design = design,
      seed = as.integer(seed),
      rng = new_trial_rng(as.integer(seed)),
      records = empty_records(design),
      codes = resolve_codes(codes, design),
      audit = append_audit(new_audit(), "create")
    ),
    class = "grace_trial"
  )
  write_trial(trial, path, passphrase)
  invisible(path)
}

resolve_codes <- function(codes, design) {
  if (is.null(codes)) {
    return(NULL)
  }
  if (is.character(codes)) {
    return(read_codes(codes, design$arms))
  }
  if (!inherits(codes, "data.frame")) {
    cli::cli_abort(
      "{.arg codes} must be NULL, a file path, or a tibble from {.fn read_codes}.",
      class = "grace_error_trial"
    )
  }
  codes
}

empty_records <- function(design) {
  variable_columns <- lapply(
    design$balance_vars,
    \(var) if (var$type == "categorical") character(0) else numeric(0)
  )
  columns <- c(
    list(seq = integer(0), id = character(0)),
    variable_columns,
    list(
      arm = character(0),
      code = character(0),
      probs = list(),
      phase = character(0),
      rng_state = list(),
      event = integer(0),
      time = .POSIXct(numeric(0), tz = "UTC")
    )
  )
  tibble::as_tibble(columns)
}

read_trial <- function(path, passphrase) {
  if (!file.exists(path)) {
    cli::cli_abort("Trial file {.path {path}} does not exist.", class = "grace_error_trial")
  }
  trial <- decrypt_object(readRDS(path), passphrase)
  if (!inherits(trial, "grace_trial")) {
    cli::cli_abort("{.path {path}} is not a trial file.", class = "grace_error_trial")
  }
  trial
}

write_trial <- function(trial, path, passphrase) {
  temporary <- paste0(path, ".tmp")
  saveRDS(encrypt_object(trial, passphrase), temporary)
  if (file.exists(path)) {
    file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  }
  renamed <- suppressWarnings(file.rename(temporary, path))
  if (!renamed) {
    cli::cli_abort(
      c(
        "Could not move the new trial file into place.",
        "i" = "The new data is in {.path {temporary}}."
      ),
      class = "grace_error_trial"
    )
  }
  invisible(path)
}
