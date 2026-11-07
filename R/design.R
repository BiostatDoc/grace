#' Define a trial design
#'
#' @param n_total Total planned enrollment, a whole number of at least 2.
#' @param balance_vars Named list of balance variables made with
#'   [categorical_var()] or [continuous_var()].
#' @param arms Character vector of unique arm labels.
#' @param ratio Positive allocation ratio, one value per arm.
#' @param weights Optional positive weights named like `balance_vars`. They are
#'   rescaled to sum to 1. `NULL` gives equal weights.
#' @param p_star Probability in (0, 1).
#' @param xi Probability in (0.5, 1).
#' @param runin_n Run-in size. `NULL` uses [runin_size()] and `0` means no
#'   run-in.
#' @param forced_at Enrollment totals at which allocation is forced to match the
#'   targets.
#' @param size_bound Whole number of at least 1.
#' @param runin_method Run-in method, `"tilted"` or `"rar"`.
#'
#' @return An object of class `grace_design`.
#' @export
#' @examples
#' grace_design(
#'   n_total = 20,
#'   balance_vars = list(
#'     sex = categorical_var(c("male", "female")),
#'     age = continuous_var()
#'   )
#' )
grace_design <- function(
  n_total,
  balance_vars,
  arms = c("A", "B"),
  ratio = c(1, 1),
  weights = NULL,
  p_star = 0.30,
  xi = 0.65,
  runin_n = NULL,
  forced_at = NULL,
  size_bound = 3,
  runin_method = c("tilted", "rar")
) {
  runin_method <- rlang::arg_match(runin_method)
  check_whole(n_total, "n_total", lower = 2)
  check_arms(arms, ratio)
  check_balance_vars(balance_vars)
  check_probability(p_star, "p_star", lower = 0, upper = 1)
  check_probability(xi, "xi", lower = 0.5, upper = 1)
  check_whole(size_bound, "size_bound", lower = 1)
  weights <- normalize_weights(weights, balance_vars)
  runin_n <- if (is.null(runin_n)) runin_size(n_total, ratio) else runin_n
  check_whole(runin_n, "runin_n", lower = 0, upper = n_total)
  forced_at <- normalize_forced_at(forced_at, n_total)

  ratio <- stats::setNames(as.numeric(ratio), arms)
  r <- ratio / sum(ratio)
  structure(
    list(
      n_total = as.integer(n_total),
      arms = arms,
      ratio = ratio,
      r = r,
      balance_vars = balance_vars,
      weights = weights,
      p_star = p_star,
      xi = xi,
      runin_n = as.integer(runin_n),
      forced_at = forced_at,
      size_bound = size_bound,
      runin_method = runin_method,
      checkpoints = checkpoint_table(n_total, runin_n, forced_at, r)
    ),
    class = "grace_design"
  )
}

abort_design <- function(message, .envir = parent.frame()) {
  cli::cli_abort(
    message,
    class = "grace_error_design",
    .envir = .envir,
    call = rlang::caller_env(2)
  )
}

is_whole <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x) && x == round(x)
}

check_whole <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is_whole(x) || x < lower || x > upper) {
    abort_design("{.arg {name}} must be a whole number from {lower} to {upper}.")
  }
}

check_probability <- function(x, name, lower, upper) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x <= lower || x >= upper) {
    abort_design("{.arg {name}} must be a single number between {lower} and {upper}.")
  }
}

check_arms <- function(arms, ratio) {
  if (!is.character(arms) || length(arms) < 2 || anyDuplicated(arms) > 0) {
    abort_design("{.arg arms} must be at least 2 unique labels.")
  }
  if (!is.numeric(ratio) || length(ratio) != length(arms)) {
    abort_design("{.arg ratio} must have one value per arm.")
  }
  if (anyNA(ratio) || any(ratio <= 0)) {
    abort_design("{.arg ratio} values must be positive.")
  }
}

check_balance_vars <- function(balance_vars) {
  if (!is.list(balance_vars) || length(balance_vars) == 0) {
    abort_design("{.arg balance_vars} must be a non-empty list.")
  }
  nms <- names(balance_vars)
  if (is.null(nms) || any(is.na(nms) | nms == "") || anyDuplicated(nms) > 0) {
    abort_design("{.arg balance_vars} must have unique, non-empty names.")
  }
  if (!all(vapply(balance_vars, inherits, logical(1), what = "grace_var"))) {
    abort_design("Every element of {.arg balance_vars} must be a {.cls grace_var}.")
  }
}

normalize_weights <- function(weights, balance_vars) {
  vars <- names(balance_vars)
  if (is.null(weights)) {
    return(stats::setNames(rep(1 / length(vars), length(vars)), vars))
  }
  valid_names <- !is.null(names(weights)) &&
    anyDuplicated(names(weights)) == 0 &&
    setequal(names(weights), vars)
  if (!is.numeric(weights) || !valid_names) {
    abort_design("{.arg weights} must be numeric and named like {.arg balance_vars}.")
  }
  if (anyNA(weights) || any(weights <= 0)) {
    abort_design("{.arg weights} must be positive.")
  }
  weights <- weights[vars]
  weights / sum(weights)
}

normalize_forced_at <- function(forced_at, n_total) {
  if (is.null(forced_at)) {
    return(integer(0))
  }
  for (value in forced_at) {
    check_whole(value, "forced_at", lower = 1, upper = n_total)
  }
  sort(unique(as.integer(forced_at)))
}
