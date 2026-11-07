#' Build a synthetic participant generator from a trial design
#'
#' A generator is a function of `n` that returns a tibble with `n` rows and one
#' column per balance variable, in design order. Draws use R's random number
#' generator, so callers control the seed.
#'
#' @param design A `grace_design`.
#' @param probs Named list. Each name is a categorical variable and each value
#'   is a vector of probabilities named by level that sums to 1 (within 1e-8).
#'   Variables left out get equal probabilities.
#' @param mean_sd Named list. Each name is a continuous variable and each value
#'   is `c(mean = , sd = )`. Variables left out get `c(mean = 0, sd = 1)`.
#' @param drift_to Named list of end-of-enrollment values in the same form as
#'   `probs` and `mean_sd`. For participant `j` of `n`, with
#'   `u = (j - 1) / max(n - 1, 1)`, each value is `start + u * (end - start)`.
#'   Variables left out do not drift.
#'
#' @return A function of `n` that returns a tibble.
#' @export
#' @examples
#' design <- example_design()
#' generator <- design_generator(
#'   design,
#'   probs = list(sex = c(male = 0.3, female = 0.7)),
#'   drift_to = list(sex = c(male = 0.7, female = 0.3))
#' )
#' withr::with_seed(1, generator(5))
design_generator <- function(design, probs = NULL, mean_sd = NULL, drift_to = NULL) {
  if (!inherits(design, "grace_design")) {
    abort_generator("{.arg design} must be a {.cls grace_design}.")
  }
  vars <- design$balance_vars
  start <- generator_start(vars, probs, mean_sd)
  end <- generator_end(vars, start, drift_to)

  function(n) {
    check_generator_n(n)
    u <- (seq_len(n) - 1) / max(n - 1, 1)
    columns <- lapply(
      names(vars),
      function(name) draw_variable(vars[[name]], start[[name]], end[[name]], u)
    )
    tibble::as_tibble(stats::setNames(columns, names(vars)))
  }
}

#' A small made-up trial design
#'
#' Two arms (`A`, `B`) with ratio 1:1, balance variables `sex`, `race`, and
#' `age`, and equal weights.
#'
#' @param n_total Total planned enrollment.
#'
#' @return A `grace_design`.
#' @export
#' @examples
#' example_design(100)
example_design <- function(n_total = 200) {
  grace_design(
    n_total = n_total,
    balance_vars = list(
      sex = categorical_var(c("male", "female")),
      race = categorical_var(c("white", "black", "other")),
      age = continuous_var()
    ),
    arms = c("A", "B"),
    ratio = c(1, 1)
  )
}

#' Example generator with no drift
#'
#' For [example_design()]. P(male) is 0.48, race (white, black, other) is
#' 0.62, 0.23, 0.15, and age is N(60, 10) rounded to 1 decimal.
#'
#' @return A function of `n` that returns a tibble.
#' @export
#' @examples
#' withr::with_seed(1, example_generator_iid()(5))
example_generator_iid <- function() {
  generator <- design_generator(
    example_design(),
    probs = list(
      sex = c(male = 0.48, female = 0.52),
      race = c(white = 0.62, black = 0.23, other = 0.15)
    ),
    mean_sd = list(age = c(mean = 60, sd = 10))
  )
  round_age(generator)
}

#' Example generator with drift over enrollment
#'
#' For [example_design()]. P(male) goes from 0.20 to 0.80, P(black) from 0.08
#' to 0.48 with P(other) fixed at 0.15, and age from mean 48 to mean 72 with
#' sd 8, rounded to 1 decimal.
#'
#' @return A function of `n` that returns a tibble.
#' @export
#' @examples
#' withr::with_seed(1, example_generator_drift()(5))
example_generator_drift <- function() {
  generator <- design_generator(
    example_design(),
    probs = list(
      sex = c(male = 0.20, female = 0.80),
      race = c(white = 0.77, black = 0.08, other = 0.15)
    ),
    mean_sd = list(age = c(mean = 48, sd = 8)),
    drift_to = list(
      sex = c(male = 0.80, female = 0.20),
      race = c(white = 0.37, black = 0.48, other = 0.15),
      age = c(mean = 72, sd = 8)
    )
  )
  round_age(generator)
}

round_age <- function(generator) {
  function(n) {
    participants <- generator(n)
    participants$age <- round(participants$age, 1)
    participants
  }
}

abort_generator <- function(message, .envir = parent.frame()) {
  cli::cli_abort(
    message,
    class = "grace_error_generator",
    .envir = .envir,
    call = rlang::caller_env(2)
  )
}

check_generator_n <- function(n) {
  if (!is_whole(n) || n < 0) {
    abort_generator("{.arg n} must be a whole number of at least 0.")
  }
}

generator_start <- function(vars, probs, mean_sd) {
  check_generator_names(probs, vars, "categorical", "probs")
  check_generator_names(mean_sd, vars, "continuous", "mean_sd")
  lapply(
    stats::setNames(names(vars), names(vars)),
    function(name) {
      supplied <- if (vars[[name]]$type == "categorical") probs[[name]] else mean_sd[[name]]
      resolve_spec(vars[[name]], name, supplied, "start")
    }
  )
}

generator_end <- function(vars, start, drift_to) {
  check_generator_names(drift_to, vars, c("categorical", "continuous"), "drift_to")
  lapply(
    stats::setNames(names(vars), names(vars)),
    function(name) {
      if (is.null(drift_to[[name]])) {
        return(start[[name]])
      }
      resolve_spec(vars[[name]], name, drift_to[[name]], "end")
    }
  )
}

check_generator_names <- function(x, vars, types, arg) {
  if (is.null(x)) {
    return(invisible())
  }
  nms <- names(x)
  if (!is.list(x) || is.null(nms) || any(is.na(nms) | nms == "") || anyDuplicated(nms) > 0) {
    abort_generator("{.arg {arg}} must be a list with unique, non-empty names.")
  }
  unknown <- setdiff(nms, names(vars))
  if (length(unknown) > 0) {
    abort_generator("{.arg {arg}} names unknown variables: {.val {unknown}}.")
  }
  wrong_type <- nms[!vapply(vars[nms], function(v) v$type %in% types, logical(1))]
  if (length(wrong_type) > 0) {
    abort_generator("{.arg {arg}} has the wrong kind of variable: {.val {wrong_type}}.")
  }
}

resolve_spec <- function(var, name, supplied, label) {
  if (var$type == "categorical") {
    return(resolve_probs(var, name, supplied, label))
  }
  resolve_mean_sd(name, supplied, label)
}

resolve_probs <- function(var, name, supplied, label) {
  levels <- var$levels
  if (is.null(supplied)) {
    return(stats::setNames(rep(1 / length(levels), length(levels)), levels))
  }
  valid_names <- !is.null(names(supplied)) &&
    anyDuplicated(names(supplied)) == 0 &&
    setequal(names(supplied), levels)
  if (!is.numeric(supplied) || !valid_names) {
    abort_generator(
      "Probabilities for {.var {name}} ({label}) must be numeric and named by level."
    )
  }
  supplied <- supplied[levels]
  if (anyNA(supplied) || any(supplied < 0) || abs(sum(supplied) - 1) > 1e-8) {
    abort_generator("Probabilities for {.var {name}} ({label}) must be in [0, 1] and sum to 1.")
  }
  supplied
}

resolve_mean_sd <- function(name, supplied, label) {
  if (is.null(supplied)) {
    return(c(mean = 0, sd = 1))
  }
  valid_names <- !is.null(names(supplied)) &&
    anyDuplicated(names(supplied)) == 0 &&
    setequal(names(supplied), c("mean", "sd"))
  if (!is.numeric(supplied) || !valid_names) {
    abort_generator("{.var {name}} ({label}) must be a numeric vector with names mean and sd.")
  }
  supplied <- supplied[c("mean", "sd")]
  if (!all(is.finite(supplied)) || supplied[["sd"]] <= 0) {
    abort_generator("{.var {name}} ({label}) needs a finite mean and sd greater than 0.")
  }
  supplied
}

draw_variable <- function(var, start, end, u) {
  values <- outer(rep(1, length(u)), start) + outer(u, end - start)
  colnames(values) <- names(start)
  if (var$type == "continuous") {
    return(stats::rnorm(length(u), mean = values[, "mean"], sd = values[, "sd"]))
  }
  n_levels <- length(var$levels)
  cumulative <- values %*% upper.tri(diag(n_levels), diag = TRUE)
  index <- 1 + rowSums(stats::runif(length(u)) > cumulative)
  var$levels[pmin(index, n_levels)]
}
