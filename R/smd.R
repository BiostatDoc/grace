smd_binary <- function(p1, p2) {
  scaled_difference(p1 - p2, sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2))
}

smd_continuous <- function(m1, m2, s1, s2) {
  scaled_difference(m1 - m2, sqrt((s1^2 + s2^2) / 2))
}

scaled_difference <- function(difference, denominator) {
  if (!is.finite(denominator) || denominator == 0 || !is.finite(difference)) {
    return(0)
  }
  difference / denominator
}

largest_pairwise <- function(by_arm, smd_fn) {
  pairs <- utils::combn(names(by_arm), 2, simplify = FALSE)
  values <- vapply(pairs, \(pair) smd_fn(by_arm[[pair[1]]], by_arm[[pair[2]]]), numeric(1))
  max(abs(values))
}

smd_for_variable <- function(values, arm, var, arms) {
  present <- !is.na(values)
  counts <- vapply(arms, \(a) sum(present & arm == a), integer(1))
  if (any(counts == 0)) {
    return(NA_real_)
  }
  if (var$type == "categorical") {
    smd_categorical(values, arm, present, var$levels, arms)
  } else {
    smd_numeric(values, arm, present, arms)
  }
}

smd_categorical <- function(values, arm, present, levels, arms) {
  per_level <- vapply(
    levels,
    \(level) {
      proportions <- lapply(
        stats::setNames(arms, arms),
        \(a) mean(values[present & arm == a] == level)
      )
      largest_pairwise(proportions, smd_binary)
    },
    numeric(1)
  )
  max(per_level)
}

smd_numeric <- function(values, arm, present, arms) {
  moments <- lapply(
    stats::setNames(arms, arms),
    \(a) {
      x <- values[present & arm == a]
      c(mean = mean(x), sd = stats::sd(x))
    }
  )
  largest_pairwise(
    moments,
    \(a, b) smd_continuous(a[["mean"]], b[["mean"]], a[["sd"]], b[["sd"]])
  )
}

#' Largest standardized mean difference for each balance variable
#'
#' For each variable, takes the largest absolute standardized mean difference
#' over all pairs of arms. Categorical variables also take the largest over
#' levels, each level treated as a 0/1 indicator. Rows with a missing value for
#' a variable are left out of that variable. A zero or non-finite denominator
#' gives 0.
#'
#' @param records Records tibble with `seq`, the balance variable columns, and
#'   `arm`.
#' @param design A [grace_design()].
#'
#' @return A tibble with columns `variable` and `smd`. `smd` is `NA` when an arm
#'   has no non-missing values for the variable.
#' @export
#' @examples
#' design <- example_design(20)
#' records <- withr::with_seed(1, example_generator_iid()(20))
#' records$seq <- seq_len(20)
#' records$arm <- rep(c("A", "B"), 10)
#' smd_by_var(records, design)
smd_by_var <- function(records, design) {
  vars <- design$balance_vars
  smd <- vapply(
    names(vars),
    \(name) smd_for_variable(records[[name]], records$arm, vars[[name]], design$arms),
    numeric(1)
  )
  tibble::tibble(variable = names(vars), smd = unname(smd))
}
