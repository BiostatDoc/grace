#' Default run-in size
#'
#' The run-in is the larger of 10 participants and 10% of the total. When every
#' ratio is a whole number, it is rounded up to a multiple of the ratio sum.
#'
#' @param n_total Total planned enrollment.
#' @param ratio Numeric allocation ratio, one value per arm.
#'
#' @return An integer run-in size, at most `n_total`.
#' @export
#' @examples
#' runin_size(60, c(2, 1))
runin_size <- function(n_total, ratio) {
  n1 <- max(10, ceiling(round(0.10 * n_total, 10)))
  if (all(ratio == round(ratio))) {
    n1 <- ceiling(n1 / sum(ratio)) * sum(ratio)
  }
  as.integer(min(n1, n_total))
}

#' Whole-number arm targets
#'
#' Splits `n` across arms by largest-remainder rounding. Ties go to the earlier
#' arm.
#'
#' @param n Number of participants to allocate.
#' @param r Named numeric vector of allocation proportions that sums to 1.
#'
#' @return An integer vector named like `r` that sums to `n`.
#' @export
#' @examples
#' arm_targets(7, c(A = 0.5, B = 0.5))
arm_targets <- function(n, r) {
  exact <- n * r
  targets <- floor(round(exact, 10))
  remainder <- as.integer(n - sum(targets))
  fractions <- round(exact - targets, 10)
  winners <- order(-fractions, seq_along(r))[seq_len(remainder)]
  targets[winners] <- targets[winners] + 1
  stats::setNames(as.integer(targets), names(r))
}

checkpoint_table <- function(n_total, runin_n, forced_at, r) {
  candidates <- tibble::tibble(
    n = as.integer(c(runin_n[runin_n > 0], forced_at, n_total)),
    type = c(
      rep("run-in", sum(runin_n > 0)),
      rep("forced", length(forced_at)),
      "final"
    )
  )
  priority <- match(candidates$type, c("final", "run-in", "forced"))
  candidates <- candidates[order(candidates$n, priority), ]
  checkpoints <- candidates[!duplicated(candidates$n), ]
  checkpoints$target <- lapply(checkpoints$n, arm_targets, r = r)
  checkpoints
}
