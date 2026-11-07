quota_probs <- function(n, design) {
  i <- sum(n) + 1
  if (i > design$n_total) {
    cli::cli_abort(
      "The trial already has {design$n_total} participants.",
      class = "grace_error_design"
    )
  }
  checkpoints <- design$checkpoints
  row <- which(checkpoints$n >= i)[[1]]
  target <- checkpoints$target[[row]]
  remaining <- checkpoints$n[[row]] - (i - 1)
  stats::setNames(pmax(target[names(n)] - n, 0) / remaining, names(n))
}

bound_probs <- function(b, n, design) {
  imbalance <- n - design$r[names(n)] * sum(n)
  tolerance <- 1e-9
  behind <- imbalance <= -design$size_bound + tolerance
  if (any(behind)) {
    forced <- ifelse(behind, -imbalance, 0)
    return(list(probs = forced / sum(forced), bound = TRUE))
  }
  blocked <- imbalance >= design$size_bound - tolerance
  allowed <- ifelse(blocked, 0, b)
  if (!any(blocked & b > 0) || sum(allowed) == 0) {
    return(list(probs = b, bound = FALSE))
  }
  list(probs = allowed / sum(allowed), bound = TRUE)
}
