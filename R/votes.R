unique_extreme <- function(values, which_extreme = c("min", "max")) {
  which_extreme <- match.arg(which_extreme)
  target <- if (which_extreme == "min") min(values) else max(values)
  hits <- which(abs(values - target) < 1e-12)
  if (length(hits) == 1) names(values)[hits] else NA_character_
}

vote_categorical <- function(tab, level, p_star) {
  no_vote <- NA_character_
  if (is.na(level)) {
    return(no_vote)
  }
  tab <- tab[rowSums(tab) > 0, , drop = FALSE]
  if (nrow(tab) < 2 || any(colSums(tab) == 0) || !(level %in% rownames(tab))) {
    return(no_vote)
  }
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  chi_square <- sum((tab - expected)^2 / expected)
  df <- (nrow(tab) - 1) * (ncol(tab) - 1)
  if (stats::pchisq(chi_square, df, lower.tail = FALSE) >= p_star) {
    return(no_vote)
  }
  shares <- tab[level, ] / colSums(tab)
  unique_extreme(shares, "min")
}

continuous_p_value <- function(s) {
  n <- s[, "n"]
  m <- s[, "sum"] / n
  v <- (s[, "ss"] - n * m^2) / (n - 1)
  if (length(n) == 2) welch_t_p_value(n, m, v) else welch_anova_p_value(n, m, v)
}

welch_t_p_value <- function(n, m, v) {
  standard_error <- sqrt(sum(v / n))
  t_statistic <- (m[[1]] - m[[2]]) / standard_error
  df <- standard_error^4 / sum((v / n)^2 / (n - 1))
  2 * stats::pt(-abs(t_statistic), df)
}

welch_anova_p_value <- function(n, m, v) {
  k <- length(n)
  w <- n / v
  weighted_mean <- sum(w * m) / sum(w)
  numerator <- sum(w * (m - weighted_mean)^2) / (k - 1)
  h <- sum((1 - w / sum(w))^2 / (n - 1))
  f_statistic <- numerator / (1 + 2 * (k - 2) / (k^2 - 1) * h)
  df2 <- (k^2 - 1) / (3 * h)
  stats::pf(f_statistic, k - 1, df2, lower.tail = FALSE)
}

vote_continuous <- function(s, x, p_star) {
  no_vote <- NA_character_
  if (is.na(x) || any(s[, "n"] < 2)) {
    return(no_vote)
  }
  n <- s[, "n"]
  m <- s[, "sum"] / n
  v <- (s[, "ss"] - n * m^2) / (n - 1)
  if (any(v <= 0) || continuous_p_value(s) >= p_star) {
    return(no_vote)
  }
  if (x > max(m)) {
    unique_extreme(m, "min")
  } else if (x < min(m)) {
    unique_extreme(m, "max")
  } else {
    no_vote
  }
}

vote_size <- function(n, r) {
  deficit <- n - r * sum(n)
  if (min(deficit) > -1 + 1e-9) {
    return(NA_character_)
  }
  unique_extreme(deficit, "min")
}

tally_votes <- function(votes, weights) {
  voted <- !is.na(votes)
  if (!any(voted)) {
    return(NA_character_)
  }
  scores <- tapply(weights[names(votes)][voted], votes[voted], sum)
  unique_extreme(stats::setNames(as.numeric(scores), names(scores)), "max")
}
