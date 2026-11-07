srs_probs <- function(state, x, design) {
  list(probs = design$r, phase = "srs")
}

block_probs <- function(state, x, design) {
  if (any(design$ratio != round(design$ratio))) {
    abort_design("Permuted blocks need whole-number {.arg ratio} values.")
  }
  block_size <- 2 * sum(design$ratio)
  quota <- 2 * design$ratio
  completed <- sum(state$n) %/% block_size
  left <- quota - (state$n[names(quota)] - completed * quota)
  list(probs = left / sum(left), phase = "blocks")
}

minimization_probs <- function(state, x, design, p_high = 0.8) {
  rows <- minimization_rows(state, x, design)
  arms <- design$arms
  r <- design$r[arms]
  imbalance <- vapply(
    arms,
    \(arm) candidate_imbalance(arm, rows, design$weights, r),
    numeric(1)
  )
  best <- abs(imbalance - min(imbalance)) < 1e-9
  if (all(best)) {
    return(list(probs = r, phase = "minimization"))
  }
  probs <- r
  probs[best] <- p_high / sum(best)
  probs[!best] <- (1 - p_high) * r[!best] / sum(r[!best])
  list(probs = probs, phase = "minimization")
}

minimization_rows <- function(state, x, design) {
  rows <- list()
  for (name in names(design$balance_vars)) {
    var <- design$balance_vars[[name]]
    if (var$type == "continuous" && is.null(state$breaks[[name]])) {
      abort_design("Minimization needs {.arg breaks} for the continuous variable {.field {name}}.")
    }
    if (is.na(x[[name]])) {
      next
    }
    rows[[name]] <- if (var$type == "categorical") {
      state$cat[[name]][x[[name]], ]
    } else {
      bin <- as.character(findInterval(x[[name]], state$breaks[[name]]) + 1)
      state$cat[[paste0(name, "__bin")]][bin, ]
    }
  }
  rows
}

candidate_imbalance <- function(arm, rows, weights, r) {
  spreads <- vapply(
    names(rows),
    \(name) {
      counts <- rows[[name]]
      counts[arm] <- counts[arm] + 1
      scaled <- counts / r[names(counts)]
      weights[[name]] * (max(scaled) - min(scaled))
    },
    numeric(1)
  )
  sum(spreads)
}

msb_probs <- function(state, x, design) {
  votes <- balance_votes(state, x, design)
  winner <- tally_votes(votes, design$weights)
  list(probs = tilt_probs(design$r, winner, design$xi), phase = "msb")
}
