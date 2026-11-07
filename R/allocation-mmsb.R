mmsb_probs <- function(state, x, design) {
  i <- sum(state$n) + 1
  in_run_in <- i <= design$runin_n
  b <- quota_probs(state$n, design)
  if (max(b) >= 1 - 1e-12) {
    return(list(probs = b, phase = if (in_run_in) "runin" else "quota"))
  }
  if (in_run_in) {
    return(run_in_probs(b, state, x, design))
  }
  bounded <- bound_probs(b, state$n, design)
  if (bounded$bound) {
    return(list(probs = bounded$probs, phase = "bound"))
  }
  votes <- c(balance_votes(state, x, design), size = vote_size(state$n, design$r))
  winner <- tally_votes(votes, c(design$weights, size = 1))
  list(
    probs = tilt_probs(b, winner, design$xi),
    phase = if (is.na(winner)) "msb-random" else "msb-biased"
  )
}

run_in_probs <- function(b, state, x, design) {
  if (design$runin_method == "rar") {
    return(list(probs = b, phase = "runin"))
  }
  winner <- tally_votes(balance_votes(state, x, design), design$weights)
  list(probs = tilt_probs(b, winner, design$xi), phase = "runin")
}

balance_votes <- function(state, x, design) {
  vapply(
    names(design$balance_vars),
    \(name) {
      if (design$balance_vars[[name]]$type == "categorical") {
        vote_categorical(state$cat[[name]], x[[name]], design$p_star)
      } else {
        vote_continuous(state$cont[[name]], x[[name]], design$p_star)
      }
    },
    character(1)
  )
}
