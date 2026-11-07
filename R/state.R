new_state <- function(design, breaks = NULL) {
  arms <- design$arms
  vars <- design$balance_vars
  is_cat <- vapply(vars, \(v) v$type == "categorical", logical(1))
  cat_counts <- lapply(vars[is_cat], \(v) count_matrix(v$levels, arms))
  bin_counts <- lapply(
    breaks,
    \(cuts) count_matrix(as.character(seq_len(length(cuts) + 1)), arms)
  )
  names(bin_counts) <- if (length(breaks) > 0) paste0(names(breaks), "__bin") else NULL
  cont_sums <- lapply(vars[!is_cat], \(v) sum_matrix(arms))
  list(
    n = stats::setNames(integer(length(arms)), arms),
    cat = c(cat_counts, bin_counts),
    cont = cont_sums,
    breaks = breaks
  )
}

count_matrix <- function(levels, arms) {
  matrix(0L, nrow = length(levels), ncol = length(arms), dimnames = list(levels, arms))
}

sum_matrix <- function(arms) {
  matrix(0, nrow = length(arms), ncol = 3, dimnames = list(arms, c("n", "sum", "ss")))
}

update_state <- function(state, x, arm, design) {
  state$n[[arm]] <- state$n[[arm]] + 1L
  for (name in names(design$balance_vars)) {
    value <- x[[name]]
    if (is.na(value)) {
      next
    }
    if (design$balance_vars[[name]]$type == "categorical") {
      state$cat[[name]][value, arm] <- state$cat[[name]][value, arm] + 1L
    } else {
      state$cont[[name]][arm, ] <- state$cont[[name]][arm, ] + c(1, value, value^2)
      state <- add_bin(state, name, value, arm)
    }
  }
  state
}

add_bin <- function(state, name, value, arm) {
  cuts <- state$breaks[[name]]
  if (is.null(cuts)) {
    return(state)
  }
  bin <- as.character(findInterval(value, cuts) + 1)
  bin_name <- paste0(name, "__bin")
  state$cat[[bin_name]][bin, arm] <- state$cat[[bin_name]][bin, arm] + 1L
  state
}

state_from_records <- function(records, design, breaks = NULL) {
  state <- new_state(design, breaks)
  records <- records[order(records$seq), , drop = FALSE]
  vars <- names(design$balance_vars)
  for (i in seq_len(nrow(records))) {
    x <- lapply(stats::setNames(vars, vars), \(name) records[[name]][[i]])
    state <- update_state(state, x, records$arm[[i]], design)
  }
  state
}
