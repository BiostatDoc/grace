checkpoint_designs <- function() {
  designs <- list()
  for (method in c("tilted", "rar")) {
    designs[[paste("2 arm", method)]] <- small_design_2arm(forced_at = 15, runin_method = method)
    designs[[paste("3 arm", method)]] <- small_design_3arm(runin_method = method)
  }
  designs
}

state_with_counts <- function(design, n) {
  state <- new_state(design)
  state$n[names(n)] <- n
  state
}

count_failures <- function(design, seed) {
  stream <- make_stream(design$n_total, seed)
  arms <- run_mmsb_loop(design, stream, seed)
  failures <- character(0)
  for (row in seq_len(nrow(design$checkpoints))) {
    n <- design$checkpoints$n[[row]]
    counts <- table(factor(arms[seq_len(n)], levels = design$arms))
    if (!identical(as.integer(counts), unname(design$checkpoints$target[[row]]))) {
      failures <- c(failures, sprintf("seed %d, n %d", seed, n))
    }
  }
  failures
}

test_that("checkpoint counts are exact", {
  for (label in names(checkpoint_designs())) {
    design <- checkpoint_designs()[[label]]
    failures <- unlist(lapply(1:200, \(seed) count_failures(design, seed)))

    expect_identical(as.character(failures), character(0), info = label)
  }
})

test_that("probabilities sum to 1 and are in [0, 1]", {
  real_mmsb_probs <- mmsb_probs
  seen <- list()
  testthat::local_mocked_bindings(
    mmsb_probs = function(state, x, design) {
      result <- real_mmsb_probs(state, x, design)
      seen[[length(seen) + 1]] <<- result$probs
      result
    }
  )

  for (design in checkpoint_designs()) {
    for (seed in 1:50) {
      run_mmsb_loop(design, make_stream(design$n_total, seed), seed)
    }
  }

  sums <- vapply(seen, sum, numeric(1))
  expect_gt(length(seen), 4000)
  expect_true(all(abs(sums - 1) < 1e-12))
  expect_true(all(unlist(seen) >= 0 & unlist(seen) <= 1))
})

test_that("phases follow the algorithm", {
  design <- small_design_2arm(forced_at = 15)
  x <- clean_values(list(sex = "male", race = "white", age = 50), design)

  early <- mmsb_probs(new_state(design), x, design)
  forced_run_in <- mmsb_probs(state_with_counts(design, c(A = 5L, B = 4L)), x, design)
  quota_only <- mmsb_probs(state_with_counts(design, c(A = 7L, B = 7L)), x, design)

  expect_identical(early$phase, "runin")
  expect_identical(forced_run_in$phase, "runin")
  expect_identical(quota_only$phase, "quota")
})

test_that("no votes gives the quota base", {
  design <- small_design_2arm()
  x <- clean_values(list(sex = "male", race = "white", age = 50), design)
  state <- new_state(design)

  result <- mmsb_probs(state, x, design)

  expect_identical(result$probs, quota_probs(state$n, design))
  expect_identical(result$phase, "runin")
})

test_that("a rar run-in returns the quota base without votes", {
  design <- small_design_2arm(runin_method = "rar")
  x <- clean_values(list(sex = "male", race = "white", age = 50), design)
  state <- new_state(design)
  for (arm in c("A", "A", "A", "B")) {
    state <- update_state(state, x, arm, design)
  }

  result <- mmsb_probs(state, x, design)

  expect_identical(result$probs, quota_probs(state$n, design))
  expect_identical(result$phase, "runin")
})

test_that("the size bound takes over after the run-in", {
  design <- grace_design(
    n_total = 40,
    balance_vars = list(sex = categorical_var(c("male", "female"))),
    runin_n = 0,
    size_bound = 2
  )
  x <- clean_values(list(sex = "male"), design)
  state <- new_state(design)
  for (arm in c("A", "A", "A", "A")) {
    state <- update_state(state, x, arm, design)
  }

  result <- mmsb_probs(state, x, design)

  expect_identical(result$phase, "bound")
  expect_equal(result$probs, c(A = 0, B = 1))
})

test_that("biased votes tilt the probabilities after the run-in", {
  design <- grace_design(
    n_total = 400,
    balance_vars = list(sex = categorical_var(c("male", "female"))),
    runin_n = 0,
    size_bound = 1000
  )
  state <- new_state(design)
  fill <- list(
    list("male", "A", 30), list("female", "A", 10), list("male", "B", 10),
    list("female", "B", 30)
  )
  for (row in fill) {
    for (i in seq_len(row[[3]])) {
      state <- update_state(state, list(sex = row[[1]]), row[[2]], design)
    }
  }

  male <- mmsb_probs(state, list(sex = "male"), design)
  female <- mmsb_probs(state, list(sex = "female"), design)

  expect_identical(male$phase, "msb-biased")
  expect_gt(male$probs[["B"]], 0.5)
  expect_gt(female$probs[["A"]], 0.5)
})

reference_votes <- function(state, x, p_star) {
  cat_votes <- vapply(c("sex", "race"), \(name) {
    tab <- state$cat[[name]]
    tab <- tab[rowSums(tab) > 0, , drop = FALSE]
    level <- x[[name]]
    if (nrow(tab) < 2 || any(colSums(tab) == 0) || !(level %in% rownames(tab))) {
      return(0)
    }
    expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    chi_square <- sum((tab - expected)^2 / expected)
    if (pchisq(chi_square, nrow(tab) - 1, lower.tail = FALSE) >= p_star) {
      return(0)
    }
    sign(expected[level, "A"] - tab[level, "A"])
  }, numeric(1))
  s <- state$cont$age
  n_a <- s["A", "n"]
  n_b <- s["B", "n"]
  age_vote <- 0
  if (n_a >= 2 && n_b >= 2) {
    m_a <- s["A", "sum"] / n_a
    m_b <- s["B", "sum"] / n_b
    v_a <- max((s["A", "ss"] - n_a * m_a^2) / (n_a - 1), 0)
    v_b <- max((s["B", "ss"] - n_b * m_b^2) / (n_b - 1), 0)
    se <- sqrt(v_a / n_a + v_b / n_b)
    if (se > 0) {
      t_statistic <- (m_a - m_b) / se
      df <- se^4 / ((v_a / n_a)^2 / (n_a - 1) + (v_b / n_b)^2 / (n_b - 1))
      if (2 * pt(-abs(t_statistic), df) < p_star) {
        age_vote <- if (t_statistic < 0) {
          if (x$age > m_b) 1 else if (x$age < m_a) -1 else 0
        } else {
          if (x$age < m_b) 1 else if (x$age > m_a) -1 else 0
        }
      }
    }
  }
  c(cat_votes, age = age_vote)
}

reference_tilt <- function(p_base, direction, xi) {
  if (direction == 0 || p_base <= 0 || p_base >= 1) {
    return(p_base)
  }
  odds <- p_base / (1 - p_base) * (xi / (1 - xi))^direction
  odds / (1 + odds)
}

test_that("two-arm reduction matches MSB", {
  design <- grace_design(
    n_total = 1000,
    balance_vars = list(
      sex = categorical_var(c("male", "female")),
      race = categorical_var(c("white", "black", "other")),
      age = continuous_var()
    ),
    runin_n = 0,
    size_bound = 1000
  )
  stream <- make_stream(50, 11)
  state <- new_state(design)
  mismatches <- character(0)

  withr::with_seed(5, {
    for (j in seq_len(50)) {
      x <- clean_values(as.list(stream[j, ]), design)
      votes <- reference_votes(state, x, design$p_star)
      n_a <- state$n[["A"]]
      deficit <- n_a - 0.5 * sum(state$n)
      size_vote <- if (abs(deficit) < 1) 0 else -sign(deficit)
      direction <- sign(round(sum(design$weights * votes) + size_vote, 10))
      base <- quota_probs(state$n, design)
      expected <- reference_tilt(base[["A"]], direction, design$xi)
      actual <- mmsb_probs(state, x, design)$probs[["A"]]
      if (abs(actual - expected) > 1e-12) {
        mismatches <- c(mismatches, sprintf("step %d", j))
      }
      arm <- sample(c("A", "B"), 1)
      state <- update_state(state, x, arm, design)
    }
  })

  expect_identical(mismatches, character(0))
})
