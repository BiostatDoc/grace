sex_design <- function() {
  grace_design(
    n_total = 100,
    balance_vars = list(sex = categorical_var(c("male", "female"))),
    runin_n = 0
  )
}

sex_age_design <- function() {
  grace_design(
    n_total = 100,
    balance_vars = list(sex = categorical_var(c("male", "female")), age = continuous_var()),
    runin_n = 0
  )
}

age_breaks <- function() {
  list(age = c(40, 60))
}

add_participants <- function(state, design, participants) {
  for (j in seq_len(nrow(participants))) {
    x <- as.list(participants[j, setdiff(names(participants), "arm")])
    state <- update_state(state, x, participants$arm[[j]], design)
  }
  state
}

repeat_rows <- function(sex, arm, times, age = 50) {
  tibble::tibble(sex = rep(sex, times), age = age, arm = rep(arm, times))
}

test_that("srs_probs returns the target ratio", {
  design <- small_design_3arm()
  state <- new_state(design)
  x <- clean_values(list(sex = "male", race = "white", age = 50), design)

  result <- srs_probs(state, x, design)

  expect_identical(result$probs, design$r)
  expect_identical(result$phase, "srs")
})

test_that("block_probs completes every block exactly", {
  designs <- list(small_design_2arm(), small_design_3arm())

  for (design in designs) {
    ratio <- design$ratio
    block_size <- 2 * sum(ratio)
    quota <- 2 * ratio
    stream <- make_stream(3 * block_size, 9)
    state <- new_state(design)
    withr::with_seed(21, {
      for (j in seq_len(nrow(stream))) {
        x <- clean_values(as.list(stream[j, ]), design)
        probs <- block_probs(state, x, design)$probs
        arm <- names(probs)[sample.int(length(probs), 1, prob = probs)]
        state <- update_state(state, x, arm, design)
        if (j %% block_size == 0) {
          expect_equal(state$n, (j / block_size) * quota)
        }
      }
    })
  }
})

test_that("block_probs returns the remaining quota within a block", {
  design <- small_design_2arm()
  state <- new_state(design)
  state$n[] <- c(2L, 0L)
  x <- clean_values(list(sex = "male", race = "white", age = 50), design)

  result <- block_probs(state, x, design)

  expect_equal(result$probs, c(A = 0, B = 1))
  expect_identical(result$phase, "blocks")
})

test_that("block_probs rejects fractional ratios", {
  design <- grace_design(
    n_total = 100,
    balance_vars = list(sex = categorical_var(c("male", "female"))),
    ratio = c(1.5, 1)
  )
  state <- new_state(design)

  expect_error(block_probs(state, list(sex = "male"), design), class = "grace_error_design")
})

test_that("minimization_probs favours the arm that lowers imbalance", {
  design <- sex_design()
  state <- new_state(design)
  state <- add_participants(state, design, repeat_rows("male", "A", 3))
  state <- add_participants(state, design, repeat_rows("male", "B", 1))

  result <- minimization_probs(state, list(sex = "male"), design)

  expect_equal(result$probs, c(A = 0.2, B = 0.8))
  expect_identical(result$phase, "minimization")
})

test_that("minimization_probs honours p_high", {
  design <- sex_design()
  state <- new_state(design)
  state <- add_participants(state, design, repeat_rows("male", "A", 3))

  result <- minimization_probs(state, list(sex = "male"), design, p_high = 0.9)

  expect_equal(result$probs, c(A = 0.1, B = 0.9))
})

test_that("minimization_probs returns r when every arm ties", {
  design <- sex_design()
  state <- new_state(design)

  fresh <- minimization_probs(state, list(sex = "male"), design)
  state <- add_participants(state, design, repeat_rows("male", "A", 2))
  state <- add_participants(state, design, repeat_rows("male", "B", 2))
  balanced <- minimization_probs(state, list(sex = "male"), design)

  expect_equal(fresh$probs, design$r)
  expect_equal(balanced$probs, design$r)
  expect_identical(fresh$phase, "minimization")
})

test_that("minimization_probs splits the rest in proportion to r with three arms", {
  design <- grace_design(
    n_total = 100,
    balance_vars = list(sex = categorical_var(c("male", "female"))),
    arms = c("A", "B", "C"),
    ratio = c(2, 1, 1),
    runin_n = 0
  )
  state <- new_state(design)
  state <- add_participants(state, design, repeat_rows("male", "A", 4))
  state <- add_participants(state, design, repeat_rows("male", "B", 2))

  result <- minimization_probs(state, list(sex = "male"), design)

  expect_equal(result$probs, c(A = 0.2 * 2 / 3, B = 0.2 / 3, C = 0.8))
  expect_equal(sum(result$probs), 1)
})

test_that("minimization_probs gives the tied best arms an equal share", {
  design <- grace_design(
    n_total = 100,
    balance_vars = list(sex = categorical_var(c("male", "female"))),
    arms = c("A", "B", "C"),
    ratio = c(1, 1, 1),
    runin_n = 0
  )
  state <- new_state(design)
  state <- add_participants(state, design, repeat_rows("male", "A", 4))

  result <- minimization_probs(state, list(sex = "male"), design)

  expect_equal(result$probs, c(A = 0.2, B = 0.4, C = 0.4))
})

test_that("minimization_probs skips missing values", {
  design <- sex_age_design()
  state <- new_state(design, breaks = age_breaks())
  participants <- rbind(
    tibble::tibble(sex = "male", age = 20, arm = c("A", "A")),
    tibble::tibble(sex = "female", age = 20, arm = c("B", "B"))
  )
  state <- add_participants(state, design, participants)

  skipped <- minimization_probs(state, list(sex = NA_character_, age = 25), design)
  used <- minimization_probs(state, list(sex = "male", age = 25), design)
  both_missing <- minimization_probs(state, list(sex = NA_character_, age = NA_real_), design)

  expect_equal(skipped$probs, design$r)
  expect_equal(used$probs, c(A = 0.2, B = 0.8))
  expect_equal(both_missing$probs, design$r)
})

test_that("minimization_probs uses the bins of a continuous variable", {
  design <- sex_age_design()
  state <- new_state(design, breaks = age_breaks())
  state <- add_participants(state, design, tibble::tibble(sex = "male", age = 70, arm = "A"))
  state <- add_participants(state, design, tibble::tibble(sex = "male", age = 70, arm = "A"))

  old <- minimization_probs(state, list(sex = NA_character_, age = 80), design)
  young <- minimization_probs(state, list(sex = NA_character_, age = 20), design)

  expect_equal(old$probs, c(A = 0.2, B = 0.8))
  expect_equal(young$probs, design$r)
})

test_that("minimization_probs needs breaks for continuous variables", {
  design <- sex_age_design()
  state <- new_state(design)

  expect_error(
    minimization_probs(state, list(sex = "male", age = 50), design),
    class = "grace_error_design"
  )
})

test_that("msb_probs equals tilt of the target ratio", {
  design <- sex_design()
  state <- new_state(design)
  state <- add_participants(state, design, repeat_rows("male", "A", 30))
  state <- add_participants(state, design, repeat_rows("female", "A", 10))
  state <- add_participants(state, design, repeat_rows("male", "B", 10))
  state <- add_participants(state, design, repeat_rows("female", "B", 30))

  male <- msb_probs(state, list(sex = "male"), design)
  female <- msb_probs(state, list(sex = "female"), design)

  expect_equal(male$probs, tilt_probs(design$r, "B", design$xi))
  expect_equal(female$probs, tilt_probs(design$r, "A", design$xi))
  expect_identical(male$phase, "msb")
})

test_that("msb_probs ignores the size of the arms and returns r without a vote", {
  design <- sex_design()
  state <- new_state(design)
  state <- add_participants(state, design, repeat_rows("male", "A", 12))

  result <- msb_probs(state, list(sex = "male"), design)

  expect_equal(result$probs, design$r)
  expect_identical(result$phase, "msb")
})
