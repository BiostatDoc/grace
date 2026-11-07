equal_three_arm_design <- function(size_bound) {
  grace_design(
    n_total = 30,
    balance_vars = list(sex = categorical_var(c("male", "female"))),
    arms = c("A", "B", "C"),
    ratio = c(1, 1, 1),
    size_bound = size_bound
  )
}

test_that("quota_probs uses remaining slots", {
  design <- small_design_2arm()
  cases <- list(
    list(n = c(A = 3L, B = 2L), expected = c(A = 2 / 5, B = 3 / 5)),
    list(n = c(A = 5L, B = 4L), expected = c(A = 0, B = 1)),
    list(n = c(A = 6L, B = 5L), expected = c(A = 4 / 9, B = 5 / 9))
  )

  for (case in cases) {
    expect_equal(quota_probs(case$n, design), case$expected)
  }
})

test_that("quota_probs uses the first checkpoint at or after the next slot", {
  design <- small_design_2arm(forced_at = 15)
  cases <- list(
    list(n = c(A = 7L, B = 4L), expected = c(A = 1 / 4, B = 3 / 4)),
    list(n = c(A = 7L, B = 7L), expected = c(A = 1, B = 0)),
    list(n = c(A = 8L, B = 7L), expected = c(A = 2 / 5, B = 3 / 5))
  )

  for (case in cases) {
    expect_equal(quota_probs(case$n, design), case$expected)
  }
})

test_that("quota_probs works with three arms", {
  design <- small_design_3arm()

  probs <- quota_probs(c(A = 2L, B = 1L, C = 1L), design)

  expect_equal(probs, c(A = 4 / 8, B = 2 / 8, C = 2 / 8))
  expect_equal(sum(probs), 1)
})

test_that("quota_probs errors when the trial is full", {
  design <- small_design_2arm()

  expect_error(quota_probs(c(A = 10L, B = 10L), design), class = "grace_error_design")
})

test_that("bound_probs forces and blocks arms", {
  design <- small_design_2arm(size_bound = 3)
  even <- c(A = 0.5, B = 0.5)

  forced <- bound_probs(even, c(A = 10L, B = 4L), design)
  unchanged <- bound_probs(even, c(A = 7L, B = 5L), design)

  expect_equal(forced$probs, c(A = 0, B = 1))
  expect_true(forced$bound)
  expect_equal(unchanged$probs, even)
  expect_false(unchanged$bound)
})

test_that("bound_probs splits a forced allocation in proportion to the shortfall", {
  design <- equal_three_arm_design(3)
  base <- c(A = 0.2, B = 0.4, C = 0.4)

  result <- bound_probs(base, c(A = 12L, B = 1L, C = 2L), design)

  expect_equal(result$probs, c(A = 0, B = 4 / 7, C = 3 / 7))
  expect_true(result$bound)
})

test_that("bound_probs blocks an over-filled arm and renormalizes", {
  design <- equal_three_arm_design(3)
  base <- c(A = 0.4, B = 0.3, C = 0.3)

  result <- bound_probs(base, c(A = 8L, B = 3L, C = 3L), design)

  expect_equal(result$probs, c(A = 0, B = 0.5, C = 0.5))
  expect_true(result$bound)
})

test_that("bound_probs reports no change when the blocked arm already has no chance", {
  design <- equal_three_arm_design(3)
  base <- c(A = 0, B = 0.5, C = 0.5)

  result <- bound_probs(base, c(A = 8L, B = 3L, C = 3L), design)

  expect_equal(result$probs, base)
  expect_false(result$bound)
})
