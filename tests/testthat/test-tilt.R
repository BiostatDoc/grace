test_that("tilt_probs follows the biased coin", {
  two_even <- tilt_probs(c(A = 0.5, B = 0.5), "A", 0.65)
  two_uneven <- tilt_probs(c(A = 2 / 3, B = 1 / 3), "B", 0.65)

  expect_equal(two_even[["A"]], 0.65)
  expect_equal(two_even[["B"]], 0.35)
  expect_lt(abs(two_uneven[["B"]] - 0.481481), 1e-6)
  expect_identical(names(two_uneven), c("A", "B"))
})

test_that("tilt_probs keeps the ratio of the losing arms", {
  base <- c(A = 0.5, B = 0.3, C = 0.2)

  tilted <- tilt_probs(base, "A", 0.65)

  expect_equal(sum(tilted), 1)
  expect_equal(tilted[["B"]] / tilted[["C"]], 0.3 / 0.2)
  expect_gt(tilted[["A"]], base[["A"]])
})

test_that("tilt_probs returns b when there is nothing to tilt", {
  cases <- list(
    list(b = c(A = 0.5, B = 0.5), winner = NA_character_),
    list(b = c(A = 0, B = 1), winner = "A"),
    list(b = c(A = 0, B = 1), winner = "B")
  )

  for (case in cases) {
    expect_identical(tilt_probs(case$b, case$winner, 0.65), case$b)
  }
})
