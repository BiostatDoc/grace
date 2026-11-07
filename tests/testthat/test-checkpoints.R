test_that("runin_size follows max(10, 10%) rounded to the ratio", {
  cases <- list(
    list(n_total = 60, ratio = c(1, 1), expected = 10L),
    list(n_total = 200, ratio = c(1, 1), expected = 20L),
    list(n_total = 1000, ratio = c(1, 1), expected = 100L),
    list(n_total = 60, ratio = c(2, 1), expected = 12L),
    list(n_total = 24, ratio = c(2, 1, 1), expected = 12L),
    list(n_total = 8, ratio = c(1, 1), expected = 8L),
    list(n_total = 60, ratio = c(1.5, 1), expected = 10L)
  )

  for (case in cases) {
    expect_identical(runin_size(case$n_total, case$ratio), case$expected)
  }
})

test_that("arm_targets sums to n and rounds by largest remainder", {
  r2 <- c(A = 0.5, B = 0.5)
  r3 <- c(A = 1 / 3, B = 1 / 3, C = 1 / 3)

  expect_identical(arm_targets(10, r2), c(A = 5L, B = 5L))
  expect_identical(arm_targets(7, r2), c(A = 4L, B = 3L))
  expect_identical(arm_targets(10, r3), c(A = 4L, B = 3L, C = 3L))
  expect_identical(arm_targets(0, r3), c(A = 0L, B = 0L, C = 0L))
  for (n in 0:50) {
    expect_identical(sum(arm_targets(n, r3)), as.integer(n))
  }
})

test_that("checkpoint_table merges and orders checkpoints", {
  r <- c(A = 0.5, B = 0.5)

  table <- checkpoint_table(100, 10, c(50, 10, 100), r)

  expect_identical(table$n, c(10L, 50L, 100L))
  expect_identical(table$type, c("run-in", "forced", "final"))
  expect_identical(table$target[[2]], c(A = 25L, B = 25L))
})

test_that("checkpoint_table omits the run-in when runin_n is zero", {
  r <- c(A = 0.5, B = 0.5)

  table <- checkpoint_table(20, 0, NULL, r)

  expect_identical(table$n, 20L)
  expect_identical(table$type, "final")
})
