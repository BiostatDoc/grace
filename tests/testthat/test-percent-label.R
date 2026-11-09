test_that("percent_label formats proportions as percents", {
  expect_identical(percent_label(0.1234), "12.3%")
  expect_identical(percent_label(0.1234, digits = 0), "12%")
  expect_identical(percent_label(0.1234, digits = 2), "12.34%")
  expect_identical(percent_label(c(0, 1)), c("0.0%", "100.0%"))
})

test_that("percent_label keeps missing values missing", {
  expect_identical(percent_label(c(0.5, NA)), c("50.0%", NA_character_))
  expect_identical(percent_label(NA_real_), NA_character_)
})

test_that("percent_label handles empty input", {
  expect_identical(percent_label(numeric(0)), character(0))
})

test_that("percent_label does not clamp values", {
  expect_identical(percent_label(1.5), "150.0%")
  expect_identical(percent_label(-0.25), "-25.0%")
})

test_that("percent_label rejects invalid digits", {
  cases <- list(
    negative = -1,
    two_values = c(1, 2),
    missing = NA_real_,
    fractional = 1.5,
    text = "1",
    empty = numeric(0),
    null = NULL,
    infinite = Inf,
    too_large = 1e10,
    above_limit = 16
  )

  for (name in names(cases)) {
    expect_error(
      percent_label(0.5, digits = cases[[name]]),
      "digits",
      class = "grace_error_percent_label",
      info = name
    )
  }
})

test_that("percent_label accepts the largest allowed digits", {
  expect_identical(nchar(percent_label(0.5, digits = 15)), 19L)
})
