test_that("binary and continuous SMD match hand calculations", {
  cases <- list(
    list(smd_binary(0.6, 1 / 3), 2 / sqrt(13)),
    list(smd_binary(0.2, 0.8), -0.6 / 0.4),
    list(smd_binary(0, 0), 0),
    list(smd_binary(1, 1), 0),
    list(smd_continuous(10, 12, 2, 2), -1),
    list(smd_continuous(3, 1, 1, 3), 2 / sqrt(5)),
    list(smd_continuous(5, 6, 0, 0), 0),
    list(smd_continuous(5, 6, NA, 1), 0),
    list(smd_continuous(5, 5, 1, 1), 0)
  )

  for (case in cases) {
    expect_equal(case[[1]], case[[2]])
  }
})

test_that("smd_by_var matches hand calculations for two arms", {
  design <- small_design_2arm()
  records <- make_records(design, eight_records())

  result <- smd_by_var(records, design)

  expect_identical(result$variable, c("sex", "race", "age"))
  expect_equal(result$smd[1], 2 / sqrt(13))
  expect_equal(result$smd[3], 0)
  expect_true(result$smd[2] > 0)
})

test_that("smd_by_var takes the largest pairwise SMD", {
  design <- small_design_3arm()
  arms <- rep(c("A", "B", "C"), each = 3)
  centers <- rep(c(10, 20, 40), each = 3)
  df <- tibble::tibble(
    id = as.character(1:9),
    arm = arms,
    sex = rep(c("male", "male", "female"), 3),
    race = rep(c("white", "black", "other"), 3),
    age = centers + rep(c(-1, 0, 1), 3)
  )

  result <- smd_by_var(make_records(design, df), design)

  expect_equal(result$smd, c(0, 0, 30))
})

test_that("smd_by_var is NA when an arm has no values", {
  design <- small_design_2arm()
  df <- eight_records()
  df$age[df$arm == "B"] <- NA

  result <- smd_by_var(make_records(design, df), design)

  expect_identical(is.na(result$smd), c(FALSE, FALSE, TRUE))
})
