test_that("group_size_table counts and targets", {
  design <- small_design_2arm()
  records <- make_records(design, eight_records())

  result <- group_size_table(records, design)

  expect_identical(result$arm, c("A", "B"))
  expect_identical(result$n, c(5L, 3L))
  expect_equal(result$pct, c(62.5, 37.5))
  expect_identical(result$target_next, c(5L, 5L))
  expect_identical(result$target_final, c(10L, 10L))
})

test_that("group_size_table handles empty records and a full trial", {
  design <- small_design_2arm()

  empty <- group_size_table(make_records(design, eight_records()[0, ]), design)
  full_df <- eight_records()[rep(1:8, length.out = 20), ]
  full_df$arm <- rep(c("A", "B"), 10)
  full <- group_size_table(make_records(design, full_df), design)

  expect_identical(empty$n, c(0L, 0L))
  expect_equal(empty$pct, c(0, 0))
  expect_identical(empty$target_next, c(5L, 5L))
  expect_identical(full$target_next, c(10L, 10L))
})

test_that("balance_table formats levels and continuous rows", {
  design <- small_design_2arm()
  records <- make_records(design, eight_records())

  result <- balance_table(records, design)

  expect_identical(names(result), c("variable", "level", "A", "B", "missing", "smd"))
  expect_identical(
    result$variable,
    c("sex", "sex", "race", "race", "race", "age")
  )
  expect_identical(
    result$level,
    c("male", "female", "white", "black", "other", NA)
  )
  expect_identical(
    result$A,
    c(
      "3 (60.0%)",
      "2 (40.0%)",
      "3 (60.0%)",
      "1 (20.0%)",
      "1 (20.0%)",
      "45.0 (12.9)"
    )
  )
  expect_identical(
    result$B,
    c(
      "1 (33.3%)",
      "2 (66.7%)",
      "1 (50.0%)",
      "1 (50.0%)",
      "0 (0.0%)",
      "45.0 (10.0)"
    )
  )
  expect_identical(result$missing, c(0L, 0L, 1L, 1L, 1L, 1L))
  expect_equal(result$smd[1:2], rep(2 / sqrt(13), 2))
  expect_equal(result$smd[6], 0)
  expect_identical(result$smd[3], result$smd[4])
})

test_that("balance_table shows a dash when an arm has no continuous values", {
  design <- small_design_2arm()
  df <- eight_records()
  df$age[df$arm == "B"] <- NA

  result <- balance_table(make_records(design, df), design)

  expect_identical(result$B[6], "-")
  expect_true(is.na(result$smd[6]))
})
