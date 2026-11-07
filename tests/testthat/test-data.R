test_that("example_participants is made-up and well-formed", {
  design <- example_design()
  vars <- design$balance_vars

  expect_s3_class(example_participants, "tbl_df")
  expect_identical(names(example_participants), c("id", "sex", "race", "age"))
  expect_identical(nrow(example_participants), 40L)
  expect_identical(example_participants$id, sprintf("P%03d", 1:40))
  expect_identical(anyDuplicated(example_participants$id), 0L)
  expect_true(all(example_participants$sex %in% vars$sex$levels))
  expect_true(all(example_participants$race %in% vars$race$levels))
  expect_type(example_participants$age, "double")
  expect_false(anyNA(example_participants))
})

test_that("example_participants can be randomized with the example design", {
  design <- example_design(40)

  cleaned <- lapply(
    seq_len(nrow(example_participants)),
    \(j) clean_values(example_participants[j, c("sex", "race", "age")], design)
  )

  expect_length(cleaned, 40)
})

test_that("example_participants comes from the iid example generator", {
  regenerated <- withr::with_seed(20260611, example_generator_iid()(40))

  expect_identical(example_participants$sex, regenerated$sex)
  expect_identical(example_participants$race, regenerated$race)
  expect_identical(example_participants$age, regenerated$age)
})
