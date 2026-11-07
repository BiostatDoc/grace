test_that("design_generator returns the right shape and levels", {
  design <- small_design_2arm()
  generator <- design_generator(design)

  participants <- withr::with_seed(1, generator(50))

  expect_s3_class(participants, "tbl_df")
  expect_identical(dim(participants), c(50L, 3L))
  expect_identical(names(participants), c("sex", "race", "age"))
  expect_true(all(participants$sex %in% c("male", "female")))
  expect_true(all(participants$race %in% c("white", "black", "other")))
  expect_type(participants$age, "double")
  expect_false(anyNA(participants))
})

test_that("design_generator returns zero rows for n = 0", {
  generator <- design_generator(small_design_2arm())

  expect_identical(nrow(generator(0)), 0L)
})

test_that("design_generator uses equal probabilities by default", {
  generator <- design_generator(small_design_2arm())

  participants <- withr::with_seed(1, generator(20000))
  shares <- prop.table(table(participants$sex))

  expect_true(all(abs(shares - 0.5) < 0.02))
})

test_that("design_generator uses the supplied probabilities and mean_sd", {
  generator <- design_generator(
    small_design_2arm(),
    probs = list(sex = c(male = 0.9, female = 0.1)),
    mean_sd = list(age = c(mean = 50, sd = 5))
  )

  participants <- withr::with_seed(2, generator(20000))

  expect_lt(abs(mean(participants$sex == "male") - 0.9), 0.02)
  expect_lt(abs(mean(participants$age) - 50), 0.2)
  expect_lt(abs(stats::sd(participants$age) - 5), 0.2)
})

test_that("drift moves probabilities over enrollment", {
  generator <- design_generator(
    small_design_2arm(),
    probs = list(sex = c(male = 0.2, female = 0.8)),
    mean_sd = list(age = c(mean = 40, sd = 5)),
    drift_to = list(
      sex = c(male = 0.8, female = 0.2),
      age = c(mean = 70, sd = 5)
    )
  )

  participants <- withr::with_seed(3, generator(20000))
  first <- participants[1:2000, ]
  last <- participants[18001:20000, ]

  expect_lt(mean(first$sex == "male"), 0.3)
  expect_gt(mean(last$sex == "male"), 0.7)
  expect_lt(mean(first$age), 45)
  expect_gt(mean(last$age), 65)
})

test_that("drift with n = 1 uses the starting values", {
  generator <- design_generator(
    small_design_2arm(),
    probs = list(sex = c(male = 1, female = 0)),
    drift_to = list(sex = c(male = 0, female = 1))
  )

  participants <- withr::with_seed(4, generator(1))

  expect_identical(participants$sex, "male")
})

test_that("design_generator is reproducible under the caller's seed", {
  generator <- design_generator(small_design_2arm())

  first <- withr::with_seed(5, generator(30))
  second <- withr::with_seed(5, generator(30))

  expect_identical(first, second)
})

test_that("design_generator validates inputs", {
  design <- small_design_2arm()
  cases <- list(
    unknown_prob_variable = list(probs = list(height = c(a = 0.5, b = 0.5))),
    unknown_mean_sd_variable = list(mean_sd = list(height = c(mean = 0, sd = 1))),
    unknown_drift_variable = list(drift_to = list(height = c(mean = 0, sd = 1))),
    wrong_level_names = list(probs = list(sex = c(m = 0.5, f = 0.5))),
    missing_level = list(probs = list(sex = c(male = 1))),
    probs_do_not_sum_to_one = list(probs = list(sex = c(male = 0.5, female = 0.6))),
    negative_probability = list(probs = list(sex = c(male = 1.5, female = -0.5))),
    unnamed_probs = list(probs = list(sex = c(0.5, 0.5))),
    zero_sd = list(mean_sd = list(age = c(mean = 0, sd = 0))),
    negative_sd = list(mean_sd = list(age = c(mean = 0, sd = -1))),
    negative_drift_sd = list(drift_to = list(age = c(mean = 0, sd = -1))),
    drift_probs_do_not_sum_to_one = list(
      drift_to = list(sex = c(male = 0.9, female = 0.9))
    ),
    probs_for_continuous = list(probs = list(age = c(mean = 0, sd = 1))),
    mean_sd_for_categorical = list(mean_sd = list(sex = c(mean = 0, sd = 1))),
    mean_sd_missing_names = list(mean_sd = list(age = c(0, 1))),
    unnamed_list = list(probs = list(c(male = 0.5, female = 0.5)))
  )

  for (case in names(cases)) {
    args <- c(list(design = design), cases[[case]])
    expect_error(do.call(design_generator, args), class = "grace_error_generator", info = case)
  }
})

test_that("design_generator rejects a non-design", {
  expect_error(design_generator(list()), class = "grace_error_generator")
})

test_that("generators reject a bad n", {
  generator <- design_generator(small_design_2arm())

  expect_error(generator(-1), class = "grace_error_generator")
  expect_error(generator(1.5), class = "grace_error_generator")
  expect_error(generator(NA), class = "grace_error_generator")
})

test_that("example generators match example_design", {
  design <- example_design()
  iid <- withr::with_seed(6, example_generator_iid()(20000))
  drift <- withr::with_seed(7, example_generator_drift()(20000))

  expect_s3_class(design, "grace_design")
  expect_identical(design$n_total, 200L)
  expect_identical(design$arms, c("A", "B"))
  expect_identical(names(design$balance_vars), c("sex", "race", "age"))
  expect_identical(design$balance_vars$sex$levels, c("male", "female"))
  expect_identical(design$balance_vars$race$levels, c("white", "black", "other"))
  expect_identical(example_design(50)$n_total, 50L)

  for (participants in list(iid, drift)) {
    expect_identical(names(participants), names(design$balance_vars))
    expect_true(all(participants$sex %in% design$balance_vars$sex$levels))
    expect_true(all(participants$race %in% design$balance_vars$race$levels))
  }

  expect_lt(abs(mean(iid$sex == "male") - 0.48), 0.02)
  expect_lt(abs(mean(iid$race == "white") - 0.62), 0.02)
  expect_lt(abs(mean(iid$race == "black") - 0.23), 0.02)
  expect_lt(abs(mean(iid$age) - 60), 0.3)
  expect_lt(abs(stats::sd(iid$age) - 10), 0.3)
  expect_true(all(iid$age == round(iid$age, 1)))

  expect_lt(mean(drift$sex[1:2000] == "male"), 0.3)
  expect_gt(mean(drift$sex[18001:20000] == "male"), 0.7)
  expect_lt(mean(drift$race[1:2000] == "black"), 0.15)
  expect_gt(mean(drift$race[18001:20000] == "black"), 0.40)
  expect_lt(abs(mean(drift$race == "other") - 0.15), 0.02)
  expect_lt(mean(drift$age[1:2000]), 52)
  expect_gt(mean(drift$age[18001:20000]), 68)
})
