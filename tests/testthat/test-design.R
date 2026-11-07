vars <- function() {
  list(sex = categorical_var(c("male", "female")), age = continuous_var())
}

test_that("grace_design builds the documented fields", {
  design <- small_design_2arm()

  expect_s3_class(design, "grace_design")
  expect_identical(design$n_total, 20L)
  expect_identical(design$r, c(A = 0.5, B = 0.5))
  expect_equal(unname(design$weights), rep(1 / 3, 3))
  expect_identical(names(design$weights), c("sex", "race", "age"))
  expect_s3_class(design$checkpoints, "tbl_df")
})

test_that("default run-in size comes from runin_size", {
  expect_identical(small_design_2arm()$runin_n, 10L)
  expect_identical(small_design_3arm()$runin_n, 12L)
})

test_that("weights are rescaled and reordered like balance_vars", {
  design <- small_design_2arm(weights = c(age = 1, sex = 2, race = 1))

  expect_identical(design$weights, c(sex = 0.5, race = 0.25, age = 0.25))
})

test_that("forced_at is sorted and unique", {
  design <- small_design_2arm(forced_at = c(15, 12, 15))

  expect_identical(design$forced_at, c(12L, 15L))
})

test_that("n_total must be a whole number of at least 2", {
  for (bad in list(1, 2.5, "10", NA, c(10, 20))) {
    expect_error(
      grace_design(bad, vars()),
      class = "grace_error_design"
    )
  }
})

test_that("arms must be unique with at least two entries", {
  expect_error(grace_design(20, vars(), arms = "A", ratio = 1), class = "grace_error_design")
  expect_error(
    grace_design(20, vars(), arms = c("A", "A")),
    class = "grace_error_design"
  )
})

test_that("ratio must match arms and be positive", {
  expect_error(grace_design(20, vars(), ratio = c(1, 1, 1)), class = "grace_error_design")
  expect_error(grace_design(20, vars(), ratio = c(1, 0)), class = "grace_error_design")
  expect_error(grace_design(20, vars(), ratio = c(1, -1)), class = "grace_error_design")
})

test_that("balance_vars must be a named list of grace_var objects", {
  bad <- list(
    list(),
    list(categorical_var(c("a", "b"))),
    list(x = categorical_var(c("a", "b")), x = continuous_var()),
    list(x = "not a var")
  )

  for (balance_vars in bad) {
    expect_error(grace_design(20, balance_vars), class = "grace_error_design")
  }
})

test_that("weights must be named like balance_vars and positive", {
  expect_error(
    grace_design(20, vars(), weights = c(sex = 1, other = 1)),
    class = "grace_error_design"
  )
  expect_error(
    grace_design(20, vars(), weights = c(sex = 1, age = 0)),
    class = "grace_error_design"
  )
  expect_error(
    grace_design(20, vars(), weights = c(1, 1)),
    class = "grace_error_design"
  )
})

test_that("p_star must be in (0, 1)", {
  for (bad in c(0, 1, -0.1, 1.5)) {
    expect_error(grace_design(20, vars(), p_star = bad), class = "grace_error_design")
  }
})

test_that("xi must be in (0.5, 1)", {
  for (bad in c(0.5, 1, 0.2, 1.5)) {
    expect_error(grace_design(20, vars(), xi = bad), class = "grace_error_design")
  }
})

test_that("size_bound must be a whole number of at least 1", {
  for (bad in list(0, 1.5, "3", NA)) {
    expect_error(grace_design(20, vars(), size_bound = bad), class = "grace_error_design")
  }
})

test_that("runin_n must be a whole number from 0 to n_total", {
  for (bad in list(-1, 21, 2.5, "5")) {
    expect_error(grace_design(20, vars(), runin_n = bad), class = "grace_error_design")
  }
  expect_identical(grace_design(20, vars(), runin_n = 0)$runin_n, 0L)
})

test_that("forced_at values must be whole numbers from 1 to n_total", {
  for (bad in list(0, 21, 2.5, c(5, 30))) {
    expect_error(grace_design(20, vars(), forced_at = bad), class = "grace_error_design")
  }
})
