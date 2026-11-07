test_that("categorical_var lower-cases and trims levels", {
  var <- categorical_var(c(" Male", "FEMALE "))

  expect_s3_class(var, "grace_var")
  expect_identical(var$type, "categorical")
  expect_identical(var$levels, c("male", "female"))
})

test_that("categorical_var rejects a single level", {
  expect_error(categorical_var("male"), class = "grace_error_design")
})

test_that("categorical_var rejects duplicate levels after cleaning", {
  expect_error(categorical_var(c("male", " Male ")), class = "grace_error_design")
})

test_that("continuous_var returns a continuous grace_var", {
  var <- continuous_var()

  expect_s3_class(var, "grace_var")
  expect_identical(var$type, "continuous")
})
