test_that("package version is set", {
  expect_true(utils::packageVersion("graceDR") >= "0.1.0")
})
