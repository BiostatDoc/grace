test_that("package version is set", {
  expect_true(utils::packageVersion("grace") >= "0.0.0.9000")
})
