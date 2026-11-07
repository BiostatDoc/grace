print_trial <- function(env = parent.frame()) {
  path <- new_test_trial(design = small_design_2arm(forced_at = 15), env = env)
  randomize_stream(path, make_stream(12, seed = 3))
  path
}

test_that("print methods are stable", {
  path <- print_trial()
  design <- read_trial(path, "test-pass")$design
  assignment <- randomize(path, "P100", example_values())
  status <- trial_status(path, from = 2, vars = c("sex", "age"))

  expect_snapshot(print(design))
  expect_snapshot(print(assignment))
  expect_snapshot(print(status))
})

test_that("print methods return their input invisibly and never print plots", {
  path <- print_trial()
  status <- trial_status(path)

  output <- utils::capture.output(result <- withVisible(suppressMessages(print(status))))

  expect_identical(result$value, status)
  expect_false(result$visible)
  expect_false(any(grepl("ggplot", output)))
})
