status_trial <- function(env = parent.frame()) {
  path <- new_test_trial(design = small_design_2arm(forced_at = 15), env = env)
  randomize_stream(path, make_stream(12, seed = 3))
  path
}

test_that("trial_status returns tables and plots", {
  path <- status_trial()
  records <- read_trial(path, "test-pass")$records
  design <- read_trial(path, "test-pass")$design

  status <- trial_status(path)

  expect_s3_class(status, "grace_status")
  expect_identical(
    names(status),
    c(
      "n_randomized", "group_sizes", "balance", "smd",
      "imbalance_plot", "smd_plot", "filters"
    )
  )
  expect_identical(status$n_randomized, 12L)
  expect_equal(status$group_sizes, group_size_table(records, design))
  expect_equal(status$balance, balance_table(records, design))
  expect_equal(status$smd, smd_by_var(records, design))
  expect_s3_class(status$imbalance_plot, "ggplot")
  expect_s3_class(status$smd_plot, "ggplot")
  expect_identical(
    status$filters,
    list(from = NULL, to = NULL, checkpoint = NULL, vars = NULL)
  )
})

test_that("trial_status applies filters", {
  path <- status_trial()
  cases <- list(
    list(args = list(from = 3, to = 8), n = 6L),
    list(args = list(checkpoint = "run-in"), n = 10L),
    list(args = list(to = 4), n = 4L)
  )

  for (case in cases) {
    status <- do.call(trial_status, c(list(path), case$args))
    expect_identical(status$n_randomized, case$n)
    expect_identical(sum(status$group_sizes$n), case$n)
  }
  expect_identical(trial_status(path, checkpoint = "run-in")$filters$checkpoint, "run-in")
})

test_that("trial_status errors for a wrong passphrase and a missing file", {
  path <- status_trial()

  expect_error(trial_status(path, passphrase = "wrong"))
  expect_error(trial_status(file.path(dirname(path), "none.rds")), class = "grace_error_trial")
})

test_that("vars restricts the balance outputs", {
  path <- status_trial()

  status <- trial_status(path, vars = c("sex", "age"))

  expect_identical(status$smd$variable, c("sex", "age"))
  expect_setequal(status$balance$variable, c("sex", "age"))
  expect_identical(status$filters$vars, c("sex", "age"))
  expect_error(trial_status(path, vars = c("sex", "height")), class = "grace_error_status")
  expect_error(trial_status(path, vars = character(0)), class = "grace_error_status")
})

test_that("build_status handles no records", {
  design <- small_design_2arm()
  records <- make_records(design, eight_records()[0, ])

  status <- build_status(records, design)

  expect_identical(status$n_randomized, 0L)
  expect_s3_class(status$imbalance_plot, "ggplot")
})

test_that("randomize result includes status", {
  path <- new_test_trial()
  randomize_stream(path, make_stream(2, seed = 3))

  assignment <- randomize(path, "P100", example_values())
  stored <- trial_status(path)

  expect_s3_class(assignment$status, "grace_status")
  expect_identical(assignment$status$n_randomized, 3L)
  expect_equal(assignment$status$group_sizes, stored$group_sizes)
  expect_equal(assignment$status$smd, stored$smd)
})
