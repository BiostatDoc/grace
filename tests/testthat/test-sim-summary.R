made_up_sim <- function() {
  results <- tibble::tibble(
    sim = c(1:5, 1:5, 1:3),
    method = c(rep("srs", 5), rep("mmsb", 5), rep("srs", 3)),
    point = c(rep("end", 10), rep("all", 3)),
    metric = c(rep("smd_max", 5), rep("smd_max", 5), rep("correct_guess", 3)),
    value = c(1, 2, 3, 4, 10, 0.5, 0.5, 0.5, 0.5, 0.5, 1, NA, 3)
  )
  structure(
    results,
    class = c("grace_sim", class(results)),
    design = small_design_2arm(),
    seed = 12,
    n_sims = 5,
    methods = c("srs", "mmsb")
  )
}

test_that("summary has one row per method, point, and metric", {
  design <- small_design_2arm(forced_at = 15)

  result <- simulate_trials(design, methods = c("srs", "mmsb"), n_sims = 3, seed = 1)
  summarized <- summary(result)

  keys <- paste(result$method, result$point, result$metric)
  expect_s3_class(summarized, "grace_sim_summary")
  expect_s3_class(summarized, "tbl_df")
  expect_identical(
    names(summarized),
    c("method", "point", "metric", "mean", "sd", "q95", "q99", "max")
  )
  expect_identical(nrow(summarized), length(unique(keys)))
  expect_identical(anyDuplicated(summarized[c("method", "point", "metric")]), 0L)
  expect_identical(unique(summarized$method), c("srs", "mmsb"))
})

test_that("summary statistics match hand calculations", {
  summarized <- summary(made_up_sim())
  row <- function(method, point, metric) {
    keep <- summarized$method == method & summarized$point == point & summarized$metric == metric
    summarized[keep, ]
  }

  srs <- row("srs", "end", "smd_max")
  expect_equal(srs$mean, 4)
  expect_equal(srs$sd, sqrt(12.5))
  expect_equal(srs$q95, 8.8)
  expect_equal(srs$q99, 9.76)
  expect_equal(srs$max, 10)
  constant <- row("mmsb", "end", "smd_max")
  expect_equal(constant$mean, 0.5)
  expect_equal(constant$sd, 0)
  expect_equal(constant$q99, 0.5)
  with_missing <- row("srs", "all", "correct_guess")
  expect_equal(with_missing$mean, 2)
  expect_equal(with_missing$sd, sqrt(2))
  expect_equal(with_missing$q95, 2.9)
  expect_equal(with_missing$q99, 2.98)
  expect_equal(with_missing$max, 3)
})

test_that("summary gives missing values when a metric is missing everywhere", {
  sim <- made_up_sim()
  sim$value[sim$method == "mmsb"] <- NA_real_

  summarized <- summary(sim)
  row <- summarized[summarized$method == "mmsb", ]

  expect_true(is.na(row$mean))
  expect_true(is.na(row$sd))
  expect_true(is.na(row$q95))
  expect_true(is.na(row$max))
})

test_that("parallel and sequential results are identical", {
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("future")
  install_package_for_workers()
  future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(future::sequential))
  design <- small_design_2arm()

  sequential <- simulate_trials(design, n_sims = 4, seed = 3)
  parallel <- simulate_trials(design, n_sims = 4, seed = 3, parallel = TRUE)

  expect_identical(parallel, sequential)
})

test_that("print.grace_sim is stable", {
  expect_snapshot(print(made_up_sim()))
})

test_that("print.grace_sim returns its input invisibly", {
  sim <- made_up_sim()

  expect_invisible(print(sim))
  expect_identical(withVisible(print(sim))$value, sim)
})
