test_that("run_trial returns one row per participant with the documented columns", {
  design <- small_design_2arm()
  stream <- make_stream(20, 3)

  trial <- withr::with_seed(1, run_trial("mmsb", stream, design))

  expect_s3_class(trial, "tbl_df")
  expect_identical(nrow(trial), 20L)
  expect_identical(
    names(trial),
    c("seq", "sex", "race", "age", "arm", "probs", "phase")
  )
  expect_identical(trial$seq, 1:20)
  expect_true(all(trial$arm %in% design$arms))
  expect_true(all(vapply(trial$probs, \(p) abs(sum(p) - 1) < 1e-12, logical(1))))
  expect_identical(trial$sex, stream$sex)
  expect_identical(trial$age, stream$age)
})

test_that("run_trial with mmsb follows the same draws as the test loop", {
  design <- small_design_2arm(forced_at = 15)
  stream <- make_stream(20, 5)

  trial <- withr::with_seed(8, run_trial("mmsb", stream, design))

  expect_identical(trial$arm, run_mmsb_loop(design, stream, 8))
})

test_that("run_trial maps each method name to its rule", {
  design <- small_design_2arm()
  stream <- make_stream(20, 2)
  phases <- c(
    mmsb = "runin", srs = "srs", blocks = "blocks", msb = "msb", minimization = "minimization"
  )

  for (method in names(phases)) {
    breaks <- if (method == "minimization") list(age = c(-0.5, 0.5)) else NULL
    trial <- withr::with_seed(1, run_trial(method, stream, design, breaks))
    expect_identical(trial$phase[[1]], unname(phases[[method]]), info = method)
  }
})

test_that("run_trial rejects an unknown method", {
  design <- small_design_2arm()

  expect_error(
    run_trial("nonsense", make_stream(20, 1), design),
    class = "grace_error_simulate"
  )
})

test_that("simulate_trials is reproducible", {
  design <- small_design_2arm()

  first <- simulate_trials(design, n_sims = 5, seed = 10)
  second <- simulate_trials(design, n_sims = 5, seed = 10)
  other <- simulate_trials(design, n_sims = 5, seed = 11)

  expect_identical(first, second)
  expect_false(identical(first$value, other$value))
})

test_that("simulate_trials returns a grace_sim tibble with its settings", {
  design <- small_design_2arm()

  result <- simulate_trials(design, methods = c("srs", "mmsb"), n_sims = 3, seed = 4)

  expect_s3_class(result, "grace_sim")
  expect_s3_class(result, "tbl_df")
  expect_identical(names(result), c("sim", "method", "point", "metric", "value"))
  expect_identical(unique(result$sim), 1:3)
  expect_identical(unique(result$method), c("srs", "mmsb"))
  expect_identical(attr(result, "design"), design)
  expect_identical(attr(result, "seed"), 4)
  expect_identical(attr(result, "n_sims"), 3)
  expect_identical(attr(result, "methods"), c("srs", "mmsb"))
  per_trial <- nrow(trial_metrics(run_trial("srs", make_stream(20, 1), design), design))
  expect_identical(nrow(result), 3L * 2L * per_trial)
})

test_that("methods share participant streams", {
  design <- small_design_2arm()
  real_run_trial <- run_trial
  seen <- list()
  testthat::local_mocked_bindings(
    run_trial = function(method, stream, design, breaks = NULL) {
      seen[[length(seen) + 1]] <<- list(method = method, stream = stream)
      real_run_trial(method, stream, design, breaks)
    }
  )

  simulate_trials(design, methods = c("mmsb", "srs", "blocks"), n_sims = 4, seed = 2)

  streams <- lapply(seen, \(call) call$stream)
  by_sim <- split(streams, rep(1:4, each = 3))
  for (sim_streams in by_sim) {
    expect_identical(sim_streams[[1]], sim_streams[[2]])
    expect_identical(sim_streams[[1]], sim_streams[[3]])
  }
  expect_false(identical(by_sim[[1]][[1]], by_sim[[2]][[1]]))
  expect_identical(vapply(seen[1:3], \(call) call$method, character(1)), c("mmsb", "srs", "blocks"))
})

test_that("mmsb hits checkpoint targets in every simulated trial", {
  design <- small_design_2arm(forced_at = 15)

  result <- simulate_trials(design, methods = "mmsb", n_sims = 25, seed = 6)

  size_deviations <- result$value[grepl("^size_dev_", result$metric)]
  expect_gt(length(size_deviations), 0)
  expect_true(all(size_deviations == 0))
})

test_that("mmsb hits checkpoint targets with three arms", {
  design <- small_design_3arm()

  result <- simulate_trials(design, methods = "mmsb", n_sims = 15, seed = 7)

  size_deviations <- result$value[grepl("^size_dev_", result$metric)]
  expect_true(all(size_deviations == 0))
})

test_that("blocks finish with exact sizes when the total is a multiple of the block", {
  design <- small_design_2arm(runin_n = 0)

  result <- simulate_trials(design, methods = "blocks", n_sims = 10, seed = 3)

  end_sizes <- result$value[result$point == "end" & grepl("^size_dev_", result$metric)]
  expect_true(all(end_sizes == 0))
})

test_that("simulate_trials uses breaks for minimization", {
  design <- small_design_2arm()

  result <- simulate_trials(design, methods = "minimization", n_sims = 3, seed = 9)

  expect_identical(unique(result$method), "minimization")
})

test_that("simulate_trials accepts a custom generator", {
  design <- small_design_2arm()
  generator <- design_generator(design, probs = list(sex = c(male = 0.9, female = 0.1)))

  result <- simulate_trials(design, methods = "srs", n_sims = 2, generator = generator, seed = 1)

  expect_identical(unique(result$sim), 1:2)
})

test_that("simulate_trials does not change the user's seed", {
  design <- small_design_2arm()
  set.seed(42)
  seed_before <- .Random.seed

  simulate_trials(design, methods = c("mmsb", "minimization"), n_sims = 2, seed = 5)

  expect_identical(.Random.seed, seed_before)
})

test_that("simulate_trials requires a seed", {
  design <- small_design_2arm()

  expect_error(simulate_trials(design, n_sims = 2), class = "grace_error_simulate")
})

test_that("simulate_trials errors for parallel runs until they are added", {
  design <- small_design_2arm()

  expect_error(
    simulate_trials(design, n_sims = 2, seed = 1, parallel = TRUE),
    "GRACE-23",
    class = "grace_error_simulate"
  )
})

test_that("simulate_trials validates its inputs", {
  design <- small_design_2arm()
  cases <- list(
    unknown_method = list(methods = "nonsense", n_sims = 2),
    no_methods = list(methods = character(0), n_sims = 2),
    repeated_method = list(methods = c("srs", "srs"), n_sims = 2),
    zero_sims = list(methods = "srs", n_sims = 0),
    fractional_sims = list(methods = "srs", n_sims = 1.5)
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    expect_error(
      simulate_trials(design, methods = case$methods, n_sims = case$n_sims, seed = 1),
      class = "grace_error_simulate",
      info = name
    )
  }
  expect_error(
    simulate_trials(list(), n_sims = 2, seed = 1),
    class = "grace_error_simulate"
  )
})
