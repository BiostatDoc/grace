hand_trial <- function(arms, probs) {
  tibble::tibble(seq = seq_along(arms), arm = arms, probs = probs)
}

two_arm_probs <- function(a) {
  c(A = a, B = 1 - a)
}

test_that("correct_guess matches hand calculations", {
  design <- small_design_2arm()
  trial <- hand_trial(
    c("A", "A", "B", "B"),
    list(two_arm_probs(0.5), two_arm_probs(0.3), two_arm_probs(0.2), two_arm_probs(0))
  )

  guess <- correct_guess(trial, design)

  expect_equal(guess, (0.5 + 0.7 + 0.8 + 1) / 4)
})

test_that("correct_guess averages over tied arms", {
  design <- small_design_3arm()
  trial <- hand_trial(
    c("A", "B"),
    list(c(A = 0.2, B = 0.3, C = 0.5), c(A = 0.1, B = 0.4, C = 0.5))
  )

  guess <- correct_guess(trial, design)

  expect_equal(guess, (1 / 3 + 0.45) / 2)
})

test_that("correct_guess is one half for a coin flip with two arms", {
  design <- small_design_2arm()
  trial <- hand_trial(rep(c("A", "B"), 5), rep(list(two_arm_probs(0.5)), 10))

  expect_equal(correct_guess(trial, design), 0.5)
})

test_that("longest_forced_run counts same-arm forced runs", {
  forced_a <- c(A = 1, B = 0)
  forced_b <- c(A = 0, B = 1)
  free <- c(A = 0.5, B = 0.5)
  cases <- list(
    list(arms = c("A", "B", "A"), probs = list(free, free, free), expected = 0),
    list(arms = c("A", "B"), probs = list(forced_a, free), expected = 1),
    list(arms = c("A", "A", "A"), probs = list(forced_a, forced_a, forced_a), expected = 3),
    list(
      arms = c("A", "A", "B", "B", "B"),
      probs = list(forced_a, forced_a, forced_b, forced_b, forced_b),
      expected = 3
    ),
    list(arms = c("A", "B", "A"), probs = list(forced_a, forced_b, forced_a), expected = 1),
    list(arms = c("A", "A", "A"), probs = list(forced_a, free, forced_a), expected = 1),
    list(arms = c("A", "B", "B", "B"), probs = list(free, free, forced_b, forced_b), expected = 2)
  )

  for (case in cases) {
    expect_equal(longest_forced_run(hand_trial(case$arms, case$probs)), case$expected)
  }
})

test_that("trial_metrics has every point and metric", {
  design <- small_design_2arm(forced_at = 15)
  trial <- run_trial("mmsb", make_stream(20, 1), design)

  metrics <- trial_metrics(trial, design)

  per_point <- c(
    "smd_max", "smd_sex", "smd_race", "smd_age", "size_dev_A", "size_dev_B"
  )
  expected <- rbind(
    expand.grid(point = c("run-in", "n=15", "end"), metric = per_point),
    expand.grid(
      point = "all",
      metric = c("pct_deterministic", "longest_forced_run", "correct_guess")
    )
  )
  expect_s3_class(metrics, "tbl_df")
  expect_identical(names(metrics), c("point", "metric", "value"))
  expect_type(metrics$value, "double")
  expect_setequal(
    paste(metrics$point, metrics$metric),
    paste(expected$point, expected$metric)
  )
  expect_identical(nrow(metrics), nrow(expected))
})

test_that("trial_metrics leaves out the run-in point when there is no run-in", {
  design <- small_design_2arm(runin_n = 0)
  trial <- run_trial("srs", make_stream(20, 1), design)

  metrics <- trial_metrics(trial, design)

  expect_setequal(unique(metrics$point), c("end", "all"))
})

test_that("trial_metrics measures size deviations from the checkpoint targets", {
  design <- small_design_2arm()
  trial <- tibble::tibble(
    seq = 1:20,
    make_stream(20, 1),
    arm = "A",
    probs = rep(list(c(A = 1, B = 0)), 20),
    phase = "quota"
  )

  metrics <- trial_metrics(trial, design)
  value <- function(point, metric) {
    metrics$value[metrics$point == point & metrics$metric == metric]
  }

  expect_equal(value("run-in", "size_dev_A"), 5)
  expect_equal(value("run-in", "size_dev_B"), 5)
  expect_equal(value("end", "size_dev_A"), 10)
  expect_equal(value("end", "size_dev_B"), 10)
  expect_true(is.na(value("end", "smd_max")))
  expect_equal(value("all", "pct_deterministic"), 100)
  expect_equal(value("all", "longest_forced_run"), 20)
})

test_that("trial_metrics scales the guess and the deterministic share to percentages", {
  design <- small_design_2arm()
  stream <- make_stream(20, 1)
  trial <- tibble::tibble(
    seq = 1:20,
    stream,
    arm = rep(c("A", "B"), 10),
    probs = rep(list(c(A = 0.5, B = 0.5)), 20),
    phase = "msb-random"
  )

  metrics <- trial_metrics(trial, design)
  value <- function(metric) metrics$value[metrics$metric == metric]

  expect_equal(value("pct_deterministic"), 0)
  expect_equal(value("longest_forced_run"), 0)
  expect_equal(value("correct_guess"), 50)
})
