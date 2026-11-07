passphrase <- function() {
  "test-pass"
}

randomized_trial <- function(
  codes = test_path("fixtures", "codes_small.csv"),
  seed = 7,
  env = parent.frame()
) {
  path <- new_test_trial(codes = codes, seed = seed, env = env)
  randomize_stream(path, make_stream(20, 2))
  path
}

test_that("replay reproduces every assignment", {
  path <- randomized_trial()
  ids <- read_trial(path, passphrase())$records$id

  results <- lapply(ids, \(id) replay_assignment(path, id))

  for (result in results) {
    expect_true(result$match)
    expect_identical(result$arm, result$arm_replayed)
    expect_identical(result$code, result$code_replayed)
  }
})

test_that("replay returns a one-row tibble with the documented columns", {
  path <- randomized_trial()

  result <- replay_assignment(path, "P003")

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 1L)
  expect_identical(
    names(result),
    c("id", "arm", "arm_replayed", "code", "code_replayed", "match")
  )
  expect_identical(result$id, "P003")
  expect_type(result$match, "logical")
})

test_that("replay works without treatment codes", {
  path <- randomized_trial(codes = NULL)

  results <- lapply(sprintf("P%03d", 1:20), \(id) replay_assignment(path, id))

  expect_true(all(vapply(results, \(result) result$match, logical(1))))
  expect_true(all(is.na(vapply(results, \(result) result$code, character(1)))))
})

test_that("replay still matches after a code was voided", {
  path <- randomized_trial()
  trial <- read_trial(path, passphrase())
  trial$codes <- void_code(trial$codes, trial$records$code[[4]])
  write_trial(trial, path, passphrase())

  results <- lapply(trial$records$id, \(id) replay_assignment(path, id))

  expect_true(all(vapply(results, \(result) result$match, logical(1))))
})

test_that("replay does not change the user's seed", {
  path <- randomized_trial()
  set.seed(5)
  seed_before <- .Random.seed

  replay_assignment(path, "P001")

  expect_identical(.Random.seed, seed_before)
})

test_that("replay flags a record that was changed without an override", {
  path <- randomized_trial()
  trial <- read_trial(path, passphrase())
  original <- trial$records$arm[[1]]
  trial$records$arm[[1]] <- setdiff(c("A", "B"), original)
  write_trial(trial, path, passphrase())

  result <- replay_assignment(path, "P001")

  expect_false(result$match)
  expect_identical(result$arm, setdiff(c("A", "B"), original))
  expect_identical(result$arm_replayed, original)
})

test_that("replay compares against the original values after an override", {
  path <- randomized_trial()
  trial <- read_trial(path, passphrase())
  original_arm <- trial$records$arm[[1]]
  original_code <- trial$records$code[[1]]
  trial$records$arm[[1]] <- setdiff(c("A", "B"), original_arm)
  trial$records$code[[1]] <- "99999"
  trial$audit <- append_audit(
    trial$audit,
    "override",
    id = "P001",
    old = sprintf("arm=%s; code=%s", original_arm, original_code),
    new = sprintf("arm=%s; code=%s", trial$records$arm[[1]], "99999"),
    reason = "test"
  )
  write_trial(trial, path, passphrase())

  result <- replay_assignment(path, "P001")

  expect_true(result$match)
  expect_identical(result$arm, original_arm)
  expect_identical(result$code, original_code)
})

test_that("replay errors on an unknown id", {
  path <- randomized_trial()

  expect_error(replay_assignment(path, "P999"), class = "grace_error_replay")
})
