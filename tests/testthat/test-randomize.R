codes_path <- function() {
  test_path("fixtures", "codes_small.csv")
}

passphrase <- function() {
  "test-pass"
}

test_that("randomize saves a record and audit row", {
  path <- new_test_trial(codes = codes_path())

  assignment <- randomize(path, "P001", example_values())
  trial <- read_trial(path, passphrase())

  expect_identical(nrow(trial$records), 1L)
  expect_identical(trial$records$id, "P001")
  expect_identical(trial$records$arm, assignment$arm)
  expect_identical(trial$records$code, assignment$code)
  expect_identical(trial$records$seq, 1L)
  expect_identical(trial$records$sex, "male")
  expect_identical(trial$records$age, 50)
  expect_identical(trial$records$phase, assignment$phase)
  expect_identical(trial$records$probs[[1]], assignment$probs)
  expect_identical(attr(trial$records$time, "tzone"), "UTC")
  used <- trial$codes[trial$codes$code == assignment$code, ]
  expect_identical(used$status, "used")
  expect_identical(used$arm, assignment$arm)
  expect_identical(used$id, "P001")
  expect_identical(used$drawn_event, trial$records$event)
  expect_identical(trial$audit$action, c("create", "randomize"))
  expect_identical(trial$audit$event[[2]], trial$records$event[[1]])
  expect_identical(trial$audit$id[[2]], "P001")
  expect_identical(
    trial$audit$new[[2]],
    sprintf("arm=%s; code=%s", assignment$arm, assignment$code)
  )
})

test_that("randomize returns a grace_assignment", {
  path <- new_test_trial()

  assignment <- randomize(path, "P001", example_values())

  expect_s3_class(assignment, "grace_assignment")
  expect_identical(
    names(assignment),
    c("id", "arm", "code", "probs", "phase", "n_randomized", "status")
  )
  expect_identical(assignment$id, "P001")
  expect_true(assignment$arm %in% c("A", "B"))
  expect_identical(assignment$code, NA_character_)
  expect_equal(sum(assignment$probs), 1)
  expect_identical(assignment$n_randomized, 1L)
})

test_that("randomize refuses a duplicate id and a full trial", {
  path <- new_test_trial()
  stream <- make_stream(20, 1)
  randomize(path, "P001", example_values())

  expect_error(randomize(path, "P001", example_values()), class = "grace_error_randomize")
  for (j in 2:20) {
    randomize(path, sprintf("P%03d", j), as.list(stream[j, ]))
  }
  expect_error(randomize(path, "P021", example_values()), class = "grace_error_randomize")
  expect_identical(nrow(read_trial(path, passphrase())$records), 20L)
})

test_that("randomize refuses an id that is not a single non-empty string", {
  path <- new_test_trial()

  for (bad in list("", NA_character_, 5, c("a", "b"), NULL, character(0))) {
    expect_error(randomize(path, bad, example_values()), class = "grace_error_randomize")
  }
  expect_identical(nrow(read_trial(path, passphrase())$records), 0L)
})

test_that("missing values warn but still randomize", {
  path <- new_test_trial()

  expect_warning(
    randomize(path, "P001", example_values(sex = NA, age = NA)),
    class = "grace_warning_missing"
  )
  trial <- read_trial(path, passphrase())

  expect_identical(nrow(trial$records), 1L)
  expect_identical(trial$records$sex, NA_character_)
  expect_identical(trial$records$age, NA_real_)
})

test_that("unknown level errors and saves nothing", {
  path <- new_test_trial(codes = codes_path())

  expect_error(
    randomize(path, "P001", example_values(sex = "unknown")),
    class = "grace_error_level"
  )
  trial <- read_trial(path, passphrase())

  expect_identical(nrow(trial$records), 0L)
  expect_identical(nrow(trial$audit), 1L)
  expect_true(all(trial$codes$status == "unused"))
})

test_that("running out of codes errors and saves nothing", {
  one_per_arm <- tibble::tibble(
    code = c("1", "2"),
    arm = c("A", "B"),
    status = "unused",
    id = NA_character_,
    drawn_event = NA_integer_
  )
  path <- new_test_trial(codes = one_per_arm)
  stream <- make_stream(6, 3)
  saved <- character(0)
  failure <- NULL

  for (j in 1:6) {
    id <- sprintf("P%03d", j)
    result <- tryCatch(
      randomize(path, id, as.list(stream[j, ])),
      error = identity
    )
    if (inherits(result, "error")) {
      failure <- result
      failed_id <- id
      break
    }
    saved <- c(saved, id)
  }
  trial <- read_trial(path, passphrase())

  expect_s3_class(failure, "grace_error_no_codes")
  expect_identical(trial$records$id, saved)
  expect_false(failed_id %in% trial$records$id)
  expect_identical(nrow(trial$audit), 1L + length(saved))
  expect_identical(sum(trial$codes$status == "used"), length(saved))
})

test_that("randomize does not change the user's seed", {
  path <- new_test_trial(codes = codes_path())
  set.seed(99)
  seed_before <- .Random.seed
  kind_before <- RNGkind()

  randomize(path, "P001", example_values())

  expect_identical(.Random.seed, seed_before)
  expect_identical(RNGkind(), kind_before)
})

test_that("randomize works when the user has no seed yet", {
  path <- new_test_trial()
  withr::defer(set.seed(1))
  if (exists(".Random.seed", envir = globalenv())) {
    rm(".Random.seed", envir = globalenv())
  }

  randomize(path, "P001", example_values())

  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("randomize is reproducible for the same trial seed", {
  first <- new_test_trial(codes = codes_path(), seed = 42)
  second <- new_test_trial(codes = codes_path(), seed = 42)
  stream <- make_stream(10, 5)

  randomize_stream(first, stream)
  randomize_stream(second, stream)

  expect_identical(
    read_trial(first, passphrase())$records[c("arm", "code", "phase")],
    read_trial(second, passphrase())$records[c("arm", "code", "phase")]
  )
})

test_that("20 randomizations hit the run-in and final targets", {
  path <- new_test_trial(codes = codes_path())
  design <- small_design_2arm()

  randomize_stream(path, make_stream(20, 1))
  records <- read_trial(path, passphrase())$records

  for (row in seq_len(nrow(design$checkpoints))) {
    n <- design$checkpoints$n[[row]]
    counts <- table(factor(records$arm[seq_len(n)], levels = design$arms))
    expect_identical(as.integer(counts), unname(design$checkpoints$target[[row]]))
  }
  expect_identical(records$seq, 1:20)
  expect_identical(records$event, 2:21)
  expect_identical(length(unique(records$code)), 20L)
  expect_true(all(records$phase %in% c("runin", "quota", "bound", "msb-biased", "msb-random")))
})
