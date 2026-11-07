trial_path <- function() {
  file.path(withr::local_tempdir(.local_envir = parent.frame()), "trial.rds")
}

test_that("create_trial then read_trial round-trips", {
  withr::local_envvar(GRACE_PASSPHRASE = "test-pass")
  path <- trial_path()
  design <- small_design_2arm()

  result <- create_trial(path, design, seed = 123)
  trial <- read_trial(path, trial_passphrase())

  expect_identical(result, path)
  expect_s3_class(trial, "grace_trial")
  expect_identical(trial$format_version, 1L)
  expect_identical(trial$package_version, as.character(utils::packageVersion("graceDR")))
  expect_identical(trial$design, design)
  expect_identical(trial$seed, 123L)
  expect_identical(trial$rng, new_trial_rng(123L))
  expect_null(trial$codes)
  expect_identical(nrow(trial$records), 0L)
  expect_identical(
    vapply(trial$records, \(column) class(column)[1], character(1)),
    c(
      seq = "integer", id = "character", sex = "character", race = "character",
      age = "numeric", arm = "character", code = "character", probs = "list",
      phase = "character", rng_state = "list", event = "integer", time = "POSIXct"
    )
  )
  expect_identical(attr(trial$records$time, "tzone"), "UTC")
  expect_identical(nrow(trial$audit), 1L)
  expect_identical(trial$audit$action, "create")
  expect_identical(trial$audit$event, 1L)
})

test_that("create_trial stores codes from a path or a tibble", {
  withr::local_envvar(GRACE_PASSPHRASE = "test-pass")
  design <- small_design_2arm()
  codes_path <- test_path("fixtures", "codes_small.csv")
  from_path <- trial_path()
  from_tibble <- trial_path()

  create_trial(from_path, design, seed = 1, codes = codes_path)
  create_trial(from_tibble, design, seed = 1, codes = read_codes(codes_path, design$arms))

  expect_identical(nrow(read_trial(from_path, trial_passphrase())$codes), 30L)
  expect_identical(
    read_trial(from_path, trial_passphrase())$codes,
    read_trial(from_tibble, trial_passphrase())$codes
  )
})

test_that("create_trial refuses an existing path", {
  withr::local_envvar(GRACE_PASSPHRASE = "test-pass")
  path <- trial_path()
  file.create(path)

  expect_error(create_trial(path, small_design_2arm(), seed = 1), class = "grace_error_trial")
})

test_that("create_trial refuses a design that is not a grace_design", {
  withr::local_envvar(GRACE_PASSPHRASE = "test-pass")
  path <- trial_path()

  expect_error(create_trial(path, list(), seed = 1), class = "grace_error_trial")
  expect_false(file.exists(path))
})

test_that("write_trial keeps the previous version as .bak", {
  path <- trial_path()
  first <- structure(list(version = "first"), class = "grace_trial")
  second <- structure(list(version = "second"), class = "grace_trial")

  write_trial(first, path, "test-pass")
  write_trial(second, path, "test-pass")

  expect_identical(read_trial(path, "test-pass")$version, "second")
  expect_identical(read_trial(paste0(path, ".bak"), "test-pass")$version, "first")
  expect_false(file.exists(paste0(path, ".tmp")))
})

test_that("read_trial errors on a missing file and wrong passphrase", {
  withr::local_envvar(GRACE_PASSPHRASE = "test-pass")
  path <- trial_path()
  create_trial(path, small_design_2arm(), seed = 1)

  expect_error(read_trial(trial_path(), "test-pass"), class = "grace_error_trial")
  expect_error(read_trial(path, "wrong-pass"), class = "grace_error_decrypt")
})

test_that("read_trial errors when the file is not a trial", {
  path <- trial_path()
  saveRDS(encrypt_object(list(a = 1), "test-pass"), path)

  expect_error(read_trial(path, "test-pass"), class = "grace_error_trial")
})

test_that("audit_log returns the audit table", {
  withr::local_envvar(GRACE_PASSPHRASE = "test-pass")
  path <- trial_path()
  create_trial(path, small_design_2arm(), seed = 1)

  log <- audit_log(path)

  expect_identical(log$action, "create")
  expect_identical(log, read_trial(path, trial_passphrase())$audit)
})
