passphrase <- function() {
  "test-pass"
}

other_arm <- function(arm) {
  setdiff(c("A", "B"), arm)
}

trial_with_participants <- function(
  n = 5,
  codes = test_path("fixtures", "codes_small.csv"),
  env = parent.frame()
) {
  path <- new_test_trial(codes = codes, seed = 11, env = env)
  randomize_stream(path, make_stream(n, 4))
  path
}

test_that("override changes the arm and logs the reason", {
  path <- trial_with_participants()
  before <- read_trial(path, passphrase())
  old_arm <- before$records$arm[[1]]
  old_code <- before$records$code[[1]]

  updated <- override_assignment(path, "P001", other_arm(old_arm), "entered in error")
  after <- read_trial(path, passphrase())

  expect_identical(after$records$arm[[1]], other_arm(old_arm))
  expect_identical(nrow(after$audit), nrow(before$audit) + 1L)
  new_row <- after$audit[nrow(after$audit), ]
  expect_identical(new_row$action, "override")
  expect_identical(new_row$id, "P001")
  expect_identical(new_row$reason, "entered in error")
  expect_identical(new_row$old, sprintf("arm=%s; code=%s", old_arm, old_code))
  expect_identical(
    new_row$new,
    sprintf("arm=%s; code=%s", other_arm(old_arm), after$records$code[[1]])
  )
  expect_identical(new_row$event, max(before$audit$event) + 1L)
  expect_identical(updated, after$records[1, ])
})

test_that("override returns the updated record invisibly", {
  path <- trial_with_participants()
  arm <- read_trial(path, passphrase())$records$arm[[1]]

  expect_invisible(override_assignment(path, "P001", other_arm(arm), "reason"))
})

test_that("override keeps probs, phase, rng_state, and event", {
  path <- trial_with_participants()
  before <- read_trial(path, passphrase())$records[1, ]

  override_assignment(path, "P001", other_arm(before$arm), "reason")
  after <- read_trial(path, passphrase())$records[1, ]

  for (column in c("seq", "id", "probs", "phase", "rng_state", "event", "time")) {
    expect_identical(after[[column]], before[[column]], info = column)
  }
})

test_that("override voids the old code and draws a new one", {
  path <- trial_with_participants()
  before <- read_trial(path, passphrase())
  old_arm <- before$records$arm[[1]]
  old_code <- before$records$code[[1]]

  override_assignment(path, "P001", other_arm(old_arm), "reason")
  after <- read_trial(path, passphrase())

  new_code <- after$records$code[[1]]
  expect_false(identical(new_code, old_code))
  expect_identical(after$codes$status[after$codes$code == old_code], "voided")
  new_row <- after$codes[after$codes$code == new_code, ]
  expect_identical(new_row$status, "used")
  expect_identical(new_row$arm, other_arm(old_arm))
  expect_identical(new_row$id, "P001")
  expect_identical(new_row$drawn_event, max(before$audit$event) + 1L)
})

test_that("a voided code is never handed out again", {
  path <- trial_with_participants(n = 1)
  trial <- read_trial(path, passphrase())
  old_arm <- trial$records$arm[[1]]
  old_code <- trial$records$code[[1]]

  override_assignment(path, "P001", other_arm(old_arm), "reason")
  randomize_stream(path, make_stream(19, 6), prefix = "Q")
  codes_used <- read_trial(path, passphrase())$records$code

  expect_false(old_code %in% codes_used[-1])
  expect_identical(anyDuplicated(codes_used), 0L)
})

test_that("override works when the trial has no codes", {
  path <- trial_with_participants(codes = NULL)
  arm <- read_trial(path, passphrase())$records$arm[[1]]

  override_assignment(path, "P001", other_arm(arm), "reason")
  trial <- read_trial(path, passphrase())

  expect_identical(trial$records$arm[[1]], other_arm(arm))
  expect_identical(trial$records$code[[1]], NA_character_)
  expect_null(trial$codes)
  expect_identical(
    trial$audit$old[[nrow(trial$audit)]],
    sprintf("arm=%s; code=NA", arm)
  )
})

test_that("override validates its inputs", {
  path <- trial_with_participants()
  current <- read_trial(path, passphrase())$records$arm[[1]]
  cases <- list(
    unknown_id = list(id = "P999", arm = "A", reason = "reason"),
    non_string_id = list(id = 1, arm = "A", reason = "reason"),
    unknown_arm = list(id = "P001", arm = "Z", reason = "reason"),
    non_string_arm = list(id = "P001", arm = NA_character_, reason = "reason"),
    same_arm = list(id = "P001", arm = current, reason = "reason"),
    empty_reason = list(id = "P001", arm = other_arm(current), reason = ""),
    blank_reason = list(id = "P001", arm = other_arm(current), reason = "   "),
    na_reason = list(id = "P001", arm = other_arm(current), reason = NA_character_),
    non_string_reason = list(id = "P001", arm = other_arm(current), reason = 3)
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    expect_error(
      override_assignment(path, case$id, case$arm, case$reason),
      class = "grace_error_override",
      info = name
    )
  }
  expect_error(
    override_assignment(path, "P001", other_arm(current)),
    class = "grace_error_override"
  )
  expect_identical(nrow(read_trial(path, passphrase())$audit), 6L)
})

test_that("running out of codes errors and saves nothing", {
  path <- trial_with_participants(n = 1)
  trial <- read_trial(path, passphrase())
  target <- other_arm(trial$records$arm[[1]])
  trial$codes$status[trial$codes$arm == target & trial$codes$status == "unused"] <- "used"
  write_trial(trial, path, passphrase())
  before <- read_trial(path, passphrase())

  expect_error(
    override_assignment(path, "P001", target, "reason"),
    class = "grace_error_no_codes"
  )
  after <- read_trial(path, passphrase())

  expect_identical(after, before)
})

test_that("override does not change the user's seed", {
  path <- trial_with_participants()
  arm <- read_trial(path, passphrase())$records$arm[[1]]
  set.seed(3)
  seed_before <- .Random.seed

  override_assignment(path, "P001", other_arm(arm), "reason")

  expect_identical(.Random.seed, seed_before)
})

test_that("later randomizations use the overridden arm", {
  path <- trial_with_participants(n = 3)
  trial <- read_trial(path, passphrase())
  arm <- trial$records$arm[[1]]

  override_assignment(path, "P001", other_arm(arm), "reason")
  records <- read_trial(path, passphrase())$records
  state <- state_from_records(records, trial$design)

  expect_identical(
    as.integer(state$n),
    as.integer(table(factor(records$arm, levels = trial$design$arms)))
  )
  expect_identical(records$arm[[1]], other_arm(arm))
})

test_that("replay still matches after an override", {
  path <- trial_with_participants(n = 12)
  trial <- read_trial(path, passphrase())
  old_arm <- trial$records$arm[[3]]
  old_code <- trial$records$code[[3]]

  override_assignment(path, "P003", other_arm(old_arm), "reason")
  results <- lapply(trial$records$id, \(id) replay_assignment(path, id))

  expect_true(all(vapply(results, \(result) result$match, logical(1))))
  overridden <- results[[3]]
  expect_identical(overridden$arm, old_arm)
  expect_identical(overridden$code, old_code)
})
