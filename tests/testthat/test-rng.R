test_that("with_trial_rng leaves the global seed unchanged", {
  set.seed(1)
  before <- get(".Random.seed", envir = globalenv())
  rng <- new_trial_rng(42)

  with_trial_rng(rng, runif(3))

  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("with_trial_rng removes .Random.seed if it did not exist", {
  set.seed(1)
  rm(".Random.seed", envir = globalenv())
  rng <- new_trial_rng(42)

  with_trial_rng(rng, runif(3))

  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("new_trial_rng does not touch the global seed", {
  set.seed(1)
  before <- get(".Random.seed", envir = globalenv())

  rng <- new_trial_rng(42)

  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(rng$kind, c("Mersenne-Twister", "Inversion", "Rejection"))
  expect_type(rng$state, "integer")
})

test_that("with_trial_rng restores the user's RNG kind", {
  old_kind <- RNGkind()
  withr::defer(do.call(RNGkind, as.list(old_kind)))
  RNGkind("L'Ecuyer-CMRG")
  set.seed(1)
  before <- get(".Random.seed", envir = globalenv())
  rng <- new_trial_rng(42)

  with_trial_rng(rng, runif(3))

  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("with_trial_rng is reproducible and advances", {
  rng <- new_trial_rng(42)

  first <- with_trial_rng(rng, runif(3))
  again <- with_trial_rng(rng, runif(3))
  next_draw <- with_trial_rng(first$rng, runif(3))

  expect_identical(first$value, again$value)
  expect_false(identical(first$value, next_draw$value))
})

test_that("with_trial_rng restores state after an error", {
  set.seed(1)
  before <- get(".Random.seed", envir = globalenv())
  rng <- new_trial_rng(42)

  expect_error(with_trial_rng(rng, stop("boom")), "boom")

  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(RNGkind()[1], "Mersenne-Twister")
})
