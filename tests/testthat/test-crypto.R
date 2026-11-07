test_that("encrypt then decrypt returns the same object", {
  x <- list(
    data = tibble::tibble(id = c("a", "b"), value = c(1.5, 2.5)),
    note = "made up"
  )

  envelope <- encrypt_object(x, "correct horse")
  result <- decrypt_object(envelope, "correct horse")

  expect_identical(envelope$format, "grace-enc-1")
  expect_identical(result, x)
})

test_that("a wrong passphrase errors", {
  envelope <- encrypt_object(list(a = 1), "right")

  expect_error(decrypt_object(envelope, "wrong"), class = "grace_error_decrypt")
})

test_that("changed ciphertext errors", {
  envelope <- encrypt_object(list(a = 1), "right")
  envelope$ciphertext[1] <- as.raw(bitwXor(as.integer(envelope$ciphertext[1]), 1L))

  expect_error(decrypt_object(envelope, "right"), class = "grace_error_decrypt")
})

test_that("an unknown format errors", {
  envelope <- encrypt_object(list(a = 1), "right")
  envelope$format <- "other"

  expect_error(decrypt_object(envelope, "right"), class = "grace_error_decrypt")
})

test_that("two encryptions use different salts and nonces", {
  first <- encrypt_object(list(a = 1), "right")
  second <- encrypt_object(list(a = 1), "right")

  expect_false(identical(first$salt, second$salt))
  expect_false(identical(first$nonce, second$nonce))
})

test_that("encrypt_object does not disturb the global seed", {
  set.seed(1)
  before <- get(".Random.seed", envir = globalenv())

  encrypt_object(list(a = 1), "right")

  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("trial_passphrase reads the environment and errors when absent", {
  rlang::local_interactive(FALSE)

  withr::with_envvar(
    c(GRACE_PASSPHRASE = "from env"),
    expect_identical(trial_passphrase(), "from env")
  )
  withr::with_envvar(
    c(GRACE_PASSPHRASE = ""),
    expect_error(trial_passphrase(), class = "grace_error_passphrase")
  )
})
