encrypt_object <- function(x, passphrase) {
  salt <- sodium::random(32)
  nonce <- sodium::random(24)
  key <- derive_key(passphrase, salt)
  ciphertext <- sodium::data_encrypt(serialize(x, NULL), key, nonce)
  list(format = "grace-enc-1", salt = salt, nonce = nonce, ciphertext = ciphertext)
}

decrypt_object <- function(envelope, passphrase) {
  decrypt_failure <- function(...) {
    cli::cli_abort(
      "Could not decrypt the trial file. The passphrase is wrong or the file is damaged.",
      class = "grace_error_decrypt"
    )
  }
  if (!identical(envelope$format, "grace-enc-1")) {
    decrypt_failure()
  }
  plain <- tryCatch(
    {
      key <- derive_key(passphrase, envelope$salt)
      sodium::data_decrypt(envelope$ciphertext, key, envelope$nonce)
    },
    error = decrypt_failure
  )
  unserialize(plain)
}

derive_key <- function(passphrase, salt) {
  sodium::scrypt(charToRaw(enc2utf8(passphrase)), salt = salt, size = 32)
}

#' Get the trial file passphrase
#'
#' Reads the passphrase from the `GRACE_PASSPHRASE` environment variable. If it is
#' not set and the session is interactive, prompts for it. The passphrase is never
#' printed.
#'
#' @return The passphrase as a single string.
#' @export
#' @examples
#' withr::with_envvar(
#'   c(GRACE_PASSPHRASE = "made-up passphrase"),
#'   trial_passphrase()
#' )
trial_passphrase <- function() {
  passphrase <- Sys.getenv("GRACE_PASSPHRASE")
  if (nzchar(passphrase)) {
    return(passphrase)
  }
  if (rlang::is_interactive()) {
    passphrase <- askpass::askpass("Trial file passphrase: ")
  }
  if (is.null(passphrase) || !nzchar(passphrase)) {
    cli::cli_abort(
      c(
        "No trial file passphrase found.",
        "i" = "Set the {.envvar GRACE_PASSPHRASE} environment variable."
      ),
      class = "grace_error_passphrase"
    )
  }
  passphrase
}
