restore_session_state <- function() {
  if (exists("setup_options", envir = globalenv())) {
    options(get("setup_options", envir = globalenv()))
  }
  if (exists("previous_passphrase", envir = globalenv())) {
    previous <- get("previous_passphrase", envir = globalenv())
    if (is.na(previous)) {
      Sys.unsetenv("GRACE_PASSPHRASE")
    } else {
      Sys.setenv(GRACE_PASSPHRASE = previous)
    }
  }
}

run_all <- function() {
  on.exit(restore_session_state(), add = TRUE)
  scripts <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
  for (script in scripts) {
    source(script, echo = TRUE, max.deparse.length = Inf)
  }
}

run_all()
