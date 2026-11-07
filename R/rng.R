seed_name <- ".Random.seed"

trial_rng_kind <- function() {
  c("Mersenne-Twister", "Inversion", "Rejection")
}

new_trial_rng <- function(seed) {
  kind <- trial_rng_kind()
  run_with_rng(kind, NULL, set.seed(seed))$rng
}

with_trial_rng <- function(rng, code) {
  run_with_rng(rng$kind, rng$state, code)
}

run_with_rng <- function(kind, state, code) {
  seed_env <- globalenv()
  had_seed <- exists(seed_name, envir = seed_env, inherits = FALSE)
  old_seed <- if (had_seed) get(seed_name, envir = seed_env) else NULL
  old_kind <- RNGkind()
  withr::defer({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(seed_name, old_seed, envir = seed_env)
    } else if (exists(seed_name, envir = seed_env, inherits = FALSE)) {
      rm(list = seed_name, envir = seed_env)
    }
  })
  RNGkind(kind[1], kind[2], kind[3])
  if (!is.null(state)) {
    assign(seed_name, state, envir = seed_env)
  }
  value <- code
  new_state <- get(seed_name, envir = seed_env)
  list(value = value, rng = list(kind = kind, state = new_state))
}
