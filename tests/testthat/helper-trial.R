new_test_trial <- function(
  design = small_design_2arm(),
  codes = NULL,
  seed = 1,
  env = parent.frame()
) {
  withr::local_envvar(GRACE_PASSPHRASE = "test-pass", .local_envir = env)
  directory <- withr::local_tempdir(.local_envir = env)
  path <- file.path(directory, "trial.rds")
  create_trial(path, design, seed = seed, codes = codes)
  path
}

randomize_stream <- function(path, stream, prefix = "P") {
  lapply(seq_len(nrow(stream)), \(j) {
    randomize(path, sprintf("%s%03d", prefix, j), as.list(stream[j, ]))
  })
}

example_values <- function(sex = "male", race = "white", age = 50) {
  list(sex = sex, race = race, age = age)
}
