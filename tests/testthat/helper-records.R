make_records <- function(design, df) {
  n <- nrow(df)
  if (n == 0) {
    return(empty_records(design))
  }
  fixed <- tibble::tibble(
    seq = seq_len(n),
    id = as.character(df$id)
  )
  variables <- tibble::as_tibble(df[names(design$balance_vars)])
  rest <- tibble::tibble(
    arm = df$arm,
    code = rep(NA_character_, n),
    probs = rep(list(NULL), n),
    phase = rep("test", n),
    rng_state = rep(list(NULL), n),
    event = rep(NA_integer_, n),
    time = rep(as.POSIXct(NA, tz = "UTC"), n)
  )
  dplyr::bind_cols(fixed, variables, rest)
}

eight_records <- function() {
  tibble::tibble(
    id = as.character(1:8),
    arm = c("A", "A", "A", "A", "A", "B", "B", "B"),
    sex = c("male", "female", "male", "male", "female", "female", "male", "female"),
    race = c("white", "black", "white", "other", "white", "white", "black", NA),
    age = c(30, 40, 50, NA, 60, 35, 45, 55)
  )
}
