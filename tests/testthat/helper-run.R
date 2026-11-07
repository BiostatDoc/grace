run_mmsb_loop <- function(design, stream, seed) {
  withr::with_seed(seed, {
    st <- new_state(design)
    arms <- character(nrow(stream))
    for (j in seq_len(nrow(stream))) {
      x <- clean_values(as.list(stream[j, ]), design)
      p <- mmsb_probs(st, x, design)$probs
      arms[j] <- names(p)[sample.int(length(p), 1, prob = p)]
      st <- update_state(st, x, arms[j], design)
    }
    arms
  })
}

make_stream <- function(n, seed) {
  withr::with_seed(seed, tibble::tibble(
    sex = sample(c("male", "female"), n, replace = TRUE),
    race = sample(c("white", "black", "other"), n, replace = TRUE),
    age = round(rnorm(n, 50, 10), 1)
  ))
}
