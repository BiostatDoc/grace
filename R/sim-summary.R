mean_or_missing <- function(values) {
  if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
}

quantile_or_missing <- function(values, probability) {
  if (all(is.na(values))) {
    return(NA_real_)
  }
  stats::quantile(values, probability, type = 7, na.rm = TRUE, names = FALSE)
}

#' Summarize a simulation
#'
#' Summarizes each metric over the simulated trials, separately for every
#' method and point: the mean, standard deviation, the 95th and 99th percentiles
#' (type 7), and the maximum. The upper percentiles and the maximum show the
#' worst cases. Missing values are left out, and a group with no values gives
#' `NA`.
#'
#' @param object A `grace_sim` from [simulate_trials()].
#' @param ... Not used.
#'
#' @return A tibble of class `grace_sim_summary` with one row per `method`,
#'   `point`, and `metric`, and columns `mean`, `sd`, `q95`, `q99`, and `max`.
#' @export
#' @examples
#' result <- simulate_trials(example_design(40), methods = c("srs", "mmsb"), n_sims = 3, seed = 1)
#' summary(result)
summary.grace_sim <- function(object, ...) {
  plain <- tibble::as_tibble(as.data.frame(object))
  summarized <- dplyr::summarise(
    plain,
    mean = mean_or_missing(.data$value),
    sd = stats::sd(.data$value, na.rm = TRUE),
    q95 = quantile_or_missing(.data$value, 0.95),
    q99 = quantile_or_missing(.data$value, 0.99),
    max = largest_or_missing(.data$value),
    .by = c("method", "point", "metric")
  )
  structure(summarized, class = c("grace_sim_summary", class(summarized)))
}

#' @rdname summary.grace_sim
#' @param x A `grace_sim` from [simulate_trials()].
#' @export
print.grace_sim <- function(x, ...) {
  methods <- attr(x, "methods") %||% unique(x$method)
  n_sims <- attr(x, "n_sims") %||% length(unique(x$sim))
  seed <- attr(x, "seed") %||% NA
  cli::cli_h1("GRACE simulation")
  cli::cli_bullets(c(
    "*" = paste0("Methods: ", paste(methods, collapse = ", ")),
    "*" = paste0("Simulations: ", n_sims),
    "*" = paste0("Seed: ", seed)
  ))
  cli::cli_text("Mean largest SMD at the end, and mean correct-guess percentage:")
  print_table(headline_table(x, methods))
  invisible(x)
}

headline_table <- function(x, methods) {
  headline <- function(method, point, metric) {
    keep <- x$method == method & x$point == point & x$metric == metric
    mean_or_missing(x$value[keep])
  }
  rows <- lapply(methods, \(method) {
    data.frame(
      method = method,
      smd_max = round(headline(method, "end", "smd_max"), 3),
      correct_guess = round(headline(method, "all", "correct_guess"), 1)
    )
  })
  do.call(rbind, rows)
}
