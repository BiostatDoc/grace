is_forced_step <- function(probs) {
  any(probs >= 1 - 1e-12)
}

correct_guess <- function(trial, design) {
  arms <- design$arms
  r <- design$r[arms]
  n_steps <- nrow(trial)
  assigned <- vapply(arms, \(arm) as.numeric(trial$arm == arm), numeric(n_steps))
  before <- rbind(0, apply(assigned, 2, cumsum))[seq_len(n_steps), , drop = FALSE]
  deficit <- before - outer(seq_len(n_steps) - 1, r)
  guessed <- abs(deficit - apply(deficit, 1, min)) < 1e-9
  probs <- do.call(rbind, lapply(trial$probs, \(p) p[arms]))
  mean(rowSums(guessed * probs) / rowSums(guessed))
}

longest_forced_run <- function(trial) {
  forced <- vapply(trial$probs, is_forced_step, logical(1))
  if (!any(forced)) {
    return(0L)
  }
  key <- ifelse(forced, trial$arm, paste0("free-", seq_along(forced)))
  runs <- rle(key)
  max(runs$lengths[!startsWith(runs$values, "free-")])
}

trial_points <- function(design) {
  points <- list()
  if (design$runin_n > 0) {
    points[["run-in"]] <- design$runin_n
  }
  for (k in design$forced_at) {
    points[[paste0("n=", k)]] <- k
  }
  points[["end"]] <- design$n_total
  points
}

largest_or_missing <- function(values) {
  if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
}

point_metrics <- function(trial, design, k) {
  first <- trial[seq_len(k), ]
  smd <- smd_by_var(first, design)
  counts <- vapply(design$arms, \(arm) sum(first$arm == arm), numeric(1))
  targets <- arm_targets(k, design$r)
  values <- c(
    smd_max = largest_or_missing(smd$smd),
    stats::setNames(smd$smd, paste0("smd_", smd$variable)),
    stats::setNames(abs(counts - targets[design$arms]), paste0("size_dev_", design$arms))
  )
  tibble::tibble(metric = names(values), value = unname(values))
}

overall_metrics <- function(trial, design) {
  forced <- vapply(trial$probs, is_forced_step, logical(1))
  tibble::tibble(
    metric = c("pct_deterministic", "longest_forced_run", "correct_guess"),
    value = c(
      100 * mean(forced),
      as.numeric(longest_forced_run(trial)),
      100 * correct_guess(trial, design)
    )
  )
}

trial_metrics <- function(trial, design) {
  points <- trial_points(design)
  by_point <- lapply(
    names(points),
    \(point) tibble::tibble(point = point, point_metrics(trial, design, points[[point]]))
  )
  all_point <- tibble::tibble(point = "all", overall_metrics(trial, design))
  do.call(rbind, c(by_point, list(all_point)))
}
