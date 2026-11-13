settings <- readRDS(file.path(results_directory, "settings.rds"))
sims <- lapply(
  stats::setNames(cells, cells),
  \(cell) readRDS(file.path(results_directory, paste0("sim_", cell, ".rds")))
)

per_sim <- function(sim, point, metric) {
  keep <- sim$point == point & sim$metric == metric
  wide <- tapply(sim$value[keep], list(sim$sim[keep], sim$method[keep]), identity)
  wide[, method_order, drop = FALSE]
}
standard_error <- function(x) stats::sd(x) / sqrt(length(x))
column_se <- function(x) apply(x, 2, standard_error)

summarize_cell <- function(sim) {
  smd_end <- per_sim(sim, "end", "smd_max")
  smd_runin <- per_sim(sim, "run-in", "smd_max")
  guess <- per_sim(sim, "all", "correct_guess")
  forced <- per_sim(sim, "all", "pct_deterministic")
  run <- per_sim(sim, "all", "longest_forced_run")
  deviation_end <- per_sim(sim, "end", "size_dev_A")
  deviation_runin <- per_sim(sim, "run-in", "size_dev_A")
  paired <- function(x) {
    others <- setdiff(method_order, "mmsb")
    d <- x[, others, drop = FALSE] - x[, "mmsb"]
    data.frame(
      method = others, mean = colMeans(d), se = column_se(d),
      lower = colMeans(d) - 1.96 * column_se(d), upper = colMeans(d) + 1.96 * column_se(d)
    )
  }
  list(
    smd_end = data.frame(
      mean = colMeans(smd_end), se = column_se(smd_end),
      q95 = apply(smd_end, 2, stats::quantile, 0.95)
    ),
    smd_runin_mean = colMeans(smd_runin),
    deviation_mean = colMeans(deviation_end), deviation_max = apply(deviation_end, 2, max),
    exact_end = 100 * colMeans(deviation_end == 0),
    exact_runin = 100 * colMeans(deviation_runin == 0),
    guess = data.frame(mean = colMeans(guess), se = column_se(guess)),
    forced_mean = colMeans(forced), forced_max = apply(forced, 2, max),
    run_mean = colMeans(run), run_max = apply(run, 2, max),
    paired_smd = paired(smd_end), paired_guess = paired(guess)
  )
}
S <- lapply(sims, summarize_cell)
