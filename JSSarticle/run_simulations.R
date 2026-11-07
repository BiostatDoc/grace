library(graceDR)

script_directory <- function() {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_argument) == 0) {
    return(getwd())
  }
  dirname(normalizePath(sub("^--file=", "", file_argument[[1]])))
}

settings <- list(
  sample_sizes = c(200, 500),
  scenarios = c("iid", "drift"),
  methods = c("mmsb", "srs", "blocks", "minimization", "msb"),
  n_sims = 1000,
  seed = 20260611
)

generators <- list(
  iid = example_generator_iid(),
  drift = example_generator_drift()
)

use_parallel <- all(vapply(c("furrr", "future"), requireNamespace, logical(1), quietly = TRUE))
if (use_parallel) {
  future::plan(future::multisession, workers = max(1, min(6, parallelly::availableCores() - 1)))
}

results_directory <- file.path(script_directory(), "results")
dir.create(results_directory, showWarnings = FALSE)

for (n_total in settings$sample_sizes) {
  for (scenario in settings$scenarios) {
    label <- sprintf("N%d_%s", n_total, scenario)
    cat("Running", label, "\n")
    simulation <- simulate_trials(
      example_design(n_total),
      methods = settings$methods,
      n_sims = settings$n_sims,
      generator = generators[[scenario]],
      seed = settings$seed,
      parallel = use_parallel
    )
    saveRDS(simulation, file.path(results_directory, paste0("sim_", label, ".rds")))
    saveRDS(summary(simulation), file.path(results_directory, paste0("summary_", label, ".rds")))
  }
}

if (use_parallel) {
  future::plan(future::sequential)
}

settings$package_version <- as.character(utils::packageVersion("graceDR"))
settings$r_version <- R.version.string
settings$parallel <- use_parallel
saveRDS(settings, file.path(results_directory, "settings.rds"))
