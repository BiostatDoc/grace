if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pak(c(
  "ggplot2@4.0.3",
  "knitr@1.52",
  "cli@3.6.6",
  "dplyr@1.2.1",
  "purrr@1.2.2",
  "rlang@1.3.0",
  "sodium@1.4.0",
  "tibble@3.3.1",
  "askpass@1.2.1",
  "furrr@0.4.0",
  "future@1.82.0",
  "parallelly@1.50.0"
))
pak::pak("BiostatDoc/graceDR")

library(graceDR)
library(ggplot2)
library(knitr)

options(
  cli.unicode = FALSE,
  cli.num_colors = 1,
  cli.width = 70,
  width = 70,
  pillar.width = 70,
  pillar.bold = FALSE
)

results_directory <- "JSSarticle/results"
cells <- c("N200_iid", "N200_drift", "N500_iid", "N500_drift")
result_files <- file.path(results_directory, paste0("sim_", cells, ".rds"))
if (!all(file.exists(result_files, file.path(results_directory, "settings.rds")))) {
  system2(file.path(R.home("bin"), "Rscript"), "JSSarticle/run_simulations.R")
}
settings <- readRDS(file.path(results_directory, "settings.rds"))
sims <- lapply(
  stats::setNames(cells, cells),
  \(cell) readRDS(file.path(results_directory, paste0("sim_", cell, ".rds")))
)

method_order <- c("srs", "blocks", "minimization", "msb", "mmsb")
method_label <- c(
  srs = "Simple randomization", blocks = "Permuted blocks", minimization = "Minimization",
  msb = "MSB", mmsb = "Modified MSB"
)
cell_label <- c(
  N200_iid = "N = 200, iid", N200_drift = "N = 200, drift",
  N500_iid = "N = 500, iid", N500_drift = "N = 500, drift"
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

code <- function(...) {
  paste0("\\code{", gsub("_", "\\_", c(...), fixed = TRUE), "}", collapse = ", ")
}

f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
f3 <- function(x) formatC(x, format = "f", digits = 3)
n_sims <- settings$n_sims

remove_latex <- function(x) {
  x |>
    gsub("\\\\code\\{([^{}]*)\\}", "\\1", x = _) |>
    gsub("\\_", "_", x = _, fixed = TRUE) |>
    gsub("\\%", "%", x = _, fixed = TRUE) |>
    gsub("\\ast", "*", x = _, fixed = TRUE) |>
    gsub("$", "", x = _, fixed = TRUE)
}
show_table <- function(data, align) {
  data[] <- lapply(data, remove_latex)
  names(data) <- remove_latex(names(data))
  print(knitr::kable(data, format = "pipe", align = align, row.names = FALSE))
}

arguments <- data.frame(
  Argument = c(
    r"(\code{n\_total})", r"(\code{balance\_vars})", r"(\code{arms})", r"(\code{ratio})",
    r"(\code{weights})", r"(\code{p\_star})", r"(\code{xi})", r"(\code{runin\_n})",
    r"(\code{forced\_at})", r"(\code{size\_bound})", r"(\code{runin\_method})"
  ),
  Default = c(
    "(required)", "(required)", r"(\code{c("A", "B")})", r"(\code{c(1, 1)})", "equal",
    "0.30", "0.65", "default rule", "none", "3", r"(\code{"tilted"})"
  ),
  Meaning = c(
    "Total planned enrollment $N$",
    r"(Named list made with \code{categorical\_var()} and \code{continuous\_var()})",
    "Arm labels, at least two",
    "Allocation ratio, any positive numbers",
    "Positive weights of the variables, rescaled to sum to one",
    r"(Imbalance $p$-value limit $p^\ast$)",
    r"(Biased coin probability $\xi$)",
    "Run-in size; 0 removes the run-in",
    "Enrollment totals at which the target counts are forced",
    "Big stick bound $B$",
    r"(Run-in with the tilt, or the plain random allocation rule (\code{"rar"}))"
  )
)
show_table(arguments, c("l", "l", "l"))

functions <- data.frame(
  Function = c(
    code("grace_design()"),
    code("categorical_var()", "continuous_var()"),
    code("runin_size()", "arm_targets()"),
    code("create_trial()"),
    code("read_codes()"),
    code("randomize()"),
    code("trial_status()"),
    code("group_size_table()", "balance_table()", "smd_by_var()"),
    code("imbalance_plot()", "smd_plot()"),
    code("override_assignment()"),
    code("audit_log()"),
    code("replay_assignment()"),
    code("trial_passphrase()"),
    code("simulate_trials()"),
    code("summary()", "print()"),
    code("design_generator()"),
    code("example_design()", "example_generator_iid()", "example_generator_drift()")
  ),
  Purpose = c(
    "Create the design object",
    "Describe balance variables",
    "Run-in size rule and whole-number target counts",
    "Create the encrypted trial file",
    "Read a file of treatment codes",
    "Randomize one participant and save the record",
    "Group sizes, balance tables, standardized differences, and plots",
    "The tables behind the status report",
    "The two plots of the status report",
    "Change an assignment with a recorded reason",
    "Read the append-only audit log",
    "Redraw a past assignment and compare it with the record",
    "Read the passphrase from the environment",
    "Simulate trials under up to five allocation methods",
    "Summaries and printing of a simulation",
    "Synthetic participant streams with optional drift",
    "A small design and two generators for examples"
  )
)
show_table(functions, c("l", "l"))

pdf("article-figures.pdf", width = 6.3, height = 5.6)

design <- example_design(40)
design

design_three <- grace_design(
  n_total = 120,
  balance_vars = list(
    sex = categorical_var(c("male", "female")),
    age = continuous_var()
  ),
  arms = c("A", "B", "C"),
  ratio = c(2, 1, 1),
  weights = c(sex = 2, age = 1),
  forced_at = c(60, 90)
)
design_three

Sys.setenv(GRACE_PASSPHRASE = "article-only")
path <- tempfile(fileext = ".rds")
codes_file <- system.file("extdata", "example_codes.csv", package = "graceDR")
create_trial(path, design, seed = 20260611, codes = codes_file)
for (j in seq_len(nrow(example_participants))) {
  participant <- example_participants[j, ]
  assignment <- randomize(
    path, participant$id, participant[c("sex", "race", "age")]
  )
}
assignment

status <- trial_status(path)
status
status$balance

status$imbalance_plot
status$smd_plot

new_arm <- setdiff(design$arms, assignment$arm)
invisible(override_assignment(
  path, "P040", new_arm,
  reason = "Wrong race entered in the example."
))
log <- audit_log(path)
log[nrow(log) + -1:0, c("event", "action", "id", "old", "new")]
log$reason[nrow(log)]
replays <- do.call(
  rbind,
  lapply(example_participants$id, \(id) replay_assignment(path, id))
)
all(replays$match)

simulation <- simulate_trials(
  design,
  methods = c("srs", "mmsb"),
  n_sims = 20,
  generator = example_generator_iid(),
  seed = 1
)
simulation

setting_column <- function() unlist(lapply(cells, \(cell) c(cell_label[[cell]], rep("", 4))))
method_column <- function() rep(method_label[method_order], length(cells))
stack <- function(f) unlist(lapply(cells, \(cell) f(S[[cell]])), use.names = FALSE)
with_se <- function(mean, se, digits) {
  paste0(
    formatC(mean, format = "f", digits = digits),
    " (", formatC(se, format = "f", digits = digits), ")"
  )
}
with_max <- function(mean, max, digits) {
  paste0(
    formatC(mean, format = "f", digits = digits),
    " (", formatC(max, format = "f", digits = 0), ")"
  )
}
ci_text <- function(estimate, lower, upper, digits) {
  paste0(
    formatC(estimate, format = "f", digits = digits), " (",
    formatC(lower, format = "f", digits = digits), ", ",
    formatC(upper, format = "f", digits = digits), ")"
  )
}

balance <- data.frame(
  Setting = setting_column(),
  Method = method_column(),
  End = with_se(stack(\(s) s$smd_end$mean), stack(\(s) s$smd_end$se), 3),
  Q95 = formatC(stack(\(s) s$smd_end$q95), format = "f", digits = 3),
  Runin = formatC(stack(\(s) s$smd_runin_mean), format = "f", digits = 3)
)
names(balance) <- c("Setting", "Method", "End (SE)", "End, 95th pct.", "Run-in")
show_table(balance, c("l", "l", "r", "r", "r"))

sizes <- data.frame(
  Setting = setting_column(),
  Method = method_column(),
  Runin = formatC(stack(\(s) s$exact_runin), format = "f", digits = 1),
  End = formatC(stack(\(s) s$exact_end), format = "f", digits = 1),
  Deviation = with_max(stack(\(s) s$deviation_mean), stack(\(s) s$deviation_max), 2)
)
names(sizes) <- c(
  "Setting", "Method", "Exact at run-in (\\%)", "Exact at end (\\%)", "Final difference"
)
show_table(sizes, c("l", "l", "r", "r", "r"))

predictability <- data.frame(
  Setting = setting_column(),
  Method = method_column(),
  Forced = formatC(stack(\(s) s$forced_mean), format = "f", digits = 1),
  Run = with_max(stack(\(s) s$run_mean), stack(\(s) s$run_max), 1),
  Guess = with_se(stack(\(s) s$guess$mean), stack(\(s) s$guess$se), 2)
)
names(predictability) <- c(
  "Setting", "Method", "Forced (\\%)", "Longest forced run", "Correct guess (\\%)"
)
show_table(predictability, c("l", "l", "r", "r", "r"))

paired_rows <- lapply(cells, \(cell) {
  smd_diff <- S[[cell]]$paired_smd
  guess_diff <- S[[cell]]$paired_guess
  data.frame(
    Setting = c(cell_label[[cell]], rep("", nrow(smd_diff) - 1)),
    Method = method_label[smd_diff$method],
    Smd = ci_text(smd_diff$mean, smd_diff$lower, smd_diff$upper, 4),
    Guess = ci_text(guess_diff$mean, guess_diff$lower, guess_diff$upper, 1)
  )
})
paired_table <- do.call(rbind, paired_rows)
names(paired_table) <- c("Setting", "Method", "Largest SMD at end", "Correct guess (\\%)")
show_table(paired_table, c("l", "l", "r", "r"))

symbols <- c(srs = 1, blocks = 2, minimization = 15, msb = 4, mmsb = 19)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.5), cex.main = 0.9)
for (cell in cells) {
  x <- S[[cell]]$guess$mean
  y <- S[[cell]]$smd_end$mean
  plot(
    x, y,
    pch = symbols[method_order], cex = 1.3, xlim = c(46, 76),
    ylim = c(0, max(y) * 1.2), xlab = "Correct-guess probability (%)",
    ylab = "Mean largest SMD at end", main = cell_label[[cell]], las = 1, bty = "l"
  )
  label_position <- c(srs = 4, blocks = 1, minimization = 4, msb = 1, mmsb = 3)
  text(x, y,
    labels = method_label[method_order], pos = label_position[method_order],
    cex = 0.6, offset = 0.6
  )
}
par(op)

invisible(dev.off())
