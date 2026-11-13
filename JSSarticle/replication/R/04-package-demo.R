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

pdf("figures/status-plots.pdf", width = 4.2, height = 3.2)
status$imbalance_plot
status$smd_plot
invisible(dev.off())

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
