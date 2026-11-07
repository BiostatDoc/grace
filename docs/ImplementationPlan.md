# grace: Implementation Plan

This plan turns `Specification.md` into an R package named `grace`, a rewritten Journal of
Statistical Software (JSS) article, and two project documents. Each step under
**Build order** becomes one Jira card, written so Sonnet or Haiku can implement it
without seeing the planning conversation.

## 1. Language and framework

- R 4.6 or higher (`Depends: R (>= 4.6.0)`).
- A standard R package that can go to CRAN, built with `devtools`, `usethis`, and
  `roxygen2` (Markdown roxygen on).
- Classes use S3 lists with a `new_*()` constructor and a `validate_*()` function for
  each.
- Runtime dependencies (`Imports`): `cli`, `rlang`, `tibble`, `dplyr`, `purrr`, `tidyr`,
  `ggplot2`, `sodium`, `askpass`, `withr`.
- Optional (`Suggests`): `testthat (>= 3.0.0)`, `vdiffr`, `furrr`, `future`, `knitr`,
  `rmarkdown`.
- Code style: native pipe `|>`, dplyr `.by =` grouping, `join_by()`, `purrr::map_*()`
  (never `sapply`), lines under 100 characters, no comments unless asked for.
- License: GPL-3 (`License: GPL (>= 3)` in `DESCRIPTION`, plus `LICENSE.md`).
- Package environment: `renv` locks the development environment (`renv.lock` is
  committed). Packages are installed with `pak`.

## 2. Project layout

```
grace/
├── DESCRIPTION, NAMESPACE, LICENSE.md, README.md, NEWS.md
├── Makefile                      format, lint, test, check, pre-pr
├── .lintr                        lintr config (line length 100)
├── .Rbuildignore                 keeps non-package files out of the build
├── .gitignore                    includes .env, .claude/worktrees/, renv/library/
├── renv.lock, renv/activate.R, .Rprofile
├── .github/workflows/ci.yml      lint, test, check jobs
├── R/                            one subject per file (see below)
├── man/                          roxygen2 output; never edited by hand
├── tests/testthat.R
├── tests/testthat/               test-<R file name>.R for every file in R/
│   └── fixtures/                 small made-up designs, codes, and trial files
├── inst/extdata/example_codes.csv   example treatment codes shipped with the package
├── data-raw/                     scripts that build inst/extdata and data/
├── data/example_participants.rda    40 made-up participants for examples
├── vignettes/grace.Rmd           walkthrough of setup, randomize, status, simulate
├── docs/ImplementationPlan.md    this plan
├── JSSarticle/                   rewritten JSS manuscript (Quarto, jss format)
│   ├── article.qmd, references.bib
│   ├── run_simulations.R         produces results/*.rds read by the article
│   └── results/
├── ProjectDescription.md
└── ProjectReport.md
```

`.Rbuildignore` lists `^docs$`, `^JSSarticle$`, `^data-raw$`, `^Makefile$`, `^\.lintr$`,
`^renv$`, `^renv\.lock$`, `^\.github$`, `^\.claude$`, `^ProjectDescription\.md$`,
`^ProjectReport\.md$`, `^LICENSE\.md$`.

### Files in `R/`

| File | Subject | Main functions |
|---|---|---|
| `grace-package.R` | package doc, imports | |
| `balance-vars.R` | balance-variable specs | `categorical_var()`, `continuous_var()` |
| `design.R` | trial design object | `grace_design()`, `validate_grace_design()` |
| `checkpoints.R` | run-in size and quotas | `runin_size()`, `arm_targets()`, `checkpoint_table()` |
| `rng.R` | isolated, replayable RNG | `with_trial_rng()` |
| `crypto.R` | encryption, passphrase | `encrypt_object()`, `decrypt_object()`, `trial_passphrase()` |
| `trial-file.R` | read and write trial file | `create_trial()`, `read_trial()`, `write_trial()` |
| `codes.R` | treatment codes | `read_codes()`, `draw_code()`, `void_code()` |
| `audit.R` | append-only audit log | `append_audit()`, `audit_log()` |
| `state.R` | running counts and sums | `new_state()`, `update_state()`, `state_from_records()` |
| `votes.R` | MSB votes | `vote_categorical()`, `vote_continuous()`, `vote_size()`, `tally_votes()` |
| `tilt.R` | biased coin | `tilt_probs()` |
| `quota.R` | checkpoint-driven probabilities | `quota_probs()`, `bound_probs()` |
| `allocation-mmsb.R` | modified MSB rule | `mmsb_probs()` |
| `allocation-comparators.R` | comparison rules | `srs_probs()`, `block_probs()`, `minimization_probs()`, `msb_probs()` |
| `randomize.R` | live randomization | `randomize()` |
| `override.R` | overrides | `override_assignment()` |
| `replay.R` | audit replay | `replay_assignment()` |
| `smd.R` | standardized mean differences | `smd_by_var()` |
| `tables.R` | output tables | `group_size_table()`, `balance_table()` |
| `plots.R` | output plots | `imbalance_plot()`, `smd_plot()` |
| `filters.R` | status filters | `filter_records()` |
| `status.R` | status report | `trial_status()` |
| `generators.R` | synthetic participants | `design_generator()`, `example_generator_iid()`, `example_generator_drift()` |
| `simulate.R` | simulation engine | `run_trial()`, `simulate_trials()` |
| `sim-metrics.R` | per-trial metrics | `trial_metrics()`, `correct_guess()`, `longest_forced_run()` |
| `sim-summary.R` | summaries | `summary.grace_sim()`, `print.grace_sim()` |
| `print.R` | print methods | `print.grace_design()`, `print.grace_assignment()`, `print.grace_status()` |

## 3. Data shapes and algorithm

### Design (`grace_design`)

```r
grace_design(
  n_total,
  balance_vars,
  arms = c("A", "B"),
  ratio = c(1, 1),
  weights = NULL,
  p_star = 0.30,
  xi = 0.65,
  runin_n = NULL,
  forced_at = NULL,
  size_bound = 3,
  runin_method = c("tilted", "rar")
)
```

- `balance_vars`: named list, for example
  `list(sex = categorical_var(c("male", "female")), age = continuous_var())`.
  Level names are lower-cased and trimmed.
- `weights`: positive numbers named like `balance_vars`. `NULL` means equal weights. Always
  rescaled to sum to 1.
- `ratio`: positive numbers, one per arm. Target proportions are `r = ratio / sum(ratio)`.
- `runin_n`: `NULL` means `runin_size(n_total, ratio)` (below). `0` turns the run-in off.
- `forced_at`: optional whole-number enrollment totals where proportions are forced.
- Constants (not arguments): size-vote limit 1, size-vote weight 1, base probability from
  the next checkpoint ("remaining").
- Validation errors: duplicate or missing names, `n_total < runin_n`, `forced_at`
  outside `1..n_total`, `xi` not in (0.5, 1), `p_star` not in (0, 1), weights ≤ 0.

### Run-in size and checkpoint quotas

- `runin_size(n_total, ratio)`: `n1 <- max(10, ceiling(0.10 * n_total))`. If every ratio
  is a whole number, round `n1` up to the next multiple of `sum(ratio)`. Cap at
  `n_total`. Examples with `ratio = c(1, 1)`: N = 60 gives 10, N = 200 gives 20,
  N = 1000 gives 100. With `ratio = c(2, 1)`: N = 60 gives 12.
- `arm_targets(n, r)`: whole-number counts that add up to `n`, using largest-remainder
  rounding of `n * r` (ties go to the earlier arm).
- `checkpoint_table(design)`: tibble with `n`, `type` (`"run-in"`, `"forced"`,
  `"final"`), and a list column `target` (from `arm_targets()`). The final checkpoint
  `n_total` is always present.

### Trial file (encrypted `.rds`)

The decrypted object, class `grace_trial`:

```r
list(
  format_version = 1L,
  design = <grace_design>,
  seed = <integer>,
  rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
  rng_state = <integer vector>,
  records = <tibble>,
  codes = <tibble or NULL>,
  audit = <tibble>
)
```

- `records`: `seq` (int), `id` (chr), one column per balance variable, `arm` (chr),
  `code` (chr or NA), `probs` (list of named numerics), `phase` (chr), `rng_state`
  (list: the state used for this draw), `time` (POSIXct UTC).
- `codes`: `code` (chr), `arm` (chr), `status` (`"unused"`, `"used"`, `"voided"`),
  `id` (chr or NA).
- `audit`: `time`, `action` (`"create"`, `"randomize"`, `"override"`), `id`, `old`,
  `new`, `reason`.
- On disk: `saveRDS(list(format = "grace-enc-1", salt, nonce, ciphertext))`. The key is
  `sodium::scrypt(charToRaw(passphrase), salt, size = 32)`. The ciphertext is
  `sodium::data_encrypt(serialize(trial, NULL), key, nonce)`.
- `write_trial()` writes to `<path>.tmp`, copies the current file to `<path>.bak`, then
  renames `.tmp` to `<path>`.

### RNG isolation (`with_trial_rng(trial, code)`)

Save the global `.Random.seed` (or note that it is missing). Set the trial's `rng_kind`
and install `trial$rng_state`. Evaluate `code`. Capture the new state into the trial.
Restore the user's kind and seed (or remove `.Random.seed` if there wasn't one), using
`withr::defer()`. `create_trial()` sets the first state with `set.seed(seed)` inside this
wrapper.

### Modified MSB probabilities (`mmsb_probs(state, x, design)`)

Inputs: running counts `n_a` for each arm, the next participant's values `x`, and
`i = sum(n_a) + 1`. Returns `list(probs = <named numeric adding to 1>, phase = <chr>)`.

1. **Quota.** `c` is the next checkpoint at or after `i`, `t_a = arm_targets(c, r)`, and
   `R = c - (i - 1)` slots remain. The base is `b_a = max(t_a - n_a, 0) / R`. These add up
   to 1 because the targets add up to `c`. If only one arm is short, its base is 1 and the
   assignment is forced (phase `"quota"`).
2. **Run-in** (`i <= runin_n`): with `runin_method = "rar"` return `b` (phase
   `"runin"`). With `"tilted"`, tilt `b` toward the vote winner (step 5), unless step 1
   forced the assignment.
3. **Bound** (after the run-in): `D_a = n_a - r_a * (i - 1)`. Arms with
   `D_a >= size_bound` get 0. If any arm has `D_a <= -size_bound`, only those arms can be
   chosen, in proportion to `-D_a` (phase `"bound"`).
4. **Votes.** Skip variables whose value is missing, and warn once per call with
   `cli::cli_warn()`.
   - Categorical: build the level × arm table, keeping levels with any count. With at
     least 2 levels and every arm > 0, run a Pearson chi-square test. If p < `p_star`,
     vote for the arm with the lowest proportion `O[level, a] / n_a`. A tie for lowest
     means no vote.
   - Continuous: needs at least 2 per arm. Use Welch's t-test for 2 arms and
     `oneway.test(var.equal = FALSE)` for more. If p < `p_star`: when x > the largest arm
     mean, vote for the arm with the smallest mean; when x < the smallest arm mean, vote
     for the arm with the largest mean; otherwise no vote.
   - Size vote (after the run-in only): the arm with the most negative `D_a` gets a vote
     of weight 1 when `D_a <= -1`.
   - Tally: `score_a = sum(weight of each vote for a)`. The winner is the single arm with
     the highest score. No votes or a tie for highest means no winner.
5. **Tilt** (`tilt_probs(b, winner, xi)`): if there is no winner, or `b[winner]` is 0 or
   1, return `b`. Otherwise
   `o = b_w / (1 - b_w) * xi / (1 - xi)`, `p_w = o / (1 + o)`, and every other arm gets
   `b_a * (1 - p_w) / (1 - b_w)`. Phase is `"msb-biased"` or `"msb-random"`.
6. **Draw:** `arm <- sample(names(probs), 1, prob = probs)` inside `with_trial_rng()`.

With 2 arms, `forced_at = NULL`, and `runin_n = 0`, steps 4 and 5 must give the same
probabilities as `msb_probs()` (the prototype's `rule_msb`) at every step.

### Comparators

- `srs_probs()`: `r`.
- `block_probs()`: permuted blocks of size `2 * sum(ratio)` (whole-number ratios only).
- `minimization_probs()`: Pocock–Simon with range imbalance, weights from the design,
  continuous variables split at tertiles of the first 10% of participants, and
  probability 0.8 for the arm that minimizes imbalance (ties spread equally).
- `msb_probs()`: unmodified MSB, which is steps 4 and 5 with base `r`, no size vote, no
  quotas, and no bound.

### Live functions (exported)

```r
create_trial(path, design, seed, codes = NULL, passphrase = trial_passphrase())
randomize(path, id, values, passphrase = trial_passphrase())
trial_status(
  path,
  passphrase = trial_passphrase(),
  from = NULL,
  to = NULL,
  checkpoint = NULL,
  vars = NULL
)
override_assignment(path, id, arm, reason, passphrase = trial_passphrase())
replay_assignment(path, id, passphrase = trial_passphrase())
audit_log(path, passphrase = trial_passphrase())
read_codes(file, arms)
```

- `randomize()` returns `grace_assignment`: `id`, `arm`, `code`, `probs`, `phase`, and a
  `status` field holding a `grace_status`. Printing it shows the id, arm, code, and the
  group size table.
- `grace_status`: `group_sizes` (tibble), `balance` (tibble), `imbalance_plot` (ggplot),
  `smd_plot` (ggplot).
- Errors: `create_trial()` when `path` exists. `randomize()` on a duplicate `id`, unknown
  level, extra or missing variable names in `values`, enrollment full, or no unused codes
  for the drawn arm. `override_assignment()` on an unknown `id`, the same arm, or an empty
  `reason`. `trial_status()` when filters conflict (`checkpoint` with `from`/`to`) or name
  an unknown checkpoint or variable.
- Override with codes: mark the old code `"voided"` (never reused), draw a new unused code
  for the new arm, and log both values in the audit row.
- `read_codes()`: CSV with columns `code` and `arm`. Error on a missing column, duplicate
  codes, or an arm not in `arms`.

### Tables and plots

- `group_size_table()`: `arm`, `n`, `pct`, `target_next` (target at the next checkpoint),
  `target_final`.
- `balance_table()`: one row per variable × level (categorical) or per variable
  (continuous). Columns per arm hold `n (pct)` or `mean (sd)`, plus `smd`, the largest
  absolute pairwise SMD between arms.
- SMD: binary `(p1 - p2) / sqrt((p1(1 - p1) + p2(1 - p2)) / 2)`; continuous
  `(m1 - m2) / sqrt((s1^2 + s2^2) / 2)`; categorical takes the largest SMD over its
  levels. Zero denominators give 0.
- `imbalance_plot()`: x = enrollment order, y = `n_a - r_a * n`, one line per arm,
  vertical dashed lines at checkpoints.
- `smd_plot()`: one dot per variable at its absolute SMD, with a dashed line at 0.1.

### Simulation

```r
design_generator(design, probs = NULL, mean_sd = NULL, drift_to = NULL)
simulate_trials(
  design,
  methods = c("mmsb", "srs", "blocks", "minimization", "msb"),
  n_sims = 1000,
  generator = design_generator(design),
  seed,
  parallel = FALSE
)
```

- A generator is `function(n)` that returns a tibble with one column per balance
  variable. `drift_to` gives end-of-enrollment probabilities (or means), reached by
  linear interpolation.
- `run_trial()` updates state step by step (no rebuilding from history) so that N = 5,000
  stays fast.
- `parallel = TRUE` uses `furrr::future_map(.options = furrr_options(seed = TRUE))`; the
  user sets `future::plan()`.
- Every method sees the same participant streams (common random numbers).
- `grace_sim`: long tibble with `sim`, `method`, `metric`, `value`.
- Metrics per trial: maximum and per-variable absolute SMD at the run-in, at each
  checkpoint, and at the end; `|n_a - target_a|` per arm at the same points; percent
  deterministic (any prob of 1); longest run of forced assignments; correct-guess
  probability (the guesser picks the arm with the most negative `D_a`, splitting ties).
- `summary.grace_sim()`: per method × metric the mean, sd, q95, q99, and max.

## 4. Formatter, linter, and test framework

- Formatter: `styler` (`styler::style_pkg()`), tidyverse style.
- Linter: `lintr`. `.lintr` uses
  `linters_with_defaults(line_length_linter(100), object_name_linter("snake_case"))`
  and excludes `JSSarticle/` and `data-raw/`.
- Tests: `testthat` 3rd edition. Every file in `R/` gets a matching
  `tests/testthat/test-<name>.R`. Plot tests use `vdiffr::expect_doppelganger()`. Use
  table-driven tests (a tibble of cases plus `purrr::pwalk()`) where one function has
  several cases.
- Coverage goal: at least 80% (`covr::package_coverage()`), checked locally.

## 5. Makefile targets

Requires `make` from Rtools 4.6 on Windows.

```make
.PHONY: format lint test check pre-pr

format:
	Rscript -e "styler::style_pkg()"

lint:
	Rscript -e "l <- lintr::lint_package(); print(l); quit(status = length(l) > 0)"

test:
	Rscript -e "devtools::test(stop_on_failure = TRUE)"

check:
	Rscript -e "devtools::check( \
	  error_on = 'warning', \
	  args = '--no-manual' \
	)"

pre-pr: format lint test
```

## 6. GitHub workflows

One file, `.github/workflows/ci.yml`, runs on `pull_request` (all branches) and `push` to
`main`. It uses `r-lib/actions/setup-r@v2` (R `release`) and
`r-lib/actions/setup-r-dependencies@v2`, which installs from `DESCRIPTION`.

| Job | Runs on | Steps |
|---|---|---|
| `lint` | ubuntu-latest | `styler::style_pkg()` then `git diff --exit-code`; then `make lint` |
| `test` | ubuntu-latest | `make test` |
| `check` | ubuntu-latest, windows-latest | `R CMD check --as-cran --no-manual`; fails on warnings |

After this workflow has run once on a PR, add `lint` and `test` as required status checks
in the "Protect main" ruleset.

## 7. Data access

- The package never connects to a trial database. The statistician copies the id and
  treatment code into REDCap or another system by hand.
- All trial data lives in one encrypted `.rds` file whose path the statistician chooses,
  plus `<path>.bak`.
- Passphrase: `trial_passphrase()` reads `GRACE_PASSPHRASE` from the environment. If it
  isn't set and the session is interactive, it prompts with `askpass::askpass()`.
  Otherwise it errors. The statistician may put `GRACE_PASSPHRASE` in a `.env` or
  `.Renviron` file outside the repo. Both are in `.gitignore`. The passphrase is never
  saved in the trial file, printed, or logged.
- A wrong passphrase gives a clear error ("could not decrypt; wrong passphrase or damaged
  file") and never a partial object.

## 8. Test data

All made up and small:

- `tests/testthat/fixtures/design_small.R`: builds a 2-arm design with N = 20, `sex`
  (male/female), `race` (white/black/other), and `age` (continuous). Also a 3-arm
  version with `ratio = c(2, 1, 1)` and N = 24.
- `tests/testthat/fixtures/codes_small.csv`: 30 codes, 15 for each of arms A and B.
  Codes are 5-digit strings starting at `"90001"`.
- `inst/extdata/example_codes.csv`: 200 made-up codes for a 2-arm example, built by
  `data-raw/example_codes.R` with `set.seed(20260611)`.
- `data/example_participants.rda`: 40 made-up participants (`id` `"P001"` to `"P040"`,
  `sex`, `race`, `age`), built by `data-raw/example_participants.R`.
- Tests set `GRACE_PASSPHRASE` with `withr::local_envvar()` and write trial files under
  `withr::local_tempdir()`.

## 9. Deployment

- The package runs on the unblinded statistician's computer. There is no server.
- Install during development: `pak::pak("BiostatDoc/grace")`.
- Releases: bump the version in `DESCRIPTION`, update `NEWS.md`, merge through a PR, then
  tag `vX.Y.Z` and create a GitHub release with `gh release create`.
- Statisticians update with `pak::pak("BiostatDoc/grace@vX.Y.Z")`. Pin a version for the
  life of a trial and record it in the trial file (`packageVersion("grace")` stored at
  `create_trial()`).
- CRAN: after the JSS submission, run `devtools::check(remote = TRUE, manual = TRUE)` and
  `devtools::check_win_devel()`, fix every NOTE, then `devtools::submit_cran()`. First
  check that the name `grace` is free on CRAN.

## 10. Build order

Each item is one Jira card and one PR. Cards depend on the cards before them unless noted.

### Setup and CI

1. **Package skeleton, Makefile, and CI.** Run `usethis::create_package()` in the repo,
   add `DESCRIPTION` (GPL-3, R ≥ 4.6, authors Grace Huiyi Ma and James W. Hardin as
   `cre`), `.lintr`, `Makefile`, `.Rbuildignore`, `renv`, `ci.yml`, and a placeholder
   `R/grace-package.R` with one test so CI has something to run. Remove `docs/` from
   `.gitignore` so this plan can be committed.
2. **Add the required status checks** `lint` and `test` to the ruleset (not a code card;
   done with `gh api` after card 1's CI has run).

### Data loading

3. **Balance variables, design, and checkpoints:** `balance-vars.R`, `design.R`,
   `checkpoints.R`.
4. **RNG isolation and encryption:** `rng.R`, `crypto.R`.
5. **Trial file, audit log, and treatment codes:** `trial-file.R`, `audit.R`, `codes.R`,
   `create_trial()`, `read_codes()`, and the example code data.

### Calculations

6. **Running state:** `state.R`.
7. **Votes and tilt:** `votes.R`, `tilt.R`, including the two-arm MSB reduction test.
8. **Quotas, bound, and modified MSB:** `quota.R`, `allocation-mmsb.R`.
9. **Live randomization and replay:** `randomize.R`, `replay.R`.
10. **Overrides:** `override.R`.
11. **Comparators:** `allocation-comparators.R` (can run alongside 9 and 10).

### Screens

12. **Tables and SMD:** `smd.R`, `tables.R`.
13. **Plots:** `plots.R`.
14. **Status report and filters:** `filters.R`, `status.R`, `print.R`.

### Simulation

15. **Generators:** `generators.R`.
16. **Simulation engine and metrics:** `simulate.R`, `sim-metrics.R`.
17. **Simulation summary and parallel runs:** `sim-summary.R`.

### Documentation and release

18. **Example data, vignette, and README:** `data-raw/`, `data/`, `vignettes/grace.Rmd`.
19. **JSS article:** `JSSarticle/` rewritten from the new package (keep the old draft's
    motivation and literature review), with `run_simulations.R` producing every number.
20. **Project documents:** `ProjectDescription.md` and `ProjectReport.md`.
21. **CRAN readiness:** `R CMD check --as-cran` clean on Windows and Linux, then version
    0.1.0.

## 11. Key tests (acceptance criteria)

- `test-checkpoints.R`: `runin_size()` gives 10, 20, 100 for N = 60, 200, 1000 with
  `c(1, 1)` and 12 for N = 60 with `c(2, 1)`. `arm_targets()` always adds up to `n`.
- `test-allocation-mmsb.R`: over 200 simulated trials, counts at the run-in, at each
  `forced_at` value, and at N exactly equal `arm_targets()` for 2 and 3 arms. Probabilities
  always add up to 1. The two-arm reduction matches `msb_probs()` at every step.
- `test-rng.R`: the global `.Random.seed` is unchanged after `randomize()`, including when
  it didn't exist before.
- `test-replay.R`: `replay_assignment()` reproduces the arm and code of every record.
- `test-crypto.R`: encrypting then decrypting gives the same object; a wrong passphrase
  errors.
- `test-trial-file.R`: `<path>.bak` holds the previous version after each write.
- `test-codes.R`: running out of codes errors; duplicate codes error; codes are never
  reused, including voided ones.
- `test-randomize.R`: duplicate id errors; a missing value warns and still randomizes;
  an unknown level errors.
- `test-override.R`: the audit log gains one row with the reason; an empty reason errors.
- `test-status.R`: each filter returns the expected subset; conflicting filters error.
- `test-plots.R`: vdiffr snapshots for both plots.
- `test-simulate.R`: the same `seed` gives identical results with `parallel = FALSE` and
  `parallel = TRUE`; `summary()` has one row per method × metric.
