# Replication project for the graceDR article

This RStudio project reproduces all code and output of the article, without the text. It uses
renv, so everyone gets the same package versions. All paths are relative to this folder.

## Requirements

- R 4.6.1. The lockfile records this version.
- Internet access for the first restore and for installing graceDR.
- No LaTeX or Quarto is needed.
- The project was tested on Windows only. On Linux, the sodium package needs the system library
  libsodium and a compiler to build packages from source.

## Steps

1. Open `replication.Rproj` in RStudio. Outside RStudio, start R in this folder. The `.Rprofile`
   activates renv. On the first start renv installs itself.
2. Restore the packages recorded in `renv.lock`:

   ```r
   renv::restore()
   ```

3. Run everything:

   ```r
   source("run-all.R")
   ```

   Or from a terminal in this folder:

   ```
   Rscript run-all.R
   ```

`run-all.R` runs the files in `R/` in order and prints each line of code with its output:

| File | Content |
|------|---------|
| `00-setup.R` | Installs graceDR from GitHub, loads the packages, sets the options |
| `01-simulate.R` | Runs 1,000 trials in each of four settings and saves them in `results/` |
| `02-summarize.R` | Reads `results/` and computes the summaries |
| `03-package-tables.R` | The tables of arguments and functions |
| `04-package-demo.R` | The worked example of the package |
| `05-simulation-tables.R` | The tables of the simulation study |
| `06-figure.R` | The figure of the simulation study |

## Output

- `results/`: the simulation results (`sim_*.rds`, `summary_*.rds`, `settings.rds`).
- `figures/`: `status-plots.pdf` and `tradeoff.pdf`.

Both folders start empty and their contents are not kept in git. Every run overwrites them.

## Run time

The simulations rerun every time: 4,000 simulated trials in all. The full run took about 22
minutes on the author's Windows computer, using up to six parallel workers. The number of trials,
the seed, and the settings are fixed in `R/01-simulate.R` to match the article. The time depends on
the number of cores.

## graceDR version

`00-setup.R` installs graceDR from the default branch of
<https://github.com/BiostatDoc/graceDR> with `pak::pak("BiostatDoc/graceDR")`. graceDR is not
recorded in `renv.lock`, so a later change to the package can change the results. All other
packages are locked.

## Updating the lockfile

The lockfile uses `snapshot.type` "implicit", so `renv::snapshot()` records only the packages that
the scripts use. graceDR is not installed from the lockfile, and several of its imports (askpass,
sodium, tidyr, withr) are not called by the scripts. A plain `renv::snapshot()` can therefore drop
them or leave them unpinned. To update the lockfile, run:

```r
renv::snapshot(packages = c(
  "askpass", "cli", "dplyr", "furrr", "future", "ggplot2", "knitr", "pak", "parallelly",
  "purrr", "renv", "rlang", "sodium", "tibble", "tidyr", "withr"
))
```
