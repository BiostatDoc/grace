# Running make on Windows

`make pre-pr` works from Git Bash on Windows with Rtools `make`
(for example `/c/rtools45/usr/bin/make`). Run it from the repo root.

## What the Makefile does

Rtools `make` removes most Windows environment variables before it starts a
recipe. Two of them matter here:

- `LOCALAPPDATA`. Without it, renv looks for the project library in
  `C:/R/cache/R/renv/...` instead of `%LOCALAPPDATA%/R/cache/R/renv/...`. It
  sees an empty library and fails with `there is no package called 'styler'`.
- `PROCESSOR_ARCHITECTURE`. Without it, `Rscript` crashes with a segmentation
  fault (exit code 139) when it exits after running styler.

At the top of the `Makefile`, a block runs only when `uname -s` reports
MINGW or MSYS. It sets both variables for every recipe, using `$HOME` and
`uname -m` to build the values. Nothing is set on Linux or macOS, and a
`LOCALAPPDATA` that already reaches `make` is left alone. The targets and their
commands are unchanged, so `make -n pre-pr` prints the same three commands.

## Why not set `RENV_PATHS_ROOT` in `~/.Renviron`

For a package project, `renv/activate.R` takes the library location from
`renv_bootstrap_user_dir()`, which ignores `RENV_PATHS_ROOT`. Setting it in
`~/.Renviron` does not change the library path, so no machine setting is needed.

## First run in a new worktree

```bash
Rscript -e "renv::restore(prompt = FALSE)"
make pre-pr
```

If `devtools::check()` says pandoc is missing, run
`export RSTUDIO_PANDOC="/c/Program Files/Quarto/bin/tools"` first.
