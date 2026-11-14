# gracedr for Stata

This folder holds the Stata 19 version of graceDR. The design is in
`docs/superpowers/specs/2026-11-13-stata-addition-design.md`. Right now it holds the
scaffolding (a placeholder `gracedr` command and the checks that run the certification
scripts) and the study folder helpers described below.

## Requirements

- Windows, with Stata 19 installed at `C:/Program Files/StataNow19/StataMP-64.exe`. To use another
  location, set the environment variable `STATA_EXE`.
- `make` and `bash` (Git for Windows provides both).
- The `stata_linter` package (`ssc install stata_linter`), Python, and the Python package
  `pandas`.
- Later cards add the Python package `pyzipper` for the encryption utility.

## Checks

Run these from the repository root, or from this folder with `make <target>`:

| Target | What it does |
|---|---|
| `format` | Replaces tabs with spaces and removes trailing spaces |
| `lint` | Runs `stata_linter` on every `.ado` and `.do` file, lines up to 99 characters |
| `test` | Runs every `cert/certify_*.do` in batch mode and fails on any Stata error |
| `pre-pr` | Runs the three above in order |

Write each certification file as `cert/certify_<subject>.do`. It must start by finding the
`stata/` folder through the environment variable that `run_stata.sh` sets:

```
local stata_dir : environment GRACEDR_STATA_DIR
adopath ++ "`stata_dir'/ado"
```

The root `make pre-pr` runs the R checks and then these. It fails on a machine without Stata.

The batch runner writes `<do-file name>.log` in this folder. The top of a Stata log shows the
license holder and serial number, so `run_stata.sh` prints only the part after the do-file starts,
and logs are never committed.

`make` from Rtools passes Stata almost no environment variables. `run_stata.ps1` restores
`ComSpec`, `TEMP` and `TMP` before it starts Stata. Shell scripts here use bash globs and never
call `find` or `sort`, which Rtools bash resolves to Windows programs.

## Study files

`_gracedr_study` reads and writes the datasets in a study folder. A write goes to `<name>.tmp`
first, the current file is copied to `<name>.bak`, and the new file replaces it. Stata has no
file rename, so the replacement is a copy. `_gracedr_utc` returns the UTC clock through
the Windows PowerShell under `SYSTEMROOT`, because Stata has only local time. Both are certified in
`cert/certify_study.do` and `cert/certify_utc.do`. Shared certification programs
(`expect_failure`, `make_data`, `remove_folder`) are in `cert/helpers.do`.

## CI

`.github/workflows/stata.yml` runs `stata-lint` and `stata-test` on every pull request from this
repository and on every push to `main`. GitHub's hosted runners have no Stata license, so the jobs
run on a self-hosted Windows runner with the requirements above. Pull requests from forks are
skipped so that outside code never runs on that machine.
