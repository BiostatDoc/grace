## Test environments

* Local: Windows 11, R 4.6.1, fresh clone of the repository
  (`devtools::check(remote = TRUE, manual = TRUE)`)
* GitHub Actions: ubuntu-latest and windows-latest, current R release
  (`.github/workflows/ci.yml`)
* win-builder: Windows Server 2022, R Under development (2026-11-07 r90850)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

The first win-builder run also reported that `LICENSE.md`, linked from
`README.md`, was a possibly invalid file URI. The README now links to the
file on GitHub. A second win-builder run was submitted after that fix, but
its results email did not arrive, so the fix has not been confirmed on
win-builder.
