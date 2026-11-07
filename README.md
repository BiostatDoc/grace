# graceDR

graceDR randomizes clinical trial participants one at a time into two or more arms. It uses a
modified minimal sufficient balance algorithm with weighted balance variables, an
exact-allocation run-in, and forced allocation at chosen enrollment totals. The trial is
stored in an encrypted file with an audit log, every past assignment can be replayed, and the
package simulates operating characteristics against simple random allocation, permuted
blocks, minimization, and unmodified MSB.

## Installation

``` r
pak::pak("BiostatDoc/grace")
```

## Example

``` r
library(graceDR)
Sys.setenv(GRACE_PASSPHRASE = "example-only")
design <- example_design(40)
path <- tempfile(fileext = ".rds")
create_trial(path, design, seed = 1)
randomize(path, "P001", list(sex = "male", race = "white", age = 61))
randomize(path, "P002", list(sex = "female", race = "black", age = 54))
trial_status(path)
audit_log(path)
```

Keep real passphrases in your `.Renviron` or `.env` file, never in a script. The example
passphrase above is only for made-up data.

## Learn more

The [vignette](vignettes/graceDR.Rmd) walks through a full trial: the design, randomizing
participants, balance reports and plots, overrides, the audit log, replay, and simulation.
After installing, run `vignette("graceDR", package = "graceDR")`.

## License

GPL (>= 3). See [LICENSE.md](LICENSE.md).
