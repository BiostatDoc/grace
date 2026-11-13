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

cells <- c("N200_iid", "N200_drift", "N500_iid", "N500_drift")
method_order <- c("srs", "blocks", "minimization", "msb", "mmsb")
method_label <- c(
  srs = "Simple randomization", blocks = "Permuted blocks", minimization = "Minimization",
  msb = "MSB", mmsb = "Modified MSB"
)
cell_label <- c(
  N200_iid = "N = 200, iid", N200_drift = "N = 200, drift",
  N500_iid = "N = 500, iid", N500_drift = "N = 500, drift"
)
