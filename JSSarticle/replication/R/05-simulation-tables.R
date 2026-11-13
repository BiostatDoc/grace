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
