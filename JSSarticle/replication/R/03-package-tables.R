code <- function(...) {
  paste0("\\code{", gsub("_", "\\_", c(...), fixed = TRUE), "}", collapse = ", ")
}

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
