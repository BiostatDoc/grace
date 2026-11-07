#' Print methods
#'
#' Short text summaries of a design, an assignment, and a status report. Plots
#' are never printed.
#'
#' @param x A [grace_design()], the result of [randomize()], or the result of
#'   [trial_status()].
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#' @export
#' @examples
#' design <- example_design(20)
#' print(design)
print.grace_design <- function(x, ...) {
  cli::cli_h1("GRACE design")
  cli::cli_bullets(c(
    "*" = "Arms (ratio): {arm_summary(x)}",
    "*" = "Total participants: {x$n_total}",
    "*" = "Run-in: {runin_summary(x)}"
  ))
  cli::cli_text("Checkpoints:")
  print_table(checkpoint_summary(x))
  cli::cli_text("Balance variables:")
  print_table(variable_summary(x))
  invisible(x)
}

#' @rdname print.grace_design
#' @export
print.grace_assignment <- function(x, ...) {
  cli::cli_h1("GRACE assignment")
  cli::cli_bullets(c(
    "*" = "Participant: {x$id}",
    "*" = "Arm: {x$arm}"
  ))
  if (!is.null(x$code) && !is.na(x$code)) {
    cli::cli_bullets(c("*" = "Code: {x$code}"))
  }
  cli::cli_bullets(c("*" = "Phase: {x$phase}"))
  if (!is.null(x$status)) {
    cli::cli_text("Group sizes:")
    print_table(x$status$group_sizes)
  }
  invisible(x)
}

#' @rdname print.grace_design
#' @export
print.grace_status <- function(x, ...) {
  cli::cli_h1("GRACE status")
  cli::cli_bullets(c(
    "*" = "Participants randomized: {x$n_randomized}",
    "*" = "Filters: {filter_summary(x$filters)}"
  ))
  cli::cli_text("Group sizes:")
  print_table(x$group_sizes)
  cli::cli_text("Plots are in {.code $imbalance_plot} and {.code $smd_plot}.")
  invisible(x)
}

print_table <- function(table) {
  print(as.data.frame(table), row.names = FALSE)
}

arm_summary <- function(design) {
  paste0(design$arms, " (", format(unname(design$ratio)), ")", collapse = ", ")
}

runin_summary <- function(design) {
  if (design$runin_n == 0) {
    return("none")
  }
  paste0(design$runin_n, " participants, method ", design$runin_method)
}

checkpoint_summary <- function(design) {
  checkpoints <- design$checkpoints
  data.frame(
    n = checkpoints$n,
    type = checkpoints$type,
    target = vapply(
      checkpoints$target,
      \(target) paste0(names(target), "=", target, collapse = ", "),
      character(1)
    )
  )
}

variable_summary <- function(design) {
  data.frame(
    variable = names(design$balance_vars),
    type = vapply(design$balance_vars, \(var) var$type, character(1)),
    weight = round(unname(design$weights[names(design$balance_vars)]), 3),
    row.names = NULL
  )
}

filter_summary <- function(filters) {
  applied <- Filter(Negate(is.null), filters)
  if (length(applied) == 0) {
    return("none")
  }
  parts <- purrr::imap_chr(applied, \(value, name) paste0(name, " = ", toString(value)))
  paste(parts, collapse = "; ")
}
