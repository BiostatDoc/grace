#' Group sizes by arm
#'
#' @inheritParams smd_by_var
#'
#' @return A tibble with `arm`, `n`, `pct` (percent of all records, 1 decimal),
#'   `target_next` (targets at the first checkpoint above the current total, or
#'   the final targets if the trial is full), and `target_final`. Rows follow
#'   `design$arms`, including arms with no records.
#' @export
#' @examples
#' design <- example_design(20)
#' records <- withr::with_seed(1, example_generator_iid()(12))
#' records$seq <- seq_len(12)
#' records$arm <- rep(c("A", "B"), 6)
#' group_size_table(records, design)
group_size_table <- function(records, design) {
  arms <- design$arms
  n <- vapply(arms, \(a) sum(records$arm == a), integer(1))
  total <- nrow(records)
  checkpoints <- design$checkpoints
  upcoming <- which(checkpoints$n >= total + 1)
  next_row <- if (length(upcoming) > 0) upcoming[1] else nrow(checkpoints)
  tibble::tibble(
    arm = arms,
    n = unname(n),
    pct = if (total == 0) rep(0, length(arms)) else round(100 * unname(n) / total, 1),
    target_next = unname(checkpoints$target[[next_row]][arms]),
    target_final = unname(checkpoints$target[[nrow(checkpoints)]][arms])
  )
}

#' Covariate balance table
#'
#' One row per level of each categorical variable and one row per continuous
#' variable. Categorical cells show `n (pct%)`, where the percent is of the
#' arm's non-missing values. Continuous cells show `mean (sd)`.
#'
#' @inheritParams smd_by_var
#'
#' @return A tibble with `variable`, `level` (`NA` for continuous variables),
#'   one character column per arm, `missing` (count of missing values over all
#'   arms), and `smd` (from [smd_by_var()], repeated on each level row).
#' @export
#' @examples
#' design <- example_design(20)
#' records <- withr::with_seed(1, example_generator_iid()(12))
#' records$seq <- seq_len(12)
#' records$arm <- rep(c("A", "B"), 6)
#' balance_table(records, design)
balance_table <- function(records, design) {
  smd <- smd_by_var(records, design)
  rows <- lapply(
    names(design$balance_vars),
    \(name) variable_rows(records, name, design$balance_vars[[name]], design$arms)
  )
  long <- dplyr::bind_rows(rows)
  wide <- tidyr::pivot_wider(long, names_from = "arm", values_from = "cell")
  wide$missing <- as.integer(unname(vapply(
    wide$variable,
    \(name) sum(is.na(records[[name]])),
    numeric(1)
  )))
  wide$smd <- smd$smd[match(wide$variable, smd$variable)]
  wide[c("variable", "level", design$arms, "missing", "smd")]
}

variable_rows <- function(records, name, var, arms) {
  values <- records[[name]]
  levels <- if (var$type == "categorical") var$levels else NA_character_
  grid <- expand.grid(level = levels, arm = arms, stringsAsFactors = FALSE)
  cell <- purrr::map2_chr(
    grid$level,
    grid$arm,
    \(level, arm) format_cell(values[records$arm == arm], level, var$type)
  )
  tibble::tibble(variable = name, level = grid$level, arm = grid$arm, cell = cell)
}

format_cell <- function(values, level, type) {
  values <- values[!is.na(values)]
  if (type == "categorical") {
    count <- sum(values == level)
    pct <- if (length(values) == 0) 0 else 100 * count / length(values)
    return(sprintf("%d (%.1f%%)", count, pct))
  }
  if (length(values) == 0) {
    return("-")
  }
  spread <- if (length(values) > 1) sprintf("%.1f", stats::sd(values)) else "-"
  sprintf("%.1f (%s)", mean(values), spread)
}
