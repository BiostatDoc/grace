imbalance_data <- function(records, design) {
  steps <- seq_len(nrow(records))
  purrr::map(
    design$arms,
    \(name) {
      tibble::tibble(
        k = steps,
        arm = name,
        deviation = cumsum(records$arm == name) - design$r[[name]] * steps
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(arm = factor(.data$arm, levels = design$arms))
}

#' Cumulative imbalance plot
#'
#' Shows, for each arm, the cumulative count minus the target share after each
#' participant. Dashed vertical lines mark checkpoints already reached.
#'
#' @inheritParams smd_by_var
#'
#' @return A `ggplot` object. With no records it is an empty plot with a
#'   subtitle saying so.
#' @export
#' @examples
#' design <- example_design(20)
#' records <- withr::with_seed(1, example_generator_iid()(12))
#' records$seq <- seq_len(12)
#' records$arm <- rep(c("A", "B"), 6)
#' imbalance_plot(records, design)
imbalance_plot <- function(records, design) {
  labels <- ggplot2::labs(
    x = "Participants randomized",
    y = "Count minus target"
  )
  if (nrow(records) == 0) {
    return(
      ggplot2::ggplot() +
        labels +
        ggplot2::labs(subtitle = "No participants randomized yet.")
    )
  }
  reached <- tibble::tibble(n = design$checkpoints$n) |>
    dplyr::filter(.data$n <= nrow(records))
  ggplot2::ggplot(
    imbalance_data(records, design),
    ggplot2::aes(x = .data$k, y = .data$deviation, colour = .data$arm)
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(
      data = reached,
      mapping = ggplot2::aes(xintercept = .data$n),
      linetype = "dashed"
    ) +
    ggplot2::geom_step() +
    labels +
    ggplot2::labs(colour = "Arm")
}

#' Standardized mean difference (Love) plot
#'
#' One point per balance variable at its absolute standardized mean difference,
#' with a dashed reference line at 0.1. Variables with a missing value are
#' dropped.
#'
#' @param smd A tibble from [smd_by_var()].
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' design <- example_design(20)
#' records <- withr::with_seed(1, example_generator_iid()(20))
#' records$seq <- seq_len(20)
#' records$arm <- rep(c("A", "B"), 10)
#' smd_plot(smd_by_var(records, design))
smd_plot <- function(smd) {
  data <- smd |>
    dplyr::mutate(
      variable = factor(.data$variable, levels = rev(unique(.data$variable))),
      abs_smd = abs(.data$smd)
    ) |>
    dplyr::filter(!is.na(.data$abs_smd))
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$abs_smd, y = .data$variable)
  ) +
    ggplot2::geom_vline(xintercept = 0.1, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Absolute standardized mean difference",
      y = NULL
    )
}
