smd_example <- function() {
  tibble::tibble(
    variable = c("sex", "race", "age"),
    smd = c(0.05, -0.32, 0.18)
  )
}

test_that("imbalance_plot snapshot", {
  design <- small_design_2arm()
  records <- make_records(design, eight_records())

  plot <- imbalance_plot(records, design)

  vdiffr::expect_doppelganger("imbalance-2arm", plot)
})

test_that("imbalance_plot handles no records", {
  design <- small_design_2arm()
  records <- make_records(design, eight_records()[0, ])

  plot <- imbalance_plot(records, design)

  expect_s3_class(plot, "ggplot")
  vdiffr::expect_doppelganger("imbalance-empty", plot)
})

test_that("smd_plot snapshot", {
  plot <- smd_plot(smd_example())

  vdiffr::expect_doppelganger("smd-3vars", plot)
})

test_that("plots return ggplot objects", {
  design <- small_design_2arm()
  records <- make_records(design, eight_records())
  smd <- smd_by_var(records, design)

  expect_s3_class(imbalance_plot(records, design), "ggplot")
  expect_s3_class(smd_plot(smd), "ggplot")
})
