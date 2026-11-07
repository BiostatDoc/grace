filter_fixture <- function() {
  design <- small_design_2arm(forced_at = 15)
  df <- eight_records()[rep(1:8, length.out = 20), ]
  df$id <- as.character(seq_len(20))
  df$arm <- rep(c("A", "B"), 10)
  list(design = design, records = make_records(design, df))
}

test_that("filter_records applies each filter", {
  fixture <- filter_fixture()
  cases <- list(
    list(args = list(), expected = 1:20),
    list(args = list(from = 5), expected = 5:20),
    list(args = list(to = 7), expected = 1:7),
    list(args = list(from = 3, to = 9), expected = 3:9),
    list(args = list(checkpoint = "run-in"), expected = 1:10),
    list(args = list(checkpoint = "final"), expected = 1:20),
    list(args = list(checkpoint = 15), expected = 1:15)
  )

  for (case in cases) {
    result <- do.call(
      filter_records,
      c(list(fixture$records, fixture$design), list(
        from = case$args$from,
        to = case$args$to,
        checkpoint = case$args$checkpoint
      ))
    )
    expect_identical(result$seq, as.integer(case$expected))
  }
})

test_that("filter_records rejects bad filters", {
  fixture <- filter_fixture()
  short <- fixture$records[1:8, ]
  cases <- list(
    list(records = fixture$records, args = list(checkpoint = "run-in", from = 2)),
    list(records = fixture$records, args = list(checkpoint = "run-in", to = 5)),
    list(records = fixture$records, args = list(checkpoint = "interim")),
    list(records = fixture$records, args = list(checkpoint = 12)),
    list(records = short, args = list(checkpoint = "run-in")),
    list(records = short, args = list(checkpoint = 15)),
    list(records = fixture$records, args = list(from = 9, to = 4)),
    list(records = fixture$records, args = list(from = 2.5))
  )

  for (case in cases) {
    expect_error(
      filter_records(
        case$records,
        fixture$design,
        from = case$args$from,
        to = case$args$to,
        checkpoint = case$args$checkpoint
      ),
      class = "grace_error_status"
    )
  }
})

test_that("filter_records says how many are randomized when a checkpoint is not reached", {
  fixture <- filter_fixture()

  expect_error(
    filter_records(fixture$records[1:8, ], fixture$design, NULL, NULL, "run-in"),
    "8"
  )
})
