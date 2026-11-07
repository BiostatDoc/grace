test_that("new_audit has the documented columns and no rows", {
  audit <- new_audit()

  expect_s3_class(audit, "tbl_df")
  expect_identical(nrow(audit), 0L)
  expect_identical(
    names(audit),
    c("event", "time", "action", "id", "old", "new", "reason")
  )
})

test_that("append_audit numbers events and keeps old rows", {
  first <- append_audit(new_audit(), "create")

  second <- append_audit(first, "edit", id = "P1", old = "a", new = "b", reason = "typo")

  expect_identical(second$event, 1:2)
  expect_identical(second[1, ], first)
  expect_identical(second$action, c("create", "edit"))
  expect_identical(second$id, c(NA, "P1"))
  expect_identical(second$old, c(NA, "a"))
  expect_identical(second$new, c(NA, "b"))
  expect_identical(second$reason, c(NA, "typo"))
  expect_s3_class(second$time, "POSIXct")
  expect_identical(attr(second$time, "tzone"), "UTC")
  expect_identical(class(second$event), "integer")
})
