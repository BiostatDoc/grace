small_codes <- function() {
  read_codes(test_path("fixtures", "codes_small.csv"), c("A", "B"))
}

test_that("read_codes returns unused codes", {
  codes <- small_codes()

  expect_s3_class(codes, "tbl_df")
  expect_identical(names(codes), c("code", "arm", "status", "id", "drawn_event"))
  expect_identical(nrow(codes), 30L)
  expect_true(all(codes$status == "unused"))
  expect_identical(table(codes$arm)[["A"]], 15L)
  expect_identical(class(codes$drawn_event), "integer")
})

test_that("read_codes validates the file", {
  cases <- list(
    missing_code_column = data.frame(arm = c("A", "B")),
    missing_arm_column = data.frame(code = c("1", "2")),
    duplicate_code = data.frame(code = c("1", "1"), arm = c("A", "B")),
    empty_code = data.frame(code = c("1", ""), arm = c("A", "B")),
    unknown_arm = data.frame(code = c("1", "2"), arm = c("A", "Z"))
  )

  for (name in names(cases)) {
    path <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(cases[[name]], path, row.names = FALSE)
    expect_error(read_codes(path, c("A", "B")), class = "grace_error_codes", info = name)
  }
})

test_that("draw_code takes only unused codes of the arm and errors when none are left", {
  codes <- small_codes()
  drawn <- character(0)

  for (event in 1:15) {
    result <- draw_code(codes, "A", event)
    codes <- result$codes
    drawn <- c(drawn, result$code)
  }

  expect_length(unique(drawn), 15)
  expect_true(all(codes$arm[match(drawn, codes$code)] == "A"))
  expect_true(all(codes$status[match(drawn, codes$code)] == "used"))
  expect_identical(codes$drawn_event[match(drawn, codes$code)], 1:15)
  expect_error(draw_code(codes, "A", 16L), class = "grace_error_no_codes")
  expect_identical(sum(codes$status == "unused"), 15L)
})

test_that("draw_code works when only one code is left", {
  codes <- small_codes()
  codes$status[codes$arm == "A"][-1] <- "used"

  result <- draw_code(codes, "A", 1L)

  expect_identical(result$code, "90001")
})

test_that("voided codes are never drawn again", {
  codes <- small_codes()
  first <- draw_code(codes, "A", 1L)
  codes <- void_code(first$codes, first$code)
  drawn <- character(0)

  for (event in 2:15) {
    result <- draw_code(codes, "A", event)
    codes <- result$codes
    drawn <- c(drawn, result$code)
  }

  expect_false(first$code %in% drawn)
  expect_identical(codes$status[codes$code == first$code], "voided")
  expect_error(draw_code(codes, "A", 16L), class = "grace_error_no_codes")
})

test_that("void_code only voids used codes", {
  codes <- small_codes()

  expect_error(void_code(codes, "90001"), class = "grace_error_codes")
  expect_error(void_code(codes, "not-a-code"), class = "grace_error_codes")
})

test_that("example codes ship with the package", {
  path <- system.file("extdata", "example_codes.csv", package = "grace")

  codes <- read_codes(path, c("A", "B"))

  expect_identical(nrow(codes), 200L)
  expect_identical(table(codes$arm)[["A"]], 100L)
})
