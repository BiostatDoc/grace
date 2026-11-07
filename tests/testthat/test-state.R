three_participants <- function() {
  list(
    list(x = list(sex = "male", race = "white", age = 30), arm = "A"),
    list(x = list(sex = "female", race = "black", age = 50), arm = "B"),
    list(x = list(sex = "male", race = "other", age = 70), arm = "A")
  )
}

apply_participants <- function(state, participants, design) {
  for (p in participants) {
    state <- update_state(state, p$x, p$arm, design)
  }
  state
}

test_that("update_state counts categorical and continuous values", {
  design <- small_design_2arm()
  state <- new_state(design)

  result <- apply_participants(state, three_participants(), design)

  expect_identical(result$n, c(A = 2L, B = 1L))
  expect_identical(
    result$cat$sex,
    matrix(
      c(2L, 0L, 0L, 1L),
      nrow = 2,
      dimnames = list(c("male", "female"), c("A", "B"))
    )
  )
  expect_identical(
    result$cat$race,
    matrix(
      c(1L, 0L, 1L, 0L, 1L, 0L),
      nrow = 3,
      dimnames = list(c("white", "black", "other"), c("A", "B"))
    )
  )
  expect_identical(
    result$cont$age,
    matrix(
      c(2, 1, 100, 50, 5800, 2500),
      nrow = 2,
      dimnames = list(c("A", "B"), c("n", "sum", "ss"))
    )
  )
})

test_that("missing values are skipped", {
  design <- small_design_2arm()
  state <- new_state(design)

  result <- update_state(
    state,
    list(sex = NA_character_, race = "white", age = NA_real_),
    "B",
    design
  )

  expect_identical(result$n, c(A = 0L, B = 1L))
  expect_identical(sum(result$cat$sex), 0L)
  expect_identical(result$cat$race["white", "B"], 1L)
  expect_identical(sum(result$cont$age), 0)
})

test_that("state_from_records equals repeated update_state", {
  design <- small_design_2arm()
  set.seed(7)
  records <- tibble::tibble(
    seq = 20:1,
    id = paste0("p", 20:1),
    sex = sample(c("male", "female"), 20, replace = TRUE),
    race = sample(c("white", "black", "other"), 20, replace = TRUE),
    age = round(stats::runif(20, 20, 80)),
    arm = sample(c("A", "B"), 20, replace = TRUE)
  )
  records$sex[3] <- NA

  result <- state_from_records(records, design)

  expected <- new_state(design)
  for (i in order(records$seq)) {
    row <- records[i, ]
    x <- list(sex = row$sex, race = row$race, age = row$age)
    expected <- update_state(expected, x, row$arm, design)
  }
  expect_identical(result, expected)
  expect_identical(sum(result$n), 20L)
})

test_that("bins are counted when breaks are given", {
  design <- small_design_2arm()
  breaks <- list(age = c(40, 60))
  state <- new_state(design, breaks)

  result <- apply_participants(state, three_participants(), design)

  expect_identical(
    result$cat$age__bin,
    matrix(
      c(1L, 0L, 1L, 0L, 1L, 0L),
      nrow = 3,
      dimnames = list(c("1", "2", "3"), c("A", "B"))
    )
  )
  expect_null(new_state(design)$cat$age__bin)
})
