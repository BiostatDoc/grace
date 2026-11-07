summary_matrix <- function(groups) {
  rows <- lapply(groups, \(values) c(n = length(values), sum = sum(values), ss = sum(values^2)))
  do.call(rbind, rows)
}

two_arm_table <- function(counts) {
  matrix(counts, 2, dimnames = list(c("male", "female"), c("A", "B")))
}

test_that("vote_categorical matches the two-arm MSB prototype", {
  skewed <- two_arm_table(c(15, 5, 5, 15))
  balanced <- two_arm_table(c(10, 10, 11, 9))

  expect_identical(vote_categorical(skewed, "male", 0.3), "B")
  expect_identical(vote_categorical(skewed, "female", 0.3), "A")
  expect_identical(vote_categorical(balanced, "male", 0.3), NA_character_)
  expect_identical(vote_categorical(skewed, NA_character_, 0.3), NA_character_)
})

test_that("vote_categorical gives no vote when the table cannot be tested", {
  empty_arm <- two_arm_table(c(5, 5, 0, 0))
  one_level <- two_arm_table(c(5, 0, 5, 0))

  expect_identical(vote_categorical(empty_arm, "male", 0.3), NA_character_)
  expect_identical(vote_categorical(one_level, "male", 0.3), NA_character_)
  expect_identical(vote_categorical(one_level, "female", 0.3), NA_character_)
  expect_identical(
    vote_categorical(two_arm_table(c(15, 5, 5, 15)), "other", 0.3),
    NA_character_
  )
})

test_that("vote_categorical works for three arms", {
  dimnames <- list(c("l1", "l2"), c("A", "B", "C"))
  clear <- matrix(c(20, 5, 10, 15, 15, 10), 2, dimnames = dimnames)
  tied <- matrix(c(10, 15, 10, 15, 20, 5), 2, dimnames = dimnames)

  expect_identical(vote_categorical(clear, "l1", 0.3), "B")
  expect_identical(vote_categorical(clear, "l2", 0.3), "A")
  expect_identical(vote_categorical(tied, "l1", 0.3), NA_character_)
})

test_that("vote_continuous matches the two-arm prototype", {
  s <- rbind(
    A = c(n = 20, sum = 1000, ss = 20 * (25 * 19 / 20 + 2500)),
    B = c(n = 20, sum = 1200, ss = 20 * (25 * 19 / 20 + 3600))
  )

  expect_identical(vote_continuous(s, 70, 0.3), "A")
  expect_identical(vote_continuous(s, 40, 0.3), "B")
  expect_identical(vote_continuous(s, 55, 0.3), NA_character_)
  expect_identical(vote_continuous(s, NA_real_, 0.3), NA_character_)
})

test_that("vote_continuous gives no vote without enough data", {
  too_few <- rbind(A = c(n = 1, sum = 50, ss = 2500), B = c(n = 20, sum = 1200, ss = 72000))
  zero_variance <- rbind(A = c(n = 5, sum = 250, ss = 12500), B = c(n = 5, sum = 300, ss = 18000))

  expect_identical(vote_continuous(too_few, 70, 0.3), NA_character_)
  expect_identical(vote_continuous(zero_variance, 70, 0.3), NA_character_)
})

test_that("vote_continuous votes for the extreme mean with three arms", {
  groups <- list(
    A = c(4.1, 5.2, 6.3, 5.5, 4.8, 5.9),
    B = c(6.8, 7.4, 8.1, 6.9, 7.7, 8.5, 7.2),
    C = c(9.0, 10.1, 9.4, 10.6, 9.8, 10.3)
  )
  s <- summary_matrix(groups)

  expect_identical(vote_continuous(s, 15, 0.3), "A")
  expect_identical(vote_continuous(s, 1, 0.3), "C")
  expect_identical(vote_continuous(s, 7, 0.3), NA_character_)
})

test_that("Welch ANOVA from summaries matches oneway.test", {
  groups <- list(
    A = c(4.1, 5.2, 6.3, 5.5, 4.8, 5.9),
    B = c(6.8, 7.4, 8.1, 6.9, 7.7, 8.5, 7.2),
    C = c(5.0, 6.1, 7.2, 6.6, 5.8, 6.9, 7.5, 6.0)
  )
  values <- unlist(groups)
  labels <- factor(rep(names(groups), lengths(groups)))

  from_summaries <- continuous_p_value(summary_matrix(groups))
  from_oneway <- oneway.test(values ~ labels, var.equal = FALSE)$p.value

  expect_equal(from_summaries, from_oneway, tolerance = 1e-10)
})

test_that("two-arm continuous p-value matches the Welch t-test", {
  groups <- list(A = c(4.1, 5.2, 6.3, 5.5, 4.8, 5.9), B = c(6.8, 7.4, 8.1, 6.9, 7.7, 8.5, 7.2))

  from_summaries <- continuous_p_value(summary_matrix(groups))
  from_t_test <- t.test(groups$A, groups$B, var.equal = FALSE)$p.value

  expect_equal(from_summaries, from_t_test, tolerance = 1e-10)
})

test_that("vote_size votes for the most under-filled arm", {
  cases <- list(
    list(n = c(A = 6, B = 4), r = c(A = 0.5, B = 0.5), expected = "B"),
    list(n = c(A = 4, B = 6), r = c(A = 0.5, B = 0.5), expected = "A"),
    list(n = c(A = 5, B = 5), r = c(A = 0.5, B = 0.5), expected = NA_character_),
    list(n = c(A = 3, B = 4), r = c(A = 0.5, B = 0.5), expected = NA_character_),
    list(n = c(A = 3, B = 1, C = 4), r = c(A = 1, B = 1, C = 1) / 3, expected = "B"),
    list(n = c(A = 6, B = 1, C = 1), r = c(A = 1, B = 1, C = 1) / 3, expected = NA_character_),
    list(n = c(A = 4, B = 2, C = 2), r = c(A = 0.5, B = 0.25, C = 0.25), expected = NA_character_),
    list(n = c(A = 0, B = 0), r = c(A = 0.5, B = 0.5), expected = NA_character_)
  )

  for (case in cases) {
    expect_identical(vote_size(case$n, case$r), case$expected)
  }
})

test_that("tally_votes picks a single weighted winner", {
  weights <- c(sex = 0.5, race = 0.3, age = 0.2)

  single <- tally_votes(c(sex = "A", race = "B", age = "B"), c(sex = 0.4, race = 0.35, age = 0.25))
  weighted <- tally_votes(c(sex = "A", race = "B", age = NA), weights)
  tie <- tally_votes(c(sex = "A", race = "B", age = NA), c(sex = 0.4, race = 0.4, age = 0.2))
  none <- tally_votes(c(sex = NA_character_, race = NA_character_, age = NA_character_), weights)

  expect_identical(single, "B")
  expect_identical(weighted, "A")
  expect_identical(tie, NA_character_)
  expect_identical(none, NA_character_)
})
