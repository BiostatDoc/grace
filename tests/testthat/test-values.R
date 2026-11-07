test_that("clean_values cleans and validates", {
  design <- small_design_2arm()
  ok <- list(sex = " Male ", race = "White", age = "42.5")

  result <- clean_values(ok, design)

  expect_identical(result, list(sex = "male", race = "white", age = 42.5))
  expect_identical(
    clean_values(
      list(age = 3, race = "other", sex = "female"),
      design
    ),
    list(sex = "female", race = "other", age = 3)
  )
  expect_identical(
    clean_values(data.frame(sex = "male", race = "black", age = 9), design),
    list(sex = "male", race = "black", age = 9)
  )
  expect_identical(
    clean_values(list(sex = NA, race = NA, age = NA), design),
    list(sex = NA_character_, race = NA_character_, age = NA_real_)
  )
})

test_that("clean_values rejects bad input", {
  design <- small_design_2arm()
  cases <- list(
    list(list(sex = "male", race = "purple", age = 1), "grace_error_level"),
    list(list(sex = "male", race = "white", age = "abc"), "grace_error_values"),
    list(list(sex = "male", race = "white", age = Inf), "grace_error_values"),
    list(list(sex = "male", race = "white", age = c(1, 2)), "grace_error_values"),
    list(list(sex = "male", race = "white"), "grace_error_values"),
    list(list(sex = "male", race = "white", age = 1, extra = 2), "grace_error_values")
  )

  for (case in cases) {
    expect_error(clean_values(case[[1]], design), class = case[[2]])
  }
})
