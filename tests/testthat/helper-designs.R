small_design_2arm <- function(...) {
  grace_design(
    n_total = 20,
    balance_vars = list(
      sex = categorical_var(c("male", "female")),
      race = categorical_var(c("white", "black", "other")),
      age = continuous_var()
    ),
    ...
  )
}

small_design_3arm <- function(...) {
  grace_design(
    n_total = 24,
    balance_vars = list(
      sex = categorical_var(c("male", "female")),
      race = categorical_var(c("white", "black", "other")),
      age = continuous_var()
    ),
    arms = c("A", "B", "C"),
    ratio = c(2, 1, 1),
    ...
  )
}
