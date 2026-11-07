set.seed(20260611)

codes <- sample(10000:99999, 200)
arm <- sample(rep(c("A", "B"), each = 100))

utils::write.csv(
  data.frame(code = codes, arm = arm),
  file = file.path("inst", "extdata", "example_codes.csv"),
  row.names = FALSE,
  quote = FALSE
)
