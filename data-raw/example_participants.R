pkgload::load_all(quiet = TRUE)

set.seed(20260611)

example_participants <- example_generator_iid()(40) |>
  tibble::add_column(id = sprintf("P%03d", 1:40), .before = 1)

usethis::use_data(example_participants, overwrite = TRUE)
