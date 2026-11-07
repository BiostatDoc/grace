clean_values <- function(values, design) {
  if (is.data.frame(values)) {
    if (nrow(values) != 1) {
      cli::cli_abort("{.arg values} must have exactly one row.", class = "grace_error_values")
    }
    values <- as.list(values)
  }
  vars <- design$balance_vars
  check_value_names(names(values), names(vars))
  cleaned <- lapply(names(vars), \(name) clean_value(values[[name]], vars[[name]], name))
  stats::setNames(cleaned, names(vars))
}

check_value_names <- function(supplied, expected) {
  missing_names <- setdiff(expected, supplied)
  extra_names <- setdiff(supplied, expected)
  if (length(missing_names) > 0) {
    cli::cli_abort(
      "Missing value{?s} for {.field {missing_names}}.",
      class = "grace_error_values"
    )
  }
  unexpected <- unique(c(extra_names, supplied[duplicated(supplied)]))
  if (length(unexpected) > 0) {
    cli::cli_abort(
      "Unexpected or repeated value name{?s}: {.field {unexpected}}.",
      class = "grace_error_values"
    )
  }
}

clean_value <- function(value, var, name) {
  if (length(value) != 1) {
    cli::cli_abort(
      "{.field {name}} must be a single value.",
      class = "grace_error_values"
    )
  }
  if (var$type == "categorical") {
    clean_categorical(value, var, name)
  } else {
    clean_continuous(value, name)
  }
}

clean_categorical <- function(value, var, name) {
  if (is.na(value)) {
    return(NA_character_)
  }
  level <- tolower(trimws(as.character(value)))
  if (!level %in% var$levels) {
    cli::cli_abort(
      c(
        "{.val {level}} is not a level of {.field {name}}.",
        "i" = "Levels are {.val {var$levels}}."
      ),
      class = "grace_error_level"
    )
  }
  level
}

clean_continuous <- function(value, name) {
  if (is.na(value)) {
    return(NA_real_)
  }
  number <- suppressWarnings(as.numeric(value))
  if (is.na(number) || !is.finite(number)) {
    cli::cli_abort(
      "{.field {name}} must be a finite number.",
      class = "grace_error_values"
    )
  }
  number
}
