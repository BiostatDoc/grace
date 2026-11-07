simulation_rules <- function() {
  list(
    mmsb = mmsb_probs,
    srs = srs_probs,
    blocks = block_probs,
    minimization = minimization_probs,
    msb = msb_probs
  )
}

abort_simulate <- function(message, .envir = parent.frame()) {
  cli::cli_abort(
    message,
    class = "grace_error_simulate",
    .envir = .envir,
    call = rlang::caller_env(2)
  )
}

rule_for_method <- function(method) {
  rules <- simulation_rules()
  if (!is.character(method) || length(method) != 1 || !method %in% names(rules)) {
    abort_simulate("Unknown method. Choose from {.val {names(rules)}}.")
  }
  rules[[method]]
}

run_trial <- function(method, stream, design, breaks = NULL) {
  rule <- rule_for_method(method)
  vars <- names(design$balance_vars)
  state <- new_state(design, breaks)
  n <- nrow(stream)
  cleaned <- vector("list", n)
  arms <- character(n)
  probs <- vector("list", n)
  phases <- character(n)
  for (j in seq_len(n)) {
    x <- clean_values(as.list(stream[j, vars]), design)
    result <- rule(state, x, design)
    arms[[j]] <- draw_arm(result$probs)
    state <- update_state(state, x, arms[[j]], design)
    cleaned[[j]] <- x
    probs[[j]] <- result$probs
    phases[[j]] <- result$phase
  }
  columns <- lapply(
    stats::setNames(vars, vars),
    \(name) unlist(lapply(cleaned, `[[`, name), use.names = FALSE)
  )
  tibble::as_tibble(
    c(list(seq = seq_len(n)), columns, list(arm = arms, probs = probs, phase = phases))
  )
}

minimization_breaks <- function(generator, design, seed) {
  draws <- withr::with_seed(seed, generator(1000))
  continuous <- Filter(\(name) design$balance_vars[[name]]$type == "continuous", names(draws))
  lapply(
    stats::setNames(continuous, continuous),
    \(name) stats::quantile(draws[[name]], c(1 / 3, 2 / 3), names = FALSE)
  )
}

simulate_one <- function(sim, seed, methods, generator, design, breaks) {
  withr::with_seed(seed, {
    stream <- generator(design$n_total)
    by_method <- lapply(methods, \(method) {
      trial <- run_trial(
        method,
        stream,
        design,
        if (method == "minimization") breaks else NULL
      )
      tibble::tibble(sim = sim, method = method, trial_metrics(trial, design))
    })
    do.call(rbind, by_method)
  })
}

check_simulation_inputs <- function(design, methods, n_sims, seed) {
  if (!inherits(design, "grace_design")) {
    abort_simulate("{.arg design} must be a {.cls grace_design}.")
  }
  valid_methods <- is.character(methods) && length(methods) > 0 && anyDuplicated(methods) == 0
  if (!valid_methods) {
    abort_simulate("{.arg methods} must be a non-empty set of unique method names.")
  }
  invisible(lapply(methods, rule_for_method))
  if (!is_whole(n_sims) || n_sims < 1) {
    abort_simulate("{.arg n_sims} must be a whole number of at least 1.")
  }
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)) {
    abort_simulate("{.arg seed} must be a single number.")
  }
}

#' Simulate trials under several allocation methods
#'
#' Simulates `n_sims` trials. In each simulation, one stream of synthetic
#' participants is made with `generator` and every method allocates that same
#' stream (common random numbers). Each trial is summarized by balance, size, and
#' predictability metrics at the run-in, at each forced total, and at the end.
#' The user's random number state is left unchanged.
#'
#' The methods are `"mmsb"` (the modified minimal sufficient balance algorithm),
#' `"srs"` (simple random), `"blocks"` (permuted blocks), `"minimization"`
#' (Pocock-Simon), and `"msb"` (unmodified MSB). Minimization splits each
#' continuous variable into three bins at the tertiles of 1000 draws from
#' `generator`.
#'
#' @param design A [grace_design()].
#' @param methods Character vector of unique method names.
#' @param n_sims Number of simulated trials, a whole number of at least 1.
#' @param generator A function of `n` that returns a tibble of `n` participants,
#'   such as one from [design_generator()].
#' @param seed A single number that fixes all random draws. Required.
#' @param parallel Reserved for parallel runs. `TRUE` is not available yet.
#'
#' @return A tibble of class `grace_sim` with columns `sim`, `method`, `point`,
#'   `metric`, and `value`, and attributes `design`, `seed`, `n_sims`, and
#'   `methods`.
#' @export
#' @examples
#' design <- example_design(40)
#' simulate_trials(design, methods = c("srs", "mmsb"), n_sims = 2, seed = 1)
simulate_trials <- function(
  design,
  methods = c("mmsb", "srs", "blocks", "minimization", "msb"),
  n_sims = 1000,
  generator = design_generator(design),
  seed,
  parallel = FALSE
) {
  if (missing(seed)) {
    abort_simulate("{.arg seed} is required.")
  }
  check_simulation_inputs(design, methods, n_sims, seed)
  if (isTRUE(parallel)) {
    abort_simulate("Parallel runs will be added in GRACE-23.")
  }
  breaks <- if ("minimization" %in% methods) minimization_breaks(generator, design, seed)
  seeds <- withr::with_seed(seed, sample.int(.Machine$integer.max, n_sims))
  per_sim <- lapply(
    seq_len(n_sims),
    \(sim) simulate_one(sim, seeds[[sim]], methods, generator, design, breaks)
  )
  results <- do.call(rbind, per_sim)
  structure(
    results,
    class = c("grace_sim", class(results)),
    design = design,
    seed = seed,
    n_sims = n_sims,
    methods = methods
  )
}
