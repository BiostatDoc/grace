package_is_installed <- function() {
  nzchar(system.file("Meta", "package.rds", package = "graceDR"))
}

install_package_for_workers <- function(env = parent.frame()) {
  if (package_is_installed()) {
    return(invisible(FALSE))
  }
  library_path <- withr::local_tempdir(.local_envir = env)
  status <- withr::with_envvar(
    c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    system2(
      file.path(R.home("bin"), "R"),
      c(
        "CMD", "INSTALL",
        "--no-docs", "--no-multiarch", "--no-test-load",
        paste0("--library=", shQuote(library_path)),
        shQuote(normalizePath(test_path("..", "..")))
      ),
      stdout = FALSE,
      stderr = FALSE
    )
  )
  if (!identical(status, 0L)) {
    cli::cli_abort("Could not install the package for the parallel workers.")
  }
  withr::local_libpaths(library_path, action = "prefix", .local_envir = env)
  invisible(TRUE)
}
