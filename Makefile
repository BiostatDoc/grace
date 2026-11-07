.PHONY: format lint test check pre-pr

format:
	Rscript -e "styler::style_pkg()"

lint:
	Rscript -e "pkgload::load_all(quiet = TRUE); \
		l <- lintr::lint_package(); \
		print(l); \
		quit(status = length(l) > 0)"

test:
	Rscript -e "devtools::test(stop_on_failure = TRUE)"

check:
	Rscript -e "devtools::check(error_on = 'warning', args = '--no-manual')"

pre-pr: format lint test
