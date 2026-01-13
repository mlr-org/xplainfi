PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

all: format doc README.md install check

.PHONY: format
format:
	@echo "Formatting with $(shell air --version)"
	@air format .

.PHONY: doc
doc: README.md
	Rscript -e "usethis::use_tidy_description()"
	Rscript -e "devtools::document()"

.PHONY: build
build:
	Rscript -e "devtools::build()"

.PHONY: vignettes
vignettes:
	Rscript -e "devtools::build_vignettes()"

.PHONY: install
install: doc
	Rscript -e "pak::local_install()"

.PHONY: deps 
deps:
	Rscript -e "pak::local_install_dev_deps()"

# devtools::check() automatically redocuments, so no need to add doc here
.PHONY: check
check:
	Rscript -e "devtools::check()"

.PHONY: check-remote
check:
	Rscript -e "devtools::check(remote = TRUE)"

.PHONY: test
test:
	Rscript -e "devtools::test(reporter = 'summary')"

coverage:
	Rscript -e "covr::report(covr::package_coverage(\".\"), file = \"coverage.html\")"

.PHONY: site
site:
	Rscript -e "pkgdown::build_site()"

README.md: README.Rmd
	Rscript -e "rmarkdown::render('README.Rmd')"
	rm README.html

clean:
	fd -HI ".*(_cache|_files|\.html)" vignettes -X rm -r
	rm -r docs
	rm -rf lib
	rm coverage.html
