document:
	Rscript -e "devtools::document()"

readme:
	Rscript -e "rmarkdown::render('README.Rmd'); pkgdown::build_home()"

build:
	Rscript -e "devtools::build()"

test:
	Rscript -e "devtools::test()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install(build_vignettes = TRUE, upgrade_dependencies = FALSE)"

winbuild:
	Rscript -e "devtools::build_win(version = 'R-devel', quiet = TRUE)"

pkgdown:
	Rscript -e "pkgdown::build_site(run_dont_run = TRUE)"
