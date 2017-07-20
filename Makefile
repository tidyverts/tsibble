document:
	Rscript -e "devtools::document()"

readme:
	Rscript -e "rmarkdown::render('README.Rmd')"

build:
	Rscript -e "devtools::build()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install(build_vignettes = TRUE, upgrade_dependencies = FALSE)"

winbuild:
	Rscript -e "devtools::build_win(version = 'R-devel', quiet = TRUE)"

pkgdown:
	Rscript -e "pkgdown::clean_site(); pkgdown::build_site()"
