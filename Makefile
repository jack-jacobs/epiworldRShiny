docs:
	Rscript --vanilla -e 'devtools::document()'

build:
	cd .. && R CMD build epiworldRShiny

install:
	cd .. && R CMD INSTALL epiworldRShiny_*.tar.gz

run:
	Rscript --vanilla -e 'shiny::runApp(".")'

check:
	cd .. && R CMD check epiworldRShiny_*.tar.gz
