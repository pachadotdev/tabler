.PHONY: install docs

install:
	@Rscript -e 'tinydev::pkg_install(".")'

docs:
	@Rscript -e 'tinydev::pkg_document("."); pkgsite::build_site("./")'

format:
	@Rscript -e 'styler::style_pkg()'
