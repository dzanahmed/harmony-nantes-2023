SRC = $(wildcard Session_*.Rmd)

PDF   = $(SRC:.Rmd=.pdf)
HTML  = $(SRC:.Rmd=.html)
R     = $(SRC:.Rmd=.R)

RENDER_H = @Rscript -e "rmarkdown::render('$<', 'html_document', params=list(presentation=FALSE)); nf <- gsub('.Rmd', '.html', '$<'); file.copy(nf, '../', overwrite=TRUE)"
RENDER_P = @Rscript -e "rmarkdown::render('$<', 'beamer_presentation', params=list(presentation=TRUE)); nf <- gsub('.Rmd', '.pdf', '$<'); file.copy(nf, '../', overwrite=TRUE)"
RENDER_D = @Rscript -e "rmarkdown::render('$<', 'pdf_document', params=list(presentation=FALSE)); nf <- gsub('.Rmd', '.pdf', '$<'); file.copy(nf, '../', overwrite=TRUE)"
PURL = @Rscript -e "knitr::purl('$<', documentation = 2L, output = paste0(tools::file_path_sans_ext('$<'), '.R'))"

%.R:%.Rmd
	$(PURL)
%.html:%.Rmd
	$(RENDER_H)
	-rm -rf Session*.log
%Session_Preparation.pdf:%Session_Preparation.Rmd
	$(RENDER_D)
	-rm -rf Session*.log
%.pdf:%.Rmd
	$(RENDER_P)
	-rm -rf Session*.log

.PHONY: clean
.PHONY: tidy
.PHONY: r
.PHONY: pdf
.PHONY: html
.PHONY: all
	
all: 	$(PDF) $(HTML) $(R)
pdf:	$(PDF)
html:	$(HTML)
r: $(R)
clean:
	-rm -rf Session*.md
	-rm -rf Session*.tex
	-rm -rf Session*.pdf
	-rm -rf Session*.html
	-rm -rf Session*.R
	-rm -rf Session*.log
	-rm -rf Session*_files
tidy:
	-rm -rf Session*.md
	-rm -rf Session*.tex
	-rm -rf Session*.log
	-rm -rf Session*_files
