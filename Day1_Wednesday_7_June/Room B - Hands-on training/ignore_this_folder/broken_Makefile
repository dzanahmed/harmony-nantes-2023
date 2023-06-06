SRC = $(wildcard Session_*.Rmd)

PDF   = $(SRC:.Rmd=.pdf)
HTML  = $(SRC:.Rmd=.html)
SOL  = $(SRC:.Rmd=_solution.html)
R     = $(SRC:.Rmd=.R)

RENDER_H = @Rscript -e "rmarkdown::render('$<', 'html_document', gsub('.Rmd', '.html', gsub('__','Q_','$<')), params=list(solutions=FALSE, latex=FALSE)); nf <- gsub('.Rmd', '.html', '$<'); file.copy(nf, '../', overwrite=TRUE)"
RENDER_S = @Rscript -e "rmarkdown::render('$<', 'html_document', gsub('.Rmd', '_solution.html', gsub('__','S_','$<')), params=list(solutions=TRUE, latex=FALSE)); nf <- gsub('.Rmd', '_solution.html', '$<'); file.copy(nf, '../Solutions/', overwrite=TRUE)"
RENDER_D = @Rscript -e "rmarkdown::render('$<', 'pdf_document', gsub('.Rmd', '.pdf', gsub('__','Q_','$<')), params=list(solutions=FALSE, latex=TRUE)); nf <- gsub('.Rmd', '.pdf', '$<'); file.copy(nf, '../', overwrite=TRUE)"
RENDER_B = @Rscript -e "rmarkdown::render('$<', 'all')"
PURL = @Rscript -e "knitr::purl('$<', documentation = 2L, output = paste0(tools::file_path_sans_ext('$<'), '.R'))"

# TODO: new .PHONY compare which deletes any files under ignore_this_folder which are not identical to files in the appropriate place

%.R:%.Rmd
	$(PURL)
%.html:%.Rmd
	$(RENDER_H)
	-rm -rf Session_*.log
%_solution.html:%.Rmd
	$(RENDER_S)
	-rm -rf Session_*.log
%.pdf:%.Rmd
	$(RENDER_D)
	-rm -rf Session_*.log

.PHONY: clean
.PHONY: tidy
.PHONY: r
.PHONY: pdf
.PHONY: html
.PHONY: all
	
all: 	$(PDF) $(HTML)# $(SOL)
pdf:	$(PDF)
html:	$(HTML) $(SOL)
r: $(R)
clean:
	-rm -rf Session_*.md
	-rm -rf Session_*.tex
	-rm -rf Session_*.pdf
	-rm -rf Session_*.html
	-rm -rf Session_*.R
	-rm -rf Session_*.log
	-rm -rf Session_*_files
	-rm -rf ../Session_*.pdf
	-rm -rf ../Session_*.html
	-rm -rf ../Solutions/Session_*.html
tidy:
	-rm -rf Session_*.md
	-rm -rf Session_*.tex
	-rm -rf Session_*.log
	-rm -rf Session_*_files
	-rm -rf ../Solutions/Session_0_Preparation_solution.html
