# Makefile for knitr

# optionally put all RMD files to be compiled to pdf here, separated by spaces
RMD_FILES= $(wildcard *.Rmd)

# location of Rscript
# R_HOME?=C:/PROGRA~1/R/R-43~1.0

# these html's will be compiled from Rmd files
HTMLS= $(RMD_FILES:.Rmd=.html) 

# these R files will be untangled from RNoWeb files
R_FILES= $(RMD_FILES:.Rmd=-purled.R) 

# cache and figure directories
CACHEDIR= cache
FIGUREDIR= figures


.PHONY: all purled clean cleanall open
.SUFFIXES: .Rmd .html .R .tex


# these targets will be made by default
all: $(HTMLS)

# use this to create R files extracted from RNoWeb files
purled: $(R_FILES)

# these tex files will be generate from Rmd files
TEX_FILES = $(RMD_FILES:.Rmd=.tex) 


# remove generated files except html and purled R files
clean:
	rm -f *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.fls
	rm -f $(TEX_FILES)

# run the clean target then remove html and purled R files too
cleanall: clean
	rm -f *.synctex.gz
	rm -f $(HTMLS)
	rm -f $(R_FILES)

# compile a html from a RNoWeb file
# replace Rscript\ with e.g. $(R_HOME)/bin/x64/Rscript\ on Windows if Rscript command is not found
%.html: %.Rmd Makefile
	Rscript\
		-e "require(knitr)" \
		-e "rmarkdown::render(input = '$*.Rmd', output_file = '$*-solution.html')" \
		-e "show.solution = FALSE" \
		-e "rmarkdown::render(input = '$*.Rmd', output_file = '$*-hints.html')"

# extract an R file from an RNoWeb file
%-purled.R: %.Rmd
	Rscript\
		-e "require(knitr)" \
		-e "knitr::purl('$*.Rmd', '$*-purled.R')"

# open all html's
open:
	open -a Skim $(HTMLS)
	