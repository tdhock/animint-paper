HOCKING-animint.pdf: HOCKING-animint.tex refs.bib figure-1.pdf figure-tornado.pdf
	rm -f *.aux *.bbl
	pdflatex HOCKING-animint
	bibtex HOCKING-animint
	pdflatex HOCKING-animint
	pdflatex HOCKING-animint
HOCKING-animint.tex: HOCKING-animint.Rnw
	echo 'library(knitr);knit("HOCKING-animint.Rnw")'|R --no-save
