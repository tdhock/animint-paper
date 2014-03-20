HOCKING-animint.pdf: HOCKING-animint.tex refs.bib figure-1.pdf figure-tornado.pdf figure-climate.pdf table-examples.tex figure-design.pdf
# Interactive animation example figures were made by first executing
# figure-*.R, then taking a screenshot saved as screenshot-*.png, then
# adding text annotations in figure-*.sla using Scribus, then
# exporting to figure-*.pdf.
	rm -f *.aux *.bbl
	pdflatex HOCKING-animint
	bibtex HOCKING-animint
	pdflatex HOCKING-animint
	pdflatex HOCKING-animint
HOCKING-animint.tex: HOCKING-animint.Rnw
	echo 'library(knitr);knit("HOCKING-animint.Rnw")'|R --no-save
table-examples.tex: table-examples.R
	R --no-save < $<
