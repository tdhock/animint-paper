HOCKING-animint.pdf: HOCKING-animint.tex refs.bib figure-1.pdf
	pdflatex HOCKING-animint
	bibtex HOCKING-animint
	pdflatex HOCKING-animint
	pdflatex HOCKING-animint
