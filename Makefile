all:
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book', envir = new.env())"

clean:
	rm -r _bookdown_files/
	rm -r docs/
	rm _main.*	
