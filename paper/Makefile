.PHONY: all quick view clean bib

TARGET = lambdascript

all: quick

quick:
	pdflatex $(TARGET).tex
	pdflatex $(TARGET).tex

bib:
	pdflatex $(TARGET).tex
	bibtex $(TARGET)
	pdflatex $(TARGET).tex
	pdflatex $(TARGET).tex

view:
	open $(TARGET).pdf

clean:
	rm -f $(TARGET).aux $(TARGET).log $(TARGET).out $(TARGET).pdf $(TARGET).bbl $(TARGET).blg
