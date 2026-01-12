# Makefile for LaTeX compilation

# Main document name (without .tex extension)
MAIN = lambdascript

# LaTeX compiler
LATEX = pdflatex

# Default target
all: $(MAIN).pdf

# Compile the PDF (run multiple times to resolve references)
$(MAIN).pdf: $(MAIN).tex
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex

# Clean auxiliary files
clean:
	rm -f $(MAIN).aux $(MAIN).log $(MAIN).out $(MAIN).toc \
	      $(MAIN).bbl $(MAIN).blg $(MAIN).synctex.gz

# Clean all generated files including PDF
cleanall: clean
	rm -f $(MAIN).pdf

# View the PDF (macOS)
view: $(MAIN).pdf
	open $(MAIN).pdf

# Force rebuild
rebuild: cleanall all

.PHONY: all clean cleanall view rebuild

