# Makefile for LaTeX paper compilation

# Main document name (without .tex extension)
MAIN = lambdascript_paper

# LaTeX compiler
LATEX = pdflatex

# BibTeX compiler
BIBTEX = bibtex

# PDF viewer (adjust based on your OS)
# macOS: open, Linux: xdg-open, Windows: start
VIEWER = open

.PHONY: all clean view watch help bib

# Default target
all: $(MAIN).pdf

# Compile the PDF (with bibliography support)
$(MAIN).pdf: $(MAIN).tex
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex

# Compile with bibliography
bib: $(MAIN).tex references.bib
	$(LATEX) $(MAIN).tex
	$(BIBTEX) $(MAIN)
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex

# Quick compile (no bibliography)
quick: $(MAIN).tex
	$(LATEX) $(MAIN).tex

# Clean auxiliary files
clean:
	rm -f *.aux *.log *.bbl *.blg *.out *.toc *.lof *.lot *.fls *.fdb_latexmk *.synctex.gz

# Clean everything including PDF
cleanall: clean
	rm -f $(MAIN).pdf

# View the PDF
view: $(MAIN).pdf
	$(VIEWER) $(MAIN).pdf

# Watch mode (requires latexmk)
watch:
	latexmk -pdf -pvc $(MAIN).tex

# Help message
help:
	@echo "Available targets:"
	@echo "  all      - Compile the paper (default, no bibliography)"
	@echo "  bib      - Compile with bibliography"
	@echo "  quick    - Quick compile (single pass)"
	@echo "  clean    - Remove auxiliary files"
	@echo "  cleanall - Remove all generated files including PDF"
	@echo "  view     - Open the PDF"
	@echo "  watch    - Continuous compilation (requires latexmk)"
	@echo "  help     - Show this help message"

