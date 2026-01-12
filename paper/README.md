# LambdaScript Paper

This folder contains the academic paper describing the LambdaScript programming language.

## Files

- `lambdascript_paper.tex` - Main LaTeX document
- `references.bib` - Bibliography file
- `Makefile` - Compilation automation

## Compilation

### Using Make (Recommended)

```bash
# Compile the paper
make

# Quick compile (no bibliography)
make quick

# View the PDF
make view

# Clean auxiliary files
make clean

# Continuous compilation (requires latexmk)
make watch

# See all options
make help
```

### Manual Compilation

```bash
# With bibliography
pdflatex lambdascript_paper.tex
bibtex lambdascript_paper
pdflatex lambdascript_paper.tex
pdflatex lambdascript_paper.tex

# Quick compile (no bibliography)
pdflatex lambdascript_paper.tex
```

## Requirements

- LaTeX distribution (TeX Live, MacTeX, MiKTeX)
- pdflatex
- bibtex
- Optional: latexmk (for watch mode)

### Installing LaTeX

**macOS:**
```bash
brew install --cask mactex
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get install texlive-full
```

## Structure

The paper includes:
- Abstract
- Introduction
- Language Design
- Implementation Details
- Related Work
- Conclusion

Feel free to modify the structure as needed for your paper.

