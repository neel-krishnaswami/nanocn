syntax.pdf: syntax.tex
	pdflatex -interaction=nonstopmode syntax.tex

syntax.tex: syntax.ott
	ott -i syntax.ott -o syntax.tex

clean:
	rm -f syntax.aux syntax.log syntax.pdf syntax.tex syntax_ott.tex

.PHONY: clean
