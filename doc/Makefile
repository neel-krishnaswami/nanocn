syntax.pdf: syntax.tex
	pdflatex -interaction=nonstopmode syntax.tex

syntax.tex: syntax.ott
	ott -sort false -i syntax.ott -o syntax.tex
	sed -i 's/left=22.5mm,right=22.5mm,top=20mm,bottom=30mm/left=15mm,right=15mm,top=15mm,bottom=20mm/' syntax.tex

clean:
	rm -f syntax.aux syntax.log syntax.pdf syntax.tex syntax_ott.tex

.PHONY: clean
