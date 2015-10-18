SHELL := /bin/bash
filename=report1

read:	pdf clean_tmp
	evince ${filename}.pdf &
	
pdf:	${filename}.tex
	pdflatex ${filename}.tex

clean_tmp:
	rm -f ${filename}.{ps,log,aux,out,dvi,bbl,blg}

clean:
	rm -f ${filename}.{ps,pdf,log,aux,out,dvi,bbl,blg}
