SHELL := /bin/bash

report=report1
boxplot=boxplot

read:	pdf clean_tmp
	evince ${report}.pdf &
	
pdf:	${report}.tex boxplot 
	pdflatex ${report}.tex

boxplot:
	Rscript ../boxplot.R

clean_tmp:
	rm -f ${report}.{ps,log,aux,out,dvi,bbl,blg} ${boxplot}.tex

clean:
	rm -f ${report}.{ps,pdf,log,aux,out,dvi,bbl,blg} ${boxplot}.tex
