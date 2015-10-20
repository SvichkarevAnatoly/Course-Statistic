library(tikzDevice)

file_sample <- 'data/n.txt'

sample <- read.table(file_sample)
sample <- as.vector(t(sample))

tikz('tex/boxplot.tex',width=3.5,height=3.5)
boxplot(sample)
dev.off()