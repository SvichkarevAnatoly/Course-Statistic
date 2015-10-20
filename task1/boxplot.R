library(tikzDevice)

readSample <- function(file_path){
  sample <- read.table(file_path)
  sample <- as.vector(t(sample))
  return(sample)
}

sample_n <- readSample('data/n.txt')
sample_ne <- readSample('data/ne.txt')

tikz('tex/boxplot.tex',width=3.5,height=3.5)
boxplot(sample_n, sample_ne)
dev.off()