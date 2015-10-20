library(tikzDevice)

# функция считывания выборки
readSample <- function(file_path){
  sample <- read.table(file_path)
  sample <- as.vector(t(sample))
  return(sample)
}

# считать нормальную и загрязнённую выборки
sample_n <- readSample('../data/n.txt')
sample_ne <- readSample('../data/ne.txt')

data <- data.frame(sample_n, sample_ne)

# генерация tex файла с боксплота
tikz('../tex/boxplot.tex',width=3.5,height=3.5)
boxplot(data, names=c("cт.н.р.", "загр."))
dev.off()
