file_sample <- 'data/n.txt'

sample <- read.table(file_sample)
sample <- as.vector(t(sample))

boxplot(sample)