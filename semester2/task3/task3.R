library(psych)

data <- read.csv("pca.csv", sep = "\t")
# Диаграмма каменистой осыпи с параллельным анализом
fa.parallel(data, fa="pc")

# Выделение главных компонент
pc <- principal(data, nfactors = 2)
pc
