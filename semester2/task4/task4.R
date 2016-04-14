library(psych)

corr <- read.csv("corr.csv", row.names = 1, sep = "\t")
# Диаграмма каменистой осыпи с параллельным анализом
fa.parallel(corr, fa="fa")

# Выделение общих факторов
fa <- fa(corr, nfactors=2, rotate="none", fm="ml")
fa
