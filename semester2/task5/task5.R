dataSet <- read.csv("data.csv", sep = "\t")
# стандартизация данных
dataStandart <- scale(dataSet)

# определение оптимального числа кластеров по 26 критериям
library(NbClust)
nc <- NbClust(dataStandart, min.nc=2, max.nc=15, method="kmeans")
# table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# кластеризация k-средних для 3 кластеров с 25 запусками
fit.km <- kmeans(dataStandart, 3, nstart=25)
fit.km$size # число экземпляров в каждом кластере
# центры кластеров в стандартизированном виде
fit.km$centers
# центры кластеров в исходных данных
aggregate(dataSet, by=list(cluster=fit.km$cluster), mean)
# вектор номеров кластеров для исходных данных
fit.km$cluster
# append cluster assignment
resultDataSet <- data.frame(dataSet, fit.km$cluster)
resultDataSet
