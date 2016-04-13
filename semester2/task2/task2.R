# Общие переменные
set.seed(0)
alphas <- seq(0.05, 0.25, by=0.05)
N <- 100

# Первая часть задания
volumes <- c(10, 20, 100)
eps <- 0.1

cat("volume", "alpha", "median", "trMean", "\n", sep="\t")
for(v in volumes){
    for(alpha in alphas){
        median <- 0.0
        trMean <- 0.0
        for(i in 1:N){
            x <- c(rnorm(v*(1-eps), 0, 1), rnorm(v*eps, 0, 3))
            median <- median + median(x)
            trMean <- trMean + mean(x, trim=alpha)
        }
        median <- median / N
        trMean <- trMean / N
        # cat(v, alpha, median, trMean, "\n", sep="\t")
    }
}

# Вторая часть задания
epses <- seq(0.0, 1.0, by=0.01)
volume <- 100
lenAlphas <- length(alphas)

medians <- c()
trMeans <- vector(mode="list", length=lenAlphas)
trMeanAccumulaters <- as.list(rep(0.0, lenAlphas))
for(eps in epses){
    for(n in 1:N){
        x <- c(rnorm(volume*(1-eps), 0, 1), rnorm(volume*eps, 1000, 1))
        median <- median + median(x)
        # накапливаем усечённые средние для каждого alpha
        for(i in 1:lenAlphas){
            alpha <- alphas[i]
            trMeanAccumulaters[[i]] <- trMeanAccumulaters[[i]] + mean(x, trim=alpha)
        }
    }
    # сохраняем средние значения для построения графиков
    median <- median / N
    medians <- c(medians, median)
    for(i in 1:lenAlphas){
        trMeanAccumulaters[[i]] <- trMeanAccumulaters[[i]] / N
        trMeans[[i]] <- c(trMeans[[i]], trMeanAccumulaters[[i]])
    }
}
# print(medians) # для отладки
plot(epses, medians, type="o", pch=18, col="blue", lty=1)

# вывод для усечённого среднего
colors <- rainbow(lenAlphas)
plot(epses, trMeans[[1]], type="l", col=colors[1])
for(i in 2:lenAlphas){
    lines(epses, trMeans[[i]], type="l", col=colors[i])
}
linetype <- rep("l", lenAlphas) 
plotchar <- seq(18,18+lenAlphas,1)
legend("topleft", "(x,y)", alphas, cex=1, col=colors, lty=1, title="Усечённое среднее")
