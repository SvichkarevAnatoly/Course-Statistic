# Чтобы генерировались одинаковые выборки
set.seed(17031993)

# Размеры выборок и смещение матожидания
sampleVolumes <- c(10, 20, 100)
meanShifts <- c(0.1, 0.5, 1)

volumesNumber <- length(sampleVolumes)
meansNumber <- length(meanShifts)

standartSample <- vector("list", volumesNumber)
for(i in 1:length(sampleVolumes)){
    standartSample[[i]] <- rnorm(sampleVolumes[i], 0, 1)
}

standartSample[[1]]
mean(standartSample[[1]])
sd(standartSample[[1]])

a1 <- rnorm(10, 0.1, 1)
a2 <- rnorm(10, 0.5, 1)
a2
mean(a2)
sd(a2)

#samples <- matrix(rep(list(), volumesNumber*meansNumber),
#                  nrow = meansNumber, ncol = volumesNumber)
#for(i in 1:volumesNumber){
#    for(j in 1:meansNumber){
        # samples[i, j] <- rnorm(sampleVolumes[i], meanShifts[j], 1)
#        samples[i, j] <- rnorm(3, 0, 1)
#    }
#}
