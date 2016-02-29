set.seed(17031993)

sampleVolumes <- c(10, 20, 100)
meanShifts <- c(0.1, 0.5, 1)

samplesNumber <- length(sampleVolumes)
standartSample <- vector("list", samplesNumber)
for(i in 1:length(sampleVolumes)){
    standartSample[[i]] <- rnorm(sampleVolumes[i], 0, 1)
}

