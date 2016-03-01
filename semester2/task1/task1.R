# Чтобы генерировались одинаковые выборки
set.seed(17031993)

# =======================================================
# Инициализация выборок и вычисление их характеристик
# =======================================================

# Размеры выборок и смещение матожидания
sampleVolumes <- c(10, 20, 100)
meanShifts <- c(0.1, 0.5, 1)

volumesNumber <- length(sampleVolumes)
meansNumber <- length(meanShifts)

standartSample <- vector("list", volumesNumber)
for(i in 1:length(sampleVolumes)){
    standartSample[[i]] <- rnorm(sampleVolumes[i], 0, 1)
}

samples <- matrix(rep(list(), volumesNumber*meansNumber),
                 nrow = meansNumber, ncol = volumesNumber)
samplesMeans <- matrix(volumesNumber*meansNumber,
                 nrow = meansNumber, ncol = volumesNumber)
samplesSDs <- matrix(volumesNumber*meansNumber,
                 nrow = meansNumber, ncol = volumesNumber)

for(i in 1:volumesNumber){ # по строкам в матрице
    for(j in 1:meansNumber){ # по столбцам в матрице
        samples[[i, j]] <- rnorm(sampleVolumes[i], meanShifts[j], 1)
        samplesMeans[[i, j]] <- mean(samples[[i, j]])
        samplesSDs[[i, j]] <- sd(samples[[i, j]])
    }
}

# =======================================================
# Критерий t-Стьюдента
# =======================================================
TTest <- function(s1, s2){
    s1Mean <- mean(s1)
    s2Mean <- mean(s2)
    s1SD <- sd(s1)
    s2SD <- sd(s2)
    n1 <- length(s1)
    n2 <- length(s2)
    
    t <- (s1Mean - s2Mean) /
        sqrt((s1SD^2)/n1 + (s2SD^2)/n2)
    return(t)
}

# qt(.95, 18)


TTestRunner <- function(){
    cat("t-test:\n")
    for(i in 1:volumesNumber){ # по строкам в матрице
        for(j in 1:meansNumber){ # по столбцам в матрице
            s1 <- samples[[i, j]] # сдвинутая выборка
            s2 <- standartSample[[i]] # оригинальная выборка
            t <- TTest(s1, s2)
            cat("N = ", sampleVolumes[i],
                "\tshift = ", meanShifts[j],
                "\tt = ", t, "\n")
        }
    }
}
TTestRunner()

# Для проверки правильности расчётов t
# t_test(s2,s1)
# t.test(s2,s1, var.equal=TRUE, paired=FALSE)

# =======================================================
# Критерий Колмагорова-Смирнова
# =======================================================

# =======================================================
# Критерий Вилкоксона
# =======================================================
