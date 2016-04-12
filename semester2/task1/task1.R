# Чтобы генерировались одинаковые выборки
set.seed(0)

# =======================================================
# Инициализация выборок и вычисление их характеристик
# =======================================================

# Доверительная вероятность
# мы должны отнять половину от процента хвостов,
# если это 5%, то значит 1-0.025
kConfProb <- 0.975

# Размеры выборок и смещение матожидания
sampleVolumes <- c(10, 20, 100)
meanShifts <- c(0.1, 0.5, 1)

volumesNumber <- length(sampleVolumes)
meansNumber <- length(meanShifts)

# Число степеней свободы
freedomDegrees <- vector(mode="numeric", length=volumesNumber)
for(i in 1:length(sampleVolumes)){
    freedomDegrees[i] <- 2 * sampleVolumes[i] - 2
}

standartSample <- vector("list", volumesNumber)
for(i in 1:length(sampleVolumes)){
    standartSample[[i]] <- rnorm(sampleVolumes[i], 0, 1)
}

samples <- matrix(rep(list(), volumesNumber*meansNumber),
                 nrow = meansNumber, ncol = volumesNumber)
samplesMeans <- matrix(0, nrow = meansNumber, ncol = volumesNumber)
samplesSDs <- matrix(0, nrow = meansNumber, ncol = volumesNumber)

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
# самописный критерий t-Стьюдента
TTest <- function(s1, s2){
    s1Mean <- mean(s1)
    s2Mean <- mean(s2)
    s1SD <- sd(s1)
    s2SD <- sd(s2)
    n1 <- length(s1)
    n2 <- length(s2)
    
    t <- abs(s1Mean - s2Mean) /
        sqrt((s1SD^2)/n1 + (s2SD^2)/n2)
    return(t)
}

# qt(0.975, 18)

# Вычисляет критерий для всех пар
TTestRunner <- function(){
    # Критическое значение
    crits <- vector(mode="numeric", length=volumesNumber)
    for(i in 1:volumesNumber){
        crits[i] <- qt(kConfProb, freedomDegrees[i])
    }

    cat("t-test:\n")
    cat("N\tMeanShift\tt\tCritical\tH0\tEfficiency\n")
    for(i in 1:volumesNumber){ # по строкам в матрице
        for(j in 1:meansNumber){ # по столбцам в матрице
            s1 <- samples[[i, j]] # сдвинутая выборка
            s2 <- standartSample[[i]] # оригинальная выборка

            t <- TTest(s1, s2)
            eff <- (crits[i] - t) / crits[i] # чем больше разница, тем лучше,
            # 0 - худший вариант, <0 просто сравнивать на сколько хуже
            pass <- if (t <= crits[i]) T else F

            cat(sampleVolumes[i], "\t",
                meanShifts[j], "\t",
                t, "\t",
                crits[i], "\t",
                pass, "\t",
                eff, "\n")
        }
    }
}
TTestRunner()

# =======================================================
# Критерий Колмагорова-Смирнова
# =======================================================

# Вычисляет критерий для всех пар
KSTestRunner <- function(){
    # Критическое значение
    # http://onlinelibrary.wiley.com/doi/10.1002/9781119961260.app3/pdf
    # http://dxdy.ru/topic16954.html
    crits <- c(0.40925, 0.29408, 0.13403)

    cat("KS-test:\n")
    cat("N\tMeanShift\tt\tCritical\tH0\tEfficiency\n")
    for(i in 1:volumesNumber){ # по строкам в матрице
        for(j in 1:meansNumber){ # по столбцам в матрице
            s1 <- samples[[i, j]] # сдвинутая выборка
            s2 <- standartSample[[i]] # оригинальная выборка

            D <- ks.test(s1, s2)$statistic
            eff <- (crits[i] - D) / crits[i] # чем больше разница, тем лучше,
            # 0 - худший вариант, <0 просто сравнивать на сколько хуже
            pass <- if (D <= crits[i]) T else F

            cat(sampleVolumes[i], "\t",
                meanShifts[j], "\t",
                D, "\t",
                crits[i], "\t",
                pass, "\t",
                eff, "\n")
        }
    }
}
KSTestRunner()

# =======================================================
# Критерий Вилкоксона
# =======================================================

# qwilcox(0.05, 100, 100)

# Вычисляет критерий для всех пар
WTestRunner <- function(){
    # Критическое значение
    # из книги для 10 и 20, для 100 подогнал по логике
    crits <- c(32, 151, 3000)

    cat("Wilcoxon-test:\n")
    cat("N\tMeanShift\tt\tCritical\tH0\tEfficiency\n")
    for(i in 1:volumesNumber){ # по строкам в матрице
        for(j in 1:meansNumber){ # по столбцам в матрице
            s1 <- samples[[i, j]] # сдвинутая выборка
            s2 <- standartSample[[i]] # оригинальная выборка

            D <- wilcox.test(s1, s2)$statistic
            eff <- (crits[i] - D) / crits[i] # чем больше разница, тем лучше,
            # 0 - худший вариант, <0 просто сравнивать на сколько хуже
            pass <- if (D <= crits[i]) T else F

            cat(sampleVolumes[i], "\t",
                meanShifts[j], "\t",
                D, "\t",
                crits[i], "\t",
                pass, "\t",
                eff, "\n")
        }
    }
}
WTestRunner()
