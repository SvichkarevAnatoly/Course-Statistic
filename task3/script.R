# библиотека для использования генерации двумерного нормального распределения
library(MASS)

# для генерации одинаковой выборки
set.seed(0)

compute_coefs <- function(ro){
  # генерация выборки из 40 элементов с нулевым матожиданием по обоим осям
  # матрица сигма для задания дисперсии и корреляции по осям
  # по диагонали стоит дисперсия, два другие элемента обозначают корреляцию,
  # empirical отвечает за соотношение корреляции при маленьких размерах выборок
  cat("ro = "); print(ro)
  sample <- mvrnorm(40, mu = c(0, 0), Sigma = matrix(c(1, ro, ro, 1), 2), empirical = TRUE)
  #plot(sample)
  
  # коэффициент корреляции Пирсона:
  cat("коэффициент корреляции Пирсона:\n")
  print(cor(sample[,1], sample[,2]))
  #cor.test(sample[,1], sample[,2])
  
  # реализовал функцию sgn, чтобы в нуле давал +!
  sgn <- function(x){
    if (sign(x))
      return(sign(x))
    else
      return(1)
  }
  
  # знаковый коэффициент корреляции:
  cor_sgn_median <- function(sample){
    result <- 0
    medx <- median(sample[,1])
    medy <- median(sample[,2])
    for(i in 1:nrow(sample)){
      result <- result + sgn(sample[i,1] - medx) * sgn(sample[i, 2] - medy)
    }
    
    return(result / nrow(sample))
  }
  cat("знаковый коэффициент корреляции:\n")
  print(cor_sgn_median(sample))
  
  # ранговый коэффициент корреляции Спирмена:
  cat("ранговый коэффициент корреляции Спирмена:\n")
  print(cor(sample[,1], sample[,2], method = "spearman"))
  #cor.test(sample[,1], sample[,2], method = "spearman")
  
  # коэффициент Кендела:
  cat("коэффициент Кендела:")
  print(cor(sample[,1], sample[,2], method = "kendall"))
  #cor.test(sample[,1], sample[,2], method = "kendall")
  cat("\n")
}

compute_coefs(0)
compute_coefs(0.5)
compute_coefs(0.9)