# библиотека для использования генерации двумерного нормального распределения
library(MASS)

# для генерации одинаковой выборки
set.seed(0)

create_sample <- function(ro){
  # генерация выборки из 40 элементов с нулевым матожиданием по обоим осям
  # матрица сигма для задания дисперсии и корреляции по осям
  # по диагонали стоит дисперсия, два другие элемента обозначают корреляцию,
  # empirical отвечает за соотношение корреляции при маленьких размерах выборок
  cat("ro = "); print(ro)
  sample <- mvrnorm(40, mu = c(0, 0), Sigma = matrix(c(1, ro, ro, 1), 2), empirical = TRUE)
  #plot(sample)
  return(sample)
}

compute_coefs <- function(sample){
  plot(sample, xlab = "X", ylab = "Y")
  # коэффициент корреляции Пирсона:
  cat("коэффициент корреляции Пирсона:\n")
  r <- cor(sample[,1], sample[,2])
  print(r)
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
  rQ <- cor_sgn_median(sample)
  print(rQ)
  
  # ранговый коэффициент корреляции Спирмена:
  cat("ранговый коэффициент корреляции Спирмена:\n")
  rs <- cor(sample[,1], sample[,2], method = "spearman")
  print(rs)
  #cor.test(sample[,1], sample[,2], method = "spearman")
  
  # коэффициент Кендела:
  cat("коэффициент Кендела:\n")
  t <- cor(sample[,1], sample[,2], method = "kendall")
  print(t)
  #cor.test(sample[,1], sample[,2], method = "kendall")
  cat("\n")
  
  # компенсация смещения
  cat("компенсация знакового коэффициента корреляции:\n")
  rQ_star <- sin((pi * rQ) / 2)
  print(rQ_star)
  
  cat("компенсация рангового коэффициента корреляции Спирмена:\n")
  rs_star <- 2*sin((pi * rs) / 6)
  print(rs_star)
  
  cat("компенсация коэффициента Кендела:\n")
  t_star <- sin((pi * t) / 2)
  print(t_star)
  
  cat("----\n")
}

# добавление 2 фиксированных выбросов в выборки
add_emissions <- function(sample){
  sample <- rbind(sample, c(-10, 10))
  sample <- rbind(sample, c( 10, -10))
  return(sample)
}

# вычисление коэффициентов для оригинальных выборок
cat("вычисление коэффициентов для оригинальных выборок:\n")
s0 <- create_sample(0)
compute_coefs(s0)

s1 <- create_sample(0.5)
compute_coefs(s1)

s2 <- create_sample(0.9)
compute_coefs(s2)

# вычисление коэффициентов для загрязнных выборок
cat("вычисление коэффициентов для загрязнных выборок:\n")
s0_e <- add_emissions(s0)
cat("ro = "); print(0)
compute_coefs(s0_e)

s1_e <- add_emissions(s1)
cat("ro = "); print(0.5)
compute_coefs(s1_e)

s2_e <- add_emissions(s2)
cat("ro = "); print(0.9)
compute_coefs(s2_e)