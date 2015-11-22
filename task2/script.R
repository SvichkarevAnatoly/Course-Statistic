# ядерная функция
K <- function(u){
  result <- exp(-u^2/2) / sqrt(2*pi)
  return(result)
}

# вычисление ядерной оценки плотности по выборке в опорных точках
f <- function(dataset, h, x_range_vector=seq(-4, 4, length=100)){
  result <- double(length(x_range_vector))
  for( j in x_range_vector){
    x_range_value <- x_range_vector[j]
    sum_K <- 0
    for(xi in dataset){
      sum_K <- sum_K + K((x_range_value - xi) / h)
    }
    result[j] <- sum_K / (length(dataset) * h)
  }
  return(result)
}

# отрисовка графика плотности нормального распределения и ядерной оценки
plot_kde <- function(x_range, h, color, label){
  hx <- dnorm(x_range) # нормальное распределение
  plot(x_range, hx, xlab="x", ylab="Density", lwd=2, type="l")
  sx <- f(x, h, x_range)
  lines(x_range, sx, lwd=2, col=color)
  legend("topright", legend=c("Normal", label), lwd=2, col=c("black", color))
}

# считали выборку и преобразовали в вектор
sample <- read.table("sample_N_0_1.txt")
sample <- as.vector(t(sample))

# вычисляем h silverman'а
x <- sample
x_mean <- mean(x)
n <- length(x)
S <- sqrt(sum((x - x_mean)^2)/length(x))
h <- S / n^(1/5)

# создаём вектор опорных точек для построения нормального распределения и ядерных оценок
x_range <- seq(-4, 4, length=200)

# построение графиков
plot_kde(x_range, h, "red", "h = h_silverman")
plot_kde(x_range, 2*h, "blue", "h = 2 * h_silverman")
plot_kde(x_range, h/2, "green", "h = h_silverman / 2")
