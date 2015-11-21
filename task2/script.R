K <- function(u){
  result <- exp(-u^2/2) / sqrt(2*pi)
  return(result)
}

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

sample <- read.table("sample_N_0_1.txt")
sample <- as.vector(t(sample))

x <- sample
x_mean <- mean(x)
n <- length(x)
S <- sqrt(sum((x - x_mean)^2)/length(x))
h <- S / n^(1/5)

x_range <- seq(-4, 4, length=200)

hx <- dnorm(x_range)
plot(x_range, hx, type="l")

sx1 <- f(x, h, x_range)
lines(x_range, sx1, col="red")

sx2 <- f(x, h*2, x_range)
lines(x_range, sx2, col="blue")

sx3 <- f(x, h/2, x_range)
lines(x_range, sx3, col="green")
