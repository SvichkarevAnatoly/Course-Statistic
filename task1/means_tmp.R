sample_file_names = c('n.txt', 'u.txt')
sample_names = c('normal', 'normal_ejection', 'uniform', 'uniform_ejection')

for (i in 1:length(sample_file_names)){
  print(sample_file_names[i])
  
  sample <- read.table(sample_file_names[i])
  sample <- as.vector(t(nlist))
  
  mean = mean(sample)
  median = median(sample)
  half_sum_extrems = (max(sample) + min(sample)) / 2
  half_sum_quantiles = (quantile(sample)[["25%"]] + quantile(sample)[["75%"]]) / 2
  
  params <- data.frame(name='normal',
                       mean=mean,
                       median=median,
                       extremums = half_sum_extrems,
                       quantile = half_sum_quantiles)  
}

# выборка из нормального распределения
nlist <- read.table("n.txt")
nlist <- as.vector(t(nlist))

mean = mean(nlist)
median = median(nlist)
half_sum_extrems = (max(nlist) + min(nlist)) / 2
half_sum_quantiles = (quantile(nlist)[["25%"]] + quantile(nlist)[["75%"]]) / 2

n_params <- data.frame(name='normal',
                      mean=mean,
                      median=median,
                      extremums = half_sum_extrems,
                      quantile = half_sum_quantiles)

# выборка из равномерного распределения
sample <- read.table("u.txt")
sample <- as.vector(t(sample))

mean = mean(sample)
median = median(sample)
half_sum_extrems = (max(sample) + min(sample)) / 2
half_sum_quantiles = (quantile(sample)[["25%"]] + quantile(sample)[["75%"]]) / 2

u_params <- data.frame(name='uniform',
                      mean=mean,
                      median=median,
                      extremums = half_sum_extrems,
                      quantile = half_sum_quantiles)

params <- rbind(n_params, u_params)
