library(tools)

args<-commandArgs(TRUE)

computeParams <- function(file){
  sample <- read.table(file)
  sample <- as.vector(t(sample))
  
  mean = mean(sample)
  median = median(sample)
  half_sum_extrems = (max(sample) + min(sample)) / 2
  half_sum_quantiles = (quantile(sample)[["25%"]] + quantile(sample)[["75%"]]) / 2
  
  result <- data.frame(mean=mean,
                       median=median,
                       extremums = half_sum_extrems,
                       quantile = half_sum_quantiles)
  
  return(result)
}

# file_sample <- file.choose()
file_sample <- args[1]
params <- computeParams(file_sample)

output_file_name <- paste(file_path_sans_ext(file_sample), '_params.csv', sep="")
write.table(params, file = output_file_name, row.names=FALSE, na="", sep="\t")
