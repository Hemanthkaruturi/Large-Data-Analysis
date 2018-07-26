load(file = "Data/data_999_boruta_cor.RData")
load(file = "Data/data_miss_boruta_cor.RData")

################################### Replacing Outliers with sd ###################################
findOutlier <- function(data, cutoff = 3) {
  ## Calculate the sd
  sds <- apply(data, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  result <- mapply(function(d, s) {
    which(d > cutoff * s)
  }, data, sds)
  result
}

outliers <- findOutlier(data.miss.boruta.cor[,c(-79,-78,-87)])
outliers

removeOutlier <- function(data, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <- NA
    return(res)
  }, data, outliers)
  return(as.data.frame(result))
}

data.miss.boruta.cor.rm_out <- removeOutlier(data.miss.boruta.cor[,c(-79,-78,-87)], outliers)
data.miss.boruta.cor.rm_out$x284 <- data.miss.boruta.cor$x284
data.miss.boruta.cor.rm_out$x283 <- data.miss.boruta.cor$x283
data.miss.boruta.cor.rm_out$y <- data.miss.boruta.cor$y

sort(colSums(is.na(data.miss.boruta.cor.rm_out.imp)), decreasing = TRUE)

save(data.miss.boruta.cor.rm_out, file = "data_miss_boruta_cor_rm_out.RData")

##### Imputing missing values
library(missForest)
data.imp <- missForest(data.miss.boruta.cor.rm_out)
data.miss.boruta.cor.rm_out.imp <- data.imp$ximp
save(data.miss.boruta.cor.rm_out.imp, file = "data_miss_boruta_cor_rm_out_imp.RData")
