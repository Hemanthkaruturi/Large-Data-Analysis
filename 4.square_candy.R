load(file = "Data/data_999_boruta_cor.RData")

################################# Log transformation #############################################
#Find variables with negative values
t(sapply(data.999.boruta.cor, range))
#There are no variables with negative values

data.999.boruta.cor.log <- log(data.999.boruta.cor[,c(1:86)])
data.999.boruta.cor.log$y <- data.999[,c(86)]

save(data.999.boruta.cor.log, file = "data_999_boruta_cor_log.RData")

hist(data.999.boruta.cor.log $x002,
     main = "Histogram of x002",
     xlab = "x002")
abline(v =mean(data.999.boruta.cor.log $x003, na.rm = TRUE),col = "royalblue",
       lwd = 2)

boxplot(data.999.boruta.cor.log $x002,
        main = toupper("Boxplot of x002"),
        ylab = "x002",
        col = "blue")

d <- density(data.999.boruta.cor$x002, na.rm = TRUE)
plot(d, main = "Kernel density of x002")
polygon(d, col = "red", border = "blue")
abline(v =mean(data.999.boruta.cor.log $x002, na.rm = TRUE),col = "royalblue",
       lwd = 2)



################################# Square root transformation #####################################
data.999.boruta.cor.sqrt <- sqrt(data.999.boruta.cor[,c(1:86)])
data.999.boruta.cor.sqrt$y <- data.999.boruta.cor$y

save(data.999.boruta.cor.sqrt, file = "data_999_boruta_cor_sqrt.RData")
hist(data.999.boruta.cor.sqrt $x002,
     main = "Histogram of x002",
     xlab = "x002")
abline(v =mean(data.999.boruta.cor.sqrt $x003, na.rm = TRUE),col = "royalblue",
       lwd = 2)

boxplot(data.999.boruta.cor.sqrt $x002,
        main = toupper("Boxplot of x002"),
        ylab = "x002",
        col = "blue")

d <- density(data.999.boruta.cor.sqrt $x002, na.rm = TRUE)
plot(d, main = "Kernel density of x002")
polygon(d, col = "red", border = "blue")
abline(v =mean(data.999.boruta.cor.sqrt $x002, na.rm = TRUE),col = "royalblue",
       lwd = 2)
