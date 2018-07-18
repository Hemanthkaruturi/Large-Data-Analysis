#Project V 0.1

#Enabling cores
library(doMC)
registerDoMC(cores = 8)

#To know memory usage of variables
#install.packages('pryr')
library('pryr')

#making results numeric
options(scipen = 999)

#Importing Data
library(data.table)
data <- fread('dataset_00_with_header.csv')
data01 <- data[,1:10]
data02 <- data[,11:20]
data03 <- data[,21:30]
data04 <- data[,31:40]
data05 <- data[,41:50]
data06 <- data[,51:60]
data07 <- data[,61:70]
data08 <- data[,71:80]
data09 <- data[,81:90]
data10 <- data[,91:100]
data11 <- data[,101:110]
data12 <- data[,111:120]
data13 <- data[,121:130]
data14 <- data[,131:140]
data15 <- data[,141:150]
data16 <- data[,151:160]
data17 <- data[,161:170]
data18 <- data[,171:180]
data19 <- data[,181:190]
data20 <- data[,191:200]
data21 <- data[,201:210]
data22 <- data[,211:220]
data23 <- data[,221:230]
data24 <- data[,231:240]
data25 <- data[,241:250]
data26 <- data[,251:260]
data27 <- data[,261:270]
data28 <- data[,271:280]
data29 <- data[,281:290]
data30 <- data[,291:300]
data31 <- data[,301:304]
data.y <- data$y

object_size(data01)


#FInding missing values
sort(colSums(is.na(data01)), decreasing = TRUE)

#finding range of all vaiables having missing values
data.miss <- data01[,c(2,3,4,5)]
data.frame(min=sapply(data.miss,min, na.rm=T),max=sapply(data.miss,max,na.rm=T))

#visualizing missing values
#This is not working properly on all versions but it looks nice
#install.packages('naniar')
library('naniar')
vis_miss(data01, warn_large_data = FALSE)

#for visualizing missing values
library(Amelia)
missmap(data01)

#Not working in R 3.5.1
library(VIM)
aggr(data01, prop = F, numbers = T)

#for visualizing missing values
install.packages("RColorBrewer")
library(RColorBrewer)
plot_missing(data01, 5, col = brewer.pal(n = 9, name = "Blues"))


#finding percentage of sparsity of the data
library(textTinyR)
sp_mat = dense_2sparse(as.matrix(data01))
dbl = matrix_sparsity(sp_mat)

#Visualizing sparcity of the matrix
install.packages('SparseM')
library('SparseM')
image(as.matrix(data),col=c("white","gray"),
      xlab="column", ylab="row")

#Imputing missing values
#install.packages('missForest')
library('missForest')
train <- data01
train.imp <- missForest(train[,c(2,3,4,5)])
train[,c(2,3,4,5)] <- train.imp$ximp
