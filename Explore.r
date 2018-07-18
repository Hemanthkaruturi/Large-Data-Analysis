#Enabling cores
library(doMC)
registerDoMC(cores = 8)

#Importing Data
library(data.table)
data <- fread('dataset_00_with_header.csv', nrows = 30000)


summary(data)




#There are no duplicate varaibles
anyDuplicated(data)

#There are missing values in 41 variables
# variables x255, x256, x257
# variables x265, x266, x267
# variables x289, x290
# variables x241, x257, x258
# variables x237, x238, x239
# variables x002, x003
#variables x244, x245 has same number of missing values

#x242  x295  x304  x098  x155  x259  x255  x256  x257  x302  x268  x162  x265  x266  x267  x253  x297  x275  x293  x288 
#93339 86533 81875 80681 79051 77432 76913 76913 76913 73069 67253 66481 66461 66461 66461 66333 58112 56131 51133 49756 
#x289  x290
#49756 49756
#these varaibles have more than 50% of missing values
sort(colSums(is.na(data)), decreasing = TRUE)

#finding range of all vaiables having missing values
data.frame(min=sapply(data,min, na.rm=T),max=sapply(data,max,na.rm=T))

#Imputing missing values
#install.packages('missForest')
library('missForest')
train <- data
train.imp <- missForest(train[,c(265,266,267,253,297,275,293,288,289,290,148,223,222,41,57,058,237,238,239,287,2,3,4,235,44,45,234,272,5)])
train[,c(265,266,267,253,297,275,293,288,289,290,148,223,222,41,57,058,237,238,239,287,2,3,4,235,44,45,234,272,5)] <- train.imp$ximp

#visualizing missing values
install.packages('naniar')
library('naniar')
vis_miss(data, warn_large_data = FALSE)

#finding percentage of sparsity of the data
library(textTinyR)
sp_mat = dense_2sparse(as.matrix(data))
dbl = matrix_sparsity(sp_mat)


#Visualizing sparcity of the matrix
install.packages('SparseM')
library('SparseM')
image(as.matrix(data),col=c("white","gray"),
      xlab="column", ylab="row")





#Finding important variables using boruta
install.packages('Boruta')
library('Boruta')
boruta.train <- Boruta(y~.,data, doTrace = 2)


