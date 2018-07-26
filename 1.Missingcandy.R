#Using sample5

#Importing data
library(data.table)
library(dplyr)
data.org <- fread("Data/dataset_00_with_header.csv")
data <- sample_n(data.org,5000)

#Finding missing values
sort(colSums(is.na(data.miss.boruta.cor.rm_out)), decreasing = TRUE)

#Removing variables having missing values more than 40%
data <- data[,c(-242,-295,-304,-098,-155,-259,-255,-256,-257,-302,-268,-162,-265,-266
                ,-267,-253,-297,-275,-293,-288,-289,-290,-148)]

#Removing missing values having unique value
data <- data[,c(-67,-94,-95,-96)]

#Removing duplicate columns
data <- data[,c(-276)]

################################# Missing value Treatement #########################
#Imputing missing values with 999
#data[is.na(data)] <- 999 this is not working for data.table
#mutate_all is from dplyr package
data.999 <- mutate_all(data, funs(replace(., is.na(.), 999)))
save(data.999, file = "data999.RData")


#imputing missing values with missforest
library(missForest)
data.miss <- data
#TO find the column number use
which( colnames(data)=="x276" )
data.imp <- missForest(data.miss)
data.miss <- data.imp$ximp
save(data.miss, file = "datamiss.RData")

#Imputation Error
data.imp$OOBerror
#Error rate is 4%
