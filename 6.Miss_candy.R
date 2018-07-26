#Using sample5
#Enabling cores
library(doMC)
registerDoMC(cores = 8)

#Importing data
load(file = "Data/datamiss.RData")

################################### sqrt Transformation #########################################################
#Find variables with negative values
t(sapply(data.miss, range))
#Variables with negative values are x043, x236
#we can square these variables
data.miss$x043 <- data.miss$x043^2
data.miss$x236 <- data.miss$x236^2

data.miss.sqrt <- sqrt(data.miss[,c(1:276)])
data.miss.sqrt$y <- data.miss$y

# # sort(colSums(is.na(data.miss)), decreasing = TRUE)
# #check whether there are variables with zero variance
which(apply(data.miss.sqrt, 2, var)==0)
# #We can not apply PCA when the variable's variance is zero hence i ma removing that zero
# #variance variable
data.miss.sqrt <- data.miss.sqrt[ , apply(data.miss.sqrt, 2, var) != 0]


#save(data.miss.log, file = "data_miss_log.RData")
# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data.miss.sqrt$y, SplitRatio = 2/3)
data.miss.sqrt.train = subset(data.miss.sqrt, split == TRUE)
data.miss.sqrt.test = subset(data.miss.sqrt, split == FALSE)



############################################# Model Building ###################################################
#support vector regression
library(e1071)
svm_regressor = svm(formula = y ~ .,
                    data = data.miss.sqrt.train,
                    type = 'eps-regression',
                    kernel = 'radial')


#Predicting the model
svm_pred = predict(svm_regressor, newdata = data.miss.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.sqrt.test$y, predicteds=svm_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9582 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- dt_regressor$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 18.61278

rmse <- sqrt(mean((data.miss.sqrt.test$y - y_pred)^2))
########################################### Decision Tree #####################################################
#decision tree regression
# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
dt_regressor = rpart(formula = y ~ .,
                     data = data.miss.sqrt.train,
                     method = "anova",
                     control = rpart.control(minsplit = 5))

#Predicting results
dt_pred = predict(dt_regressor, newdata = data.miss.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.sqrt.test$y, predicteds=dt_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.8499586 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.sqrt.test$y - dt_pred)^2))
#61.41092

########################################### Random Forest ######################################################
# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
rf_regressor = randomForest(x = data.miss.sqrt.train[,-276],
                            y = data.miss.sqrt.train$y,
                            ntree = 200)

#Predicting results
rf_pred = predict(rf_regressor, newdata = data.miss.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.sqrt.test$y, predicteds=rf_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9553267 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.sqrt.test$y - rf_pred)^2))

############################################ K fold cross validation ############################################
library(caret)
train.control <- trainControl(method = "cv", number = 10)
kf <- train(y ~., data = data.miss.sqrt.train, method = "rf",
            trControl = train.control)

#Predicting results
kf_pred = predict(kf, newdata = data.miss.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.sqrt.test$y, predicteds=kf_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9553267 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.sqrt.test$y - kf_pred)^2))
#34.50712
