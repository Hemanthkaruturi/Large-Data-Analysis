#miss_boruta_cor_sqrt

load(file = "Data/data_miss_boruta_cor.RData")

t(sapply(data.miss.boruta.cor, range))
#No negative values

#Applying square root
data.miss.boruta.cor.sqrt <- sqrt(data.miss.boruta.cor[,c(-87)])
data.miss.boruta.cor.sqrt$y <- data.miss.boruta.cor$y


# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data.miss.boruta.cor.sqrt$y, SplitRatio = 2/3)
data.miss.boruta.cor.sqrt.train = subset(data.miss.boruta.cor.sqrt, split == TRUE)
data.miss.boruta.cor.sqrt.test = subset(data.miss.boruta.cor.sqrt, split == FALSE)



############################################# Model Building ###################################################
#support vector regression
library(e1071)
svm_regressor = svm(formula = y ~ .,
                    data = data.miss.boruta.cor.sqrt.train,
                    type = 'eps-regression',
                    kernel = 'radial')

#Predicting the model
y_pred = predict(svm_regressor, newdata = data.miss.boruta.cor.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.sqrt.test$y, predicteds=y_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.950783 highly positive correlation

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

error <- svm_regressor$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   #22.67119

########################################### Decision Tree #####################################################
#decision tree regression
# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
dt_regressor = rpart(formula = y ~ .,
                     data = data.miss.boruta.cor.sqrt.train,
                     method = "anova",
                     control = rpart.control(minsplit = 5))

#Predicting results
dt_pred = predict(dt_regressor, newdata = data.miss.boruta.cor.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.sqrt.test$y, predicteds=dt_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.8351215 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.boruta.cor.sqrt.test$y - dt_pred)^2))
#64.04693

########################################### Random Forest ######################################################
# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
rf_regressor = randomForest(x = data.miss.boruta.cor.sqrt.train[,-87],
                            y = data.miss.boruta.cor.sqrt.train$y,
                            ntree = 200)

#Predicting results
rf_pred = predict(rf_regressor, newdata = data.miss.boruta.cor.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.sqrt.test$y, predicteds=rf_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9520397 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.boruta.cor.sqrt.test$y - rf_pred)^2))
#35.86611
