#miss_Boruta_cor_rm_out.imp_candy

#Import data
load(file = "Data/data_miss_boruta_cor_rm_out_imp.RData")

# Remove x079,x086,x089,x101,x147,x175 because they are constant values
#These values become constant when outliers are deleted
which( colnames(data.miss.boruta.cor.rm_out.imp)=="x175" )
data.miss.boruta.cor.rm_out.imp <- data.miss.boruta.cor.rm_out.imp[,c(-36,-38,-39,-42,-51,-55)]

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data.miss.boruta.cor.rm_out.imp$y, SplitRatio = 2/3)
data.miss.boruta.cor.rm_out.imp.train = subset(data.miss.boruta.cor.rm_out.imp, split == TRUE)
data.miss.boruta.cor.rm_out.imp.test = subset(data.miss.boruta.cor.rm_out.imp, split == FALSE)



############################################# Model Building ###################################################
#support vector regression
library(e1071)
svm_regressor = svm(formula = y ~ .,
                    data = data.miss.boruta.cor.rm_out.imp.train,
                    type = 'eps-regression',
                    kernel = 'radial')

#Predicting the model
y_pred = predict(svm_regressor, newdata = data.miss.boruta.cor.rm_out.imp.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.rm_out.imp.test$y, predicteds=y_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9409487 highly positive correlation

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
predictionRMSE <- rmse(error)   # 24.65957

rmse <- sqrt(mean(data.miss.boruta.cor.rm_out.imp$y - y_pred)^2)

########################################### Decision Tree #####################################################
#decision tree regression
# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
dt_regressor = rpart(formula = y ~ .,
                     data = data.miss.boruta.cor.rm_out.imp.train,
                     method = "anova",
                     control = rpart.control(minsplit = 5))

#Predicting results
dt_pred = predict(dt_regressor, newdata = data.miss.boruta.cor.rm_out.imp.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.rm_out.imp.test$y, predicteds=dt_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.8654772 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.boruta.cor.rm_out.imp.test$y - dt_pred)^2))
#58.25347

########################################### Random Forest ######################################################
# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
rf_regressor = randomForest(x = data.miss.boruta.cor.rm_out.imp.train[,-81],
                            y = data.miss.boruta.cor.rm_out.imp.train$y,
                            ntree = 200)

#Predicting results
rf_pred = predict(rf_regressor, newdata = data.miss.boruta.cor.rm_out.imp.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.rm_out.imp.test$y, predicteds=rf_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.949127 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.boruta.cor.rm_out.imp.test$y - rf_pred)^2))
#37.09202

