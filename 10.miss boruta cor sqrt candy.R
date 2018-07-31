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

###################################### Lasso Model ############################################
library(glmnet)
library(Metrics)
set.seed(123)
lasso_regressor = cv.glmnet(as.matrix(data.miss.boruta.cor.sqrt.train[, -87]), data.miss.boruta.cor.sqrt.train$y)

## Predictions
lasso_pred <- predict(lasso_regressor, newx = as.matrix(data.miss.boruta.cor.sqrt.test[, -87]), s = "lambda.min")

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.sqrt.test$y, predicteds=lasso_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9235009 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.9437862
mape <- mean(abs((lasso_pred - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.05968235


#Calculate RMSE 

rmse <- sqrt(mean((data.miss.boruta.test$y - lasso_pred)^2))
#44.63455

######################################### GBM ################################################
library(iterators)
library(parallel)
library(doMC)
library(caret)
set.seed(222)
## detectCores() returns 16 cpus
registerDoMC(16)
## Set up caret model training parameters
CARET.TRAIN.CTRL <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                                 verboseIter = FALSE, allowParallel = TRUE)
gbmFit <- train(y ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
                trControl = CARET.TRAIN.CTRL, tuneGrid = expand.grid(n.trees = (4:10) * 
                                                                       50, interaction.depth = c(5), shrinkage = c(0.05), n.minobsinnode = c(10)), 
                data = data.miss.boruta.cor.sqrt.train, verbose = FALSE)

## print(gbmFit)

## Predictions
preds1 <- predict(gbmFit, newdata = data.miss.boruta.cor.sqrt.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.sqrt.test$y, predicteds=preds1)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) # 0.9548024 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.9579674
mape <- mean(abs((preds1 - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.04394755


#Calculate RMSE 

rmse <- sqrt(mean((data.miss.boruta.cor.sqrt.test$y - preds1)^2))
#34.58712

#################################### XGBOOST #################################################
library(xgboost)
set.seed(123)
## Model parameters trained using xgb.cv function
xgbFit = xgboost(data = as.matrix(data.miss.boruta.cor.sqrt.train[, -87]), nfold = 5, label = as.matrix(data.miss.boruta.cor.sqrt.train$y), 
                 nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
                 nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 6, min_child_weight = 1.7817, 
                 subsample = 0.5213, colsample_bytree = 0.4603)
## print(xgbFit)

## Predictions
xgb_pred <- predict(xgbFit, newdata = as.matrix(data.miss.boruta.cor.sqrt.test[,c(-87)]))

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.cor.sqrt.test$y, predicteds=xgb_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.960546 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.9608608
mape <- mean(abs((xgb_pred - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.04091538


#Calculate RMSE 

rmse <- sqrt(mean((data.miss.boruta.test$y - xgb_pred)^2))
#32.33968
