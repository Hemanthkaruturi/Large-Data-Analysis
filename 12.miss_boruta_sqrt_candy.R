#miss boruta candy

load("data_miss_boruta.RData")

t(sapply(data.miss.boruta, range))
#Remove negative values
which(colnames(data.miss.boruta)=="x043")
data.miss.boruta <- data.miss.boruta[,c(-173, -40)]


#Applying square root
data.miss.boruta.sqrt <- sqrt(data.miss.boruta[,c(-219)])
data.miss.boruta.sqrt$y <- data.miss.boruta$y



# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data.miss.boruta.sqrt$y, SplitRatio = 2/3)
data.miss.boruta.train = subset(data.miss.boruta.sqrt, split == TRUE)
data.miss.boruta.test = subset(data.miss.boruta.sqrt, split == FALSE)



############################################# Model Building ###################################################
#support vector regression
library(e1071)
svm_regressor = svm(formula = y ~ .,
                    data = data.miss.boruta.train,
                    type = 'eps-regression',
                    kernel = 'radial')

#Predicting the model
y_pred = predict(svm_regressor, newdata = data.miss.boruta.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.test$y, predicteds=y_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9612404 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.9607997
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.04118388


#Calculate RMSE 

rmse <- sqrt(mean((data.miss.boruta.test$y - y_pred)^2))
#32.08094


########################################### Random Forest ######################################################
# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
rf_regressor = randomForest(x = data.miss.boruta.train[,-219],
                            y = data.miss.boruta.train$y,
                            ntree = 200)

#Predicting results
rf_pred = predict(rf_regressor, newdata = data.miss.boruta.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.test$y, predicteds=rf_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.958757 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# 0.9604872
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
#0.04167253

#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.miss.boruta.test$y - rf_pred)^2))
#33.29884

###################################### Lasso Model ############################################
library(glmnet)
library(Metrics)
set.seed(123)
lasso_regressor = cv.glmnet(as.matrix(data.miss.boruta.train[, -219]), data.miss.boruta.train[, 219])

## Predictions
lasso_pred <- predict(lasso_regressor, newx = as.matrix(data.miss.boruta.test[, -219]), s = "lambda.min")

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.test$y, predicteds=lasso_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9474351 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.954024
mape <- mean(abs((lasso_pred - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.04853831


#Calculate RMSE 

rmse <- sqrt(mean((data.miss.boruta.test$y - lasso_pred)^2))
#37.20991

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
                data = data.miss.boruta.train, verbose = FALSE)

## print(gbmFit)

## Predictions
preds1 <- predict(gbmFit, newdata = data.miss.boruta.test)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.test$y, predicteds=preds1)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9637059 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.9625981
mape <- mean(abs((preds1 - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.03904091


#Calculate RMSE 

rmse <- sqrt(mean((data.miss.boruta.test$y - preds1)^2))
#31.0467

#################################### XGBOOST #################################################
library(xgboost)
set.seed(123)
## Model parameters trained using xgb.cv function
xgbFit = xgboost(data = as.matrix(data.miss.boruta.train[, -219]), nfold = 5, label = as.matrix(data.miss.boruta.train$y), 
                 nrounds = 2200, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", 
                 nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 6, min_child_weight = 1.7817, 
                 subsample = 0.5213, colsample_bytree = 0.4603)
## print(xgbFit)

## Predictions
xgb_pred <- predict(xgbFit, newdata = as.matrix(data.miss.boruta.test[,c(-219)]))

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.miss.boruta.test$y, predicteds=xgb_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9686573 highly positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
#0.9653094
mape <- mean(abs((xgb_pred - actuals_preds$actuals))/actuals_preds$actuals) 
# 0.03614742


#Calculate RMSE 

rmse <- sqrt(mean((data.miss.boruta.test$y - xgb_pred)^2))
#28.8841