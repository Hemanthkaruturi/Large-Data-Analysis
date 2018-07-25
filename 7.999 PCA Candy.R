#Using sample5
#Enabling cores
library(doMC)
registerDoMC(cores = 8)

#Importing data
load(file = "data999.RData")

################################### sqrt Transformation #########################################################
#Find variables with negative values
t(sapply(data.999, range))
#Variables with negative values are x043, x236
#we can square these variables
data.999$x043 <- data.999$x043^2
data.999$x236 <- data.999$x236^2

data.999.sqrt <- sqrt(data.999[,c(1:276)])
data.999.sqrt$y <- data.999$y

# sort(colSums(is.na(data.999)), decreasing = TRUE)
#check whether there are variables with zero variance
which(apply(data.999.sqrt, 2, var)==0)
#We can not apply PCA when the variable's variance is zero hence i ma removing that zero
#variance variable
data.999.sqrt <- data.999.sqrt[ , apply(data.999.sqrt, 2, var) != 0]


#save(data.999.log, file = "data_999_log.RData")
# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(data.999.sqrt$y, SplitRatio = 2/3)
data.999.sqrt.train = subset(data.999.sqrt, split == TRUE)
data.999.sqrt.test = subset(data.999.sqrt, split == FALSE)


################################### Applying PCA ##############################################################


data.999.sqrt.pr_pca <- prcomp(data.999.sqrt.train[,c(-276)],
                               center = TRUE,
                               scale. = TRUE)
data.999.sqrt.train.pr_pca <- as.data.frame(predict(data.999.sqrt.pr_pca, data.999.sqrt.train[,c(-276)]))
data.999.sqrt.train.pr_pca$y <- data.999.sqrt.train$y
data.999.sqrt.test.pr_pca <- as.data.frame(predict(data.999.sqrt.pr_pca, data.999.sqrt.test[,c(-276)]))
data.999.sqrt.test.pr_pca$y <- data.999.sqrt.test$y

############################################# Model Building ###################################################
#support vector regression
library(e1071)
svm_regressor = svm(formula = y ~ .,
                    data = data.999.sqrt.train.pr_pca,
                    type = 'eps-regression',
                    kernel = 'radial')

#Predicting the model
y_pred = predict(svm_regressor, newdata = data.999.sqrt.test.pr_pca)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.999.sqrt.test.pr_pca$y, predicteds=y_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9295587 negative correlation

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
predictionRMSE <- rmse(error)   # 18.7114

########################################### Decision Tree #####################################################
#decision tree regression
# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
dt_regressor = rpart(formula = y ~ .,
                     data = data.999.sqrt.train.pr_pca,
                     method = "anova",
                     control = rpart.control(minsplit = 5))

#Predicting results
dt_pred = predict(dt_regressor, newdata = data.999.sqrt.test.pr_pca)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.999.sqrt.test.pr_pca$y, predicteds=dt_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.8640948 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.999.sqrt.test.pr_pca$y - dt_pred)^2))
#58.54799

########################################### Random Forest ######################################################
# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
rf_regressor = randomForest(x = data.999.sqrt.train.pr_pca[,-276],
                            y = data.999.sqrt.train.pr_pca$y,
                            ntree = 200)

#Predicting results
rf_pred = predict(rf_regressor, newdata = data.999.sqrt.test.pr_pca)

#Finding accuracy of the model using correlation
actuals_preds <- data.frame(cbind(actuals=data.999.sqrt.test.pr_pca$y, predicteds=rf_pred)) 
correlation_accuracy <- cor(actuals_preds)
print(correlation_accuracy) #0.9369358 positive correlation

#minimum maximum accuracy
# MinMaxAccuracy=mean(min(actuals,predicteds)/max(actuals,predicteds))
# MeanAbsolutePercentageError (MAPE)=mean(abs(predicteds−actuals)/actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


#Calculate RMSE 
# summarize accuracy
rmse <- sqrt(mean((data.999.sqrt.test.pr_pca$y - rf_pred)^2))
#41.74761
