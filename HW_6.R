rm(list = ls())

library(data.table)
train <- fread("train.csv")
test <- fread("test.csv")

#Check for the missing variable in the test set.

trainVars <- colnames(train)
testVars <- colnames(test)

missingVar <- NULL
j = 1

for(i in 1:ncol(train))
{
 if(trainVars[i] %in% testVars){
 }else{
   missingVar[j] = trainVars[i]
   j = j+1
  }
}

missingVar
#SalePrice is in the training set but not in the test set.
#We want to be able to predict the sale price for the observations
#in the test set.

###################################
### DEALING WITH MISSING VALUES ###
###################################

#Determine where we are missing values.
#Creates a function that determines the percentage of missing values.
pMiss <- function(x){(sum(is.na(x)) / length(x)) * 100}

#Apply the above function to the columns of the training set.
(pMissCol <- apply(train, 2, pMiss))

#View the variables which have more than 5% missing values.
pMissCol[pMissCol > 5]
#Out of the 11 variables in this list, only one of them is a concern (LotFrontage).

#View the variables which have missing values, but only up to 5%.
pMissCol[0 < pMissCol & pMissCol < 5]
f#Out of the 8 variables in this list, "MasVnrType", "MasVnrArea", "BsmtExposure", 
#"BsmtFinType2", and "Electrical" have missing values.

#Percentage of missing values for both "BsmtExposure" and "BsmtFinType2" is 0.06849315%.
#This is the same percentage as "Electrical".
percentMiss <- pMissCol[0 < pMissCol & pMissCol < 5][5] - pMissCol[0 < pMissCol & pMissCol < 5][4]

#There is only 1 row which has a basement but has a missing value for "BsmtExposure" and "BsmtFinType2".
nrow(train) * (percentMiss/100)

#This is the 949th observation.
index1 <- train[which(!is.na(train$BsmtCond) & is.na(train$BsmtExposure))][[1]]

#The 1380th observations is the one row with a missing electrical value.
index2 <- train[which(is.na(train$Electrical))][[1]]

#Before seeing how many missing values are in each row, we will want to recode
#the NA's in the variables which NA's should not be treated as missing values.

train$Alley[is.na(train$Alley)] <- "none"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "none"
train$GarageType[is.na(train$GarageType)] <- "none"
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- "none"
train$GarageFinish[is.na(train$GarageFinish)] <- "none"
train$GarageQual[is.na(train$GarageQual)] <- "none"
train$GarageCond[is.na(train$GarageCond)] <- "none"
train$PoolQC[is.na(train$PoolQC)] <- "none"
train$Fence[is.na(train$Fence)] <- "none"
train$MiscFeature[is.na(train$MiscFeature)] <- "none"
train$BsmtQual[is.na(train$BsmtQual)] <- "none"
train$BsmtCond[is.na(train$BsmtCond)] <- "none"
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "none"

train$BsmtExposure[is.na(train$BsmtExposure)] <- "none"
train$BsmtExposure[[index1]] <- NA

train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "none"
train$BsmtFinType2[[index1]] <- NA

for(i in 1:ncol(train))
{
  if(class(train[[i]]) == "character")
  {
    train[[i]] <- as.factor(train[[i]])
  }
}

library(mice)
imputeTrain <- mice(train, m = 5, method = 'rf')

completedTrain <- complete(imputeTrain, 1)

#End of Brandon's Code

#Change some variables before RF will work 
##Must rename the variables that start with a number
colnames(completedTrain)[44]<-"FirstFlrSF"
colnames(completedTrain)[45]<-"ScndFlrSF"
colnames(completedTrain)[70]<-"ThreeSsnPorch"
##GarageYrBlt needs to be numeric 
completedTrain$GarageYrBlt <- as.numeric(completedTrain$GarageYrBlt)
#RF cannot handle categorical predictors with more than 53 categories
##Do not use as.factor(SalePrice)

#Run RF and obtain varImpPlot, do not use column 1 "Id"
price.rf <- randomForest(SalePrice~ . , importance=TRUE, data=completedTrain[, 2:81])
varImpPlot(price.rf, scale=FALSE)

#Consider top7 variables from %IncMSE plot
#OverallQual, GrLivArea, Neighborhood, ExterQual, TotalBsmtSF, GarageCars, FirstFlrSF
lm7 = lm(formula = SalePrice ~ OverallQual + GrLivArea + Neighborhood + 
           ExterQual + TotalBsmtSF + GarageCars + FirstFlrSF, data = completedTrain)
summary(lm7)
MSE7 <- mean(lm7$residuals^2)
MSE7

#Consider top4 variables from %IncMSE plot
#OverallQual, GrLivArea, Neighborhood, ExterQual
lm4 = lm(formula = SalePrice ~ OverallQual + GrLivArea + Neighborhood + 
           ExterQual, data = completedTrain)
summary(lm4)
MSE4 <- mean(lm4$residuals^2)
MSE4
(MSE4/MSE7)
#MSE4 is 9.6% larger than MSE7

#Consider top3 variables from %IncMSE plot
#OverallQual, GrLivArea, Neighborhood
lm3 = lm(formula = SalePrice ~ OverallQual + GrLivArea + Neighborhood, data = completedTrain)
summary(lm3)
MSE3 <- mean(lm3$residuals^2)
MSE3
(MSE3/MSE7)
#MSE3 is 18% larger than MSE7
(MSE3/MSE4)
#MSE3 is 7.8% larger than MSE4
