# Kaggle - House Prices: Advanced Regression Techniques

My plan is to check the Kaggle competitions which are tagged as "Getting Started". After completing my initial 
Titatic Project,let's check out the competition for "House Price Prediction".

Let the FUN begin!
## 1. Load Libraries

```{r}
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(gbm)
```
## 2. Load Data
Kaggle has provided a Training Set and a Test Set. Let's Load the data from both of the files.
```{r}
######################################
# 1. Load Data
######################################
# Set Working Directory
setwd("/Users/oindrilasen/WORK_AREA/Data Science/kaggle/House_Price")
# Read train.csv data file
price_train<-read.csv("train.csv",
                      header = TRUE
                      #,na.strings = "",
                      #stringsAsFactors = FALSE
)

# Read test.csv data file
price_test<-read.csv("test.csv",
                     header = TRUE
                     #,na.strings = "",
                     #stringsAsFactors = FALSE
)
```
In the test file, there is no "SalePrice" column because that is the Value that we will be predicting.So, let's add a new column 'SalesPrice'
in the test file and set the value to 0 initially.Now, merge the Test and Training Data Frame.
```{r}
price_test$SalePrice <- 0
# Merge Train and Test Dataset
price_clean <- rbind(price_train, price_test)

# Take a look at the data
dim(price_train) # 1460,81
dim(price_test) # 1459,80
```
Let's take a peek!
```{r}
glimpse(price_clean) # 2919,81
```
## 3. Data Wrangling

### 3.1.Check For NA Values
```{r}
na_count <- sort(sapply(price_clean, function(x) sum(is.na(x))),
                 decreasing = TRUE)
na_count
```
### 3.2.Replace NA values with some meaningful value
I have used the data_description.txt file to understand the meaning od each column and have replaced the NA records accordingly.
```{r}
# 2.1. PoolQC
table(price_clean$PoolQC, exclude = NULL)
levels(price_clean$PoolQC)
levels(price_clean$PoolQC)[levels(price_clean$PoolQC)] <- "Np"
price_clean$PoolQC[is.na(price_clean$PoolQC)] <- "Np" # No Pool
```
```{r}
# 2.2. MiscFeature
table(price_clean$MiscFeature, exclude = NULL)
levels(price_clean$MiscFeature)
levels(price_clean$MiscFeature)[levels(price_clean$MiscFeature)] <- "None"
price_clean$MiscFeature[is.na(price_clean$MiscFeature)] <- "None" 
```
```{r}
#2.3 Alley
table(price_clean$Alley, exclude = NULL)
levels(price_clean$Alley)
levels(price_clean$Alley)[levels(price_clean$Alley)] <- "No Alley"
price_clean$Alley[is.na(price_clean$Alley)] <- "No Alley" 
```
```{r}
#2.4. Fence
table(price_clean$Fence, exclude = NULL)
levels(price_clean$Fence)
levels(price_clean$Fence)[levels(price_clean$Fence)] <- "No Fence"
price_clean$Fence[is.na(price_clean$Fence)] <- "No Fence" 
```
```{r}
#2.5. FireplaceQu
table(price_clean$FireplaceQu, exclude = NULL)
levels(price_clean$FireplaceQu)
levels(price_clean$FireplaceQu)[levels(price_clean$FireplaceQu)] <- "No Fireplace"
price_clean$FireplaceQu[is.na(price_clean$FireplaceQu)] <- "No Fireplace" 
```
```{r}
#2.6. LotFrontage
unique(price_clean$LotFrontage)
price_clean$LotFrontage <- as.integer(price_clean$LotFrontage)
price_clean$LotFrontage[is.na(price_clean$LotFrontage)] <- mean(price_clean$LotFrontage, na.rm = TRUE)
```
```{r}
#2.7. GarageYrBlt
class(price_clean$GarageYrBlt)
table(price_clean$GarageYrBlt, exclude = NULL)
price_clean$GarageYrBlt[is.na(price_clean$GarageYrBlt)] <- median(price_clean$GarageYrBlt,na.rm= TRUE)
```
```{r}
# 2.8. GarageFinish
table(price_clean$GarageFinish, exclude = NULL)
levels(price_clean$GarageFinish)
levels(price_clean$GarageFinish)[levels(price_clean$GarageFinish)] <- "NoGa"
price_clean$GarageFinish[is.na(price_clean$GarageFinish)] <- "NoGa"
```
```{r}
# 2.9. GarageQual
table(price_clean$GarageQual, exclude = NULL)
levels(price_clean$GarageQual)
levels(price_clean$GarageQual)[levels(price_clean$GarageQual)] <- "NoGa"
price_clean$GarageQual[is.na(price_clean$GarageQual)] <- "NoGa"
```
```{r}
# 2.10. GarageType
table(price_clean$GarageType, exclude = NULL)
levels(price_clean$GarageType)
levels(price_clean$GarageType)[levels(price_clean$GarageType)] <- "NoGa"
price_clean$GarageType[is.na(price_clean$GarageType)] <- "NoGa"
```
```{r}
# 2.11. GarageCond
table(price_clean$GarageCond, exclude = NULL)
levels(price_clean$GarageCond)
levels(price_clean$GarageCond)[levels(price_clean$GarageCond)] <- "NoGa"
price_clean$GarageCond[is.na(price_clean$GarageCond)] <- "NoGa"
```
```{r}
# 2.12. BsmtCond
table(price_clean$BsmtCond, exclude = NULL)
levels(price_clean$BsmtCond)
levels(price_clean$BsmtCond)[levels(price_clean$BsmtCond)] <- "NoBa"
price_clean$BsmtCond[is.na(price_clean$BsmtCond)] <- "NoBa"
```
```{r}
# 2.13. BsmtExposure
levels(price_clean$BsmtExposure)[levels(price_clean$BsmtExposure)] <- "NoBa"
price_clean$BsmtExposure[is.na(price_clean$BsmtExposure)] <- "NoBa"
```
```{r}
# 2.14. BsmtQual
levels(price_clean$BsmtQual)[levels(price_clean$BsmtQual)] <- "NoBa"
price_clean$BsmtQual[is.na(price_clean$BsmtQual)] <- "NoBa"
```
```{r}
# 2.15. BsmtFinType2
levels(price_clean$BsmtFinType2)[levels(price_clean$BsmtFinType2)] <- "NoBa"
price_clean$BsmtFinType2[is.na(price_clean$BsmtFinType2)] <- "NoBa"
```
```{r}
# 2.14. BsmtFinType1
levels(price_clean$BsmtFinType1)[levels(price_clean$BsmtFinType1)] <- "NoBa"
price_clean$BsmtFinType1[is.na(price_clean$BsmtFinType1)] <- "NoBa"
```
```{r}
# 2.14. GarageCars
price_clean$GarageCars[is.na(price_clean$GarageCars)] <- 0
```{r}
# 2.14. GarageArea
price_clean$GarageArea[is.na(price_clean$GarageArea)] <- 0
```
```{r}
# 2.15 Electrical
table(price_clean$Electrical, exclude = NULL)
price_clean$Electrical[is.na(price_clean$Electrical)] <- "SBrkr"
```
```{r}
# 2.16 MSZoning
table(price_clean$MSZoning, exclude = NULL)
price_clean$MSZoning[is.na(price_clean$MSZoning)] <- "RL"
```
```{r}
# 2.17 Utilities
table(price_clean$Utilities, exclude = NULL)
price_clean$Utilities[is.na(price_clean$Utilities)] <- "AllPub"
```
```{r}
# 2.18 Exterior1st
table(price_clean$Exterior1st, exclude = NULL)
price_clean$Exterior1st[is.na(price_clean$Exterior1st)] <- "VinylSd"
```
```{r}
# 2.19. Exterior2nd
table(price_clean$Exterior2nd, exclude = NULL)
price_clean$Exterior2nd[is.na(price_clean$Exterior2nd)] <- "VinylSd"
```
```{r}
# 2.20. MasVnrType
table(price_clean$MasVnrType, exclude = NULL)
price_clean$MasVnrType[is.na(price_clean$MasVnrType)] <- "None"
```
```{r}
# 2.21. MasVnrArea
table(price_clean$MasVnrArea, exclude = NULL)
price_clean$MasVnrArea[is.na(price_clean$MasVnrArea)] <- 0
```
```{r}
# 2.22. BsmtFinSF1
table(price_clean$BsmtFinSF1, exclude = NULL)
price_clean$BsmtFinSF1[is.na(price_clean$BsmtFinSF1)] <- mean(price_clean$BsmtFinSF1,na.rm = TRUE)
```
```{r}
# 2.23. BsmtFinSF2
table(price_clean$BsmtFinSF2, exclude = NULL)
price_clean$BsmtFinSF2[is.na(price_clean$BsmtFinSF2)] <- mean(price_clean$BsmtFinSF2,na.rm = TRUE)
```
```{r}
# 2.24. BsmtUnfSF
table(price_clean$BsmtUnfSF, exclude = NULL)
price_clean$BsmtUnfSF[is.na(price_clean$BsmtUnfSF)] <- mean(price_clean$BsmtUnfSF,na.rm = TRUE)
```
```{r}
# 2.25. TotalBsmtSF
table(price_clean$TotalBsmtSF, exclude = NULL)
price_clean$TotalBsmtSF[is.na(price_clean$TotalBsmtSF)] <- mean(price_clean$TotalBsmtSF,na.rm = TRUE)
```
```{r}
# 2.26. BsmtFullBath
table(price_clean$BsmtFullBath, exclude = NULL)
price_clean$BsmtFullBath[is.na(price_clean$BsmtFullBath)] <-0
```
```{r}
# 2.27. BsmtHalfBath
table(price_clean$BsmtHalfBath, exclude = NULL)
price_clean$BsmtHalfBath[is.na(price_clean$BsmtHalfBath)] <-0
```
```{r}
# 2.28. KitchenQual
table(price_clean$KitchenQual, exclude = NULL)
price_clean$KitchenQual[is.na(price_clean$KitchenQual)] <-"TA"
```
```{r}
# 2.29. Functional
table(price_clean$Functional, exclude = NULL)
price_clean$Functional[is.na(price_clean$Functional)] <-"Typ"
```
```{r}
# 2.30. SaleType
table(price_clean$SaleType, exclude = NULL)
price_clean$SaleType[is.na(price_clean$SaleType)] <-"WD"
```
Let's confirm if all NA records are taken care of.
```{r}
which(sapply(price_clean, function(x) sum(is.na(x))>0))
```
## 4. Featue Engineering
Adding a few new features to improve the prediction.

```{r}
#total_area 
price_clean$total_area <- price_clean$LotArea+
  price_clean$X1stFlrSF +
  price_clean$X2ndFlrSF+
  price_clean$TotalBsmtSF +
  price_clean$GarageArea
# reg_shape 
price_clean$reg_shape <- ifelse(price_clean$LotShape %in% c('IR1','IR2','IR3'),1,0)

# has_pool 
price_clean$has_pool <- ifelse(price_clean$PoolQC == 'Np',1,0)

# has_fence
price_clean$has_fence <- ifelse(price_clean$Fence == 'No Fence',1,0)
```
Let's check out the current data frame.
```{r}
dim(price_clean)
```
There are total 85 features in the dataset. Let's select a few variables and see how the Model goes.
```{r}
#select variables that be used for model buidling and heat map
model_var <- c('Id','SalePrice' ,'MSSubClass','MSZoning','Street',
               'Alley','reg_shape','Utilities','Condition1','Condition2',
               'OverallQual','OverallCond','YearBuilt', 'RoofStyle','RoofMatl',
               'Exterior1st','ExterQual','ExterCond','Foundation',
               'GarageType','GarageCond','BsmtCond','KitchenQual',
               'total_area','Heating','HeatingQC','has_pool' ,'has_fence',
               'CentralAir','Electrical','Fireplaces','OpenPorchSF','YrSold')

price_clean <- price_clean[, model_var]
```
## 5. Featue Analysis
```{r}
set.seed(109)
price_train <- price_clean[1:1460, ]
price_test <- price_clean[1461:2919, ]
```
* Exploratory Data Analysis

```{r}
summary(price_train$SalePrice)
# SalePrice
ggplot(price_train, aes(x=SalePrice))+
  geom_histogram(fill= 'light blue')

price_train$lSalePrice <- log(price_train$SalePrice)

# lSalePrice
ggplot(price_train, aes(x=lSalePrice))+
  geom_histogram(fill= 'light blue')

#MSSubClass
ggplot(price_train, aes(x = factor(MSSubClass), y = SalePrice)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(34900, 500000)

#MSZoning
ggplot(price_train, aes(x = factor(MSZoning), y = SalePrice)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(34900, 500000)

#LotFrontage
summary(price_train$LotFrontage)
ggplot(price_train, aes(x = LotFrontage, y = SalePrice)) +
  geom_histogram() 
```
* Check Variable Importance using Caret
```{r}
# prepare training scheme
control <- trainControl(method="repeatedcv",
                        number=5,
                        repeats=5,
                        verboseIter=FALSE)

model1 <- train(SalePrice ~ .-(SalePrice+lSalePrice+Id),
                data= price_train,
                method="lm",
                trControl=control
)
summary(model1)
# estimate variable importance
importance <- varImp(model1, scale=FALSE)
# summarize importance
print(importance)
# # plot importance
plot(importance)
```
## 6. Build a Model

* Linear Regression Model
```{r}
glimpse( price_train)
# **************** 6.1. Linear Regression Model***********
model_lm <- lm(lSalePrice ~ .-(lSalePrice+SalePrice+Id) , 
               data = price_train)
summary(model_lm)
plot(model_lm)
# Prediction
predict_price_lm <-predict(model_lm , price_test,type = "response" )
price_test$lSalePrice <- predict_price_lm
residuals <- price_test$lSalePrice - predict_price_lm
linreg_pred <- data.frame("Predicted" = predict_price_lm, 
                          "Actual" = price_test$SalePrice, 
                          "Residual" = residuals)
price_test$SalePrice <- exp(price_test$lSalePrice)
# Write the Final Solution
solution_lm <- price_test%>%
  select(Id,SalePrice)
write.csv(solution_lm, 
          file = "solution_lm.csv",row.names = F)
```
* Random Forest Model

```{r}
# *********** 6.2. Random Forest Model***********
model_rf <- randomForest(lSalePrice ~ .-(lSalePrice+SalePrice+Id),
                         data= price_train)
summary(model_rf)
# Prediction
predict_price_rf <-predict(model_rf , price_test )
price_test$lSalePrice <- predict_price_lm
price_test$SalePrice <- exp(price_test$lSalePrice)
# Write the Final Solution
solution_rf <- price_test%>%
  select(Id,SalePrice)
write.csv(solution_rf, 
          file = "solution_rf.csv",row.names = F)
```
* Gradient Boosting Model

```{r}
# *********** 6.3. Gradient Boosting Model***********
model_gbm <- gbm(lSalePrice ~ .-(lSalePrice+SalePrice+Id),
                data= price_train,
                distribution = "gaussian",
                n.trees = 10000,
                shrinkage = 0.01, 
                interaction.depth = 4)
summary(model_gbm)
# Prediction
n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 
predict_price_gbm <-predict(model_gbm , price_test,n.trees = n.trees )
dim(predict_price_gbm)

#Calculating The Mean squared Test Error
test.error<-with(price_test,apply((predict_price_gbm)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

predict_price_gbm <-predict(model_gbm , price_test,n.trees = 10000 )
price_test$lSalePrice <- predict_price_gbm
price_test$SalePrice <- exp(price_test$lSalePrice)
# Write the Final Solution
solution_gbm <- price_test%>%
  select(Id,SalePrice)
write.csv(solution_gbm, 
          file = "solution_gbm.csv",row.names = F)
```
## 7. Compare The Data For Different Model
```{r}
# Compare different Models

head(solution_lm)
head(solution_rf)
head(solution_gbm)
```
