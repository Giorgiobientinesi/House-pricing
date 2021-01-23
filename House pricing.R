library(readr)
test <- read_csv("~/Desktop/test.csv")
train <- read_csv("~/Desktop/train.csv")

library(data.table)
library(mltools)
library(dummies)

# remove response variable from training and merge with test set to ease the data transformation process
data = rbind(train[, -c(length(train))], test)
dim(data)
#Na values
colSums(is.na(data))


#NA for PoolQC means no pool
data[["PoolQC"]][is.na(data[["PoolQC"]])] <- "None"
#NA for MiscFeature means none
data[["MiscFeature"]][is.na(data[["MiscFeature"]])] <- "None"
#NA for Alley means no alley access
data[["Alley"]][is.na(data[["Alley"]])] <- "None"
#NA for Fence means no fence
data[["Fence"]][is.na(data[["Fence"]])] <- "None"
#NA for FireplaceQu means no fireplace
data[["FireplaceQu"]][is.na(data[["FireplaceQu"]])] <- "None"
#For NAs in LotFrontage, we will fill in the median lot frontage from the rest of the data.
data[["LotFrontage"]][is.na(data[["LotFrontage"]])] <- 69
#For NAs in the garage factors, they mean that there is no garage
data[["GarageYrBlt"]][is.na(data[["GarageYrBlt"]])] <- 0
data[["GarageFinish"]][is.na(data[["GarageFinish"]])] <- "None"
data[["GarageQual"]][is.na(data[["GarageQual"]])] <- "None"
data[["GarageCond"]][is.na(data[["GarageCond"]])] <- "None"
data[["GarageType"]][is.na(data[["GarageType"]])] <- "None"
data[["GarageCars"]][is.na(data[["GarageCars"]])] <- 0
data[["GarageArea"]][is.na(data[["GarageArea"]])] <- 0
#For NAs in the basement factors, they mean that there is no basement
data[["BsmtCond"]][is.na(data[["BsmtCond"]])] <- "None"
data[["BsmtExposure"]] [is.na(data[["BsmtExposure"]])] <- "None"
data[["BsmtQual"]][is.na(data[["BsmtQual"]])] <- "None"
data[["BsmtFinType2"]][is.na(data[["BsmtFinType2"]])] <- "None"
data[["BsmtFinType1"]][is.na(data[["BsmtFinType1"]])] <- "None"
data[["BsmtFullBath"]][is.na(data[["BsmtFullBath"]])] <- 0
data[["BsmtHalfBath"]][is.na(data[["BsmtHalfBath"]])] <- 0
data[["BsmtFinSF1"]][is.na(data[["BsmtFinSF1"]])] <- 0
data[["BsmtFinSF2"]][is.na(data[["BsmtFinSF2"]])] <- 0
data[["BsmtUnfSF"]][is.na(data[["BsmtUnfSF"]])] <- 0
data[["TotalBsmtSF"]][is.na(data[["TotalBsmtSF"]])] <- 0

#NA for MasVnrType and MasVnrArea means no masory veneer
data[["MasVnrType"]][is.na(data[["MasVnrType"]])] <- "None"
data[["MasVnrArea"]][is.na(data[["MasVnrArea"]])] <- 0
#For NAs in MSZoning, we fill in the most common type, RL
data[["MSZoning"]][is.na(data[["MSZoning"]])] <- "RL"
#For NAs in Utilities, we fill in the most common type, AllPub
data[["Utilities"]][is.na(data[["Utilities"]])] <- "AllPub"
#For NAs in Functional, we fill in the most common type, Typ
data[["Functional"]][is.na(data[["Functional"]])] <- "Typ"
#For NAs in exteriors, we will fill in "Other"
data[["Exterior1st"]][is.na(data[["Exterior1st"]])] <- "Other"
data[["Exterior2nd"]][is.na(data[["Exterior2nd"]])] <- "Other"
#For NAs in Electrical, we fill in the most common type, SBrkr
data[["Electrical"]][is.na(data[["Electrical"]])] <- "SBrkr"
#For NAs in KitchenQual, we fill in the most common type, TA
data[["KitchenQual"]][is.na(data[["KitchenQual"]])] <- "TA"
#For NAs in SaleType, we fill in the most common type, WD
data[["SaleType"]][is.na(data[["SaleType"]])] <- "WD"


#Na values
colSums(is.na(data))


#Miscellaneous useless
data["MiscFeature"] <- NULL
# Street values are all equals
data["Street"] <- NULL
# Utilities values are all equals
data["Utilities"] <- NULL
#Conditions 2 are all equals
data["Condition2"] <- NULL
#Fence 
data[["Fence"]] <- NULL
#Pool
data[["PoolQC"]] <- NULL
#fireplace
data[["FireplaceQu"]] <- NULL
#Alley
data[["Alley"]] <- NULL


#-------------------------


#Delete non-zero variance variables
library(caret)
nzv_cols = nearZeroVar(data)
names(data[nzv_cols])
if(length(nzv_cols) > 0) 
data = data[, -nzv_cols]

#-------------------
#Numeric factors that should be categorical
data[["MoSold"]] <- factor(data[["MoSold"]])
data[["GarageYrBlt"]] <- factor(data[["GarageYrBlt"]])
data[["MSSubClass"]] <- factor(data[["MSSubClass"]])



#Categorical factors that should be numeric
data[["ExterQual"]] <- as.numeric(factor(data[["ExterQual"]], levels=c("None","Po","Fa", "TA", "Gd", "Ex")))
data[["ExterCond"]] <- as.numeric(factor(data[["ExterCond"]], levels=c("None","Po","Fa", "TA", "Gd", "Ex")))
data[["BsmtQual"]] <- as.numeric(factor(data[["BsmtQual"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
data[["BsmtExposure"]] <- as.numeric(factor(data[["BsmtExposure"]], levels=c("None","No", "Mn", "Av", "Gd")))
data[["BsmtFinType1"]] <- as.numeric(factor(data[["BsmtFinType1"]], levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ")))
data[["HeatingQC"]] <- as.numeric(factor(data[["HeatingQC"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
data[["KitchenQual"]] <- as.numeric(factor(data[["KitchenQual"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
data[["GarageQual"]] <- as.numeric(factor(data[["GarageQual"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
data[["GarageCond"]] <- as.numeric(factor(data[["GarageCond"]], levels=c("None","Po", "Fa", "TA", "Gd", "Ex")))
#----------------------------------------------SEPARATE
train = cbind(data[1:1100,], train[80])
test = data[1101:dim(data)[1], ]
numeric_attribute <- names(Filter(is.numeric,train))
train_num <- train[numeric_attribute]
categorical_attribute <- names(Filter(is.character,train))
train_cat <- train[categorical_attribute]

numeric_attribute <- names(Filter(is.numeric,test))
test_num <- test[numeric_attribute]
categorical_attribute <- names(Filter(is.character,test))
test_cat <- test[categorical_attribute]
#---------------------------------OUTLIER
single.col.out<-function(train_num, test, col){  #on train: (train_num, train_num, col)| on test: (train_num, test, col) obv
  a=boxplot.stats(train_num[[col]])
  b=as.factor(a$out)
  c=levels(b)
  for(x in seq(1:length(test[[col]]))){
    if(test[[col]][x]>boxplot.stats(train_num[[col]])$stats[4]){
      test[[col]][x]=boxplot.stats(train_num[[col]])$stats[4]}
    else if(test[[col]][x]<boxplot.stats(train_num[[col]])$stats[2]){test[[col]][x]=boxplot.stats(train_num[[col]])$stats[2]}
  }
  return(test)
}
# Test first necessarily (otherwise my train would change being unable to change test with his outliers values)
for(colonna in colnames(test_num[1:length(test_num)])){
  print(colonna)
  test_num= single.col.out(train_num,test_num,colonna)
}
#Train
for(colonna in colnames(train_num[1:length(train_num)])){
  print(colonna)
  train_num= single.col.out(train_num, train_num, colonna)
}


#--------------------------------- DATI CORRELATI TRA DI LORO
library("corrplot")
Correlation_trainnum <- cor(train_num)
corrplot(Correlation_trainnum, method = "circle")
cor(train_num)

#x1stFlrSF e TotalBsmtSf
#grlivearea e TotRmsAbvGrd
#GarageCars e GarageArea

cor(train_num$X1stFlrSF,train_num$TotalBsmtSF)
cor(train_num$TotRmsAbvGrd,train_num$GrLivArea)
cor(train_num$GarageCars,train_num$GarageArea)

cor(train_num$GrLivArea, train_num$SalePrice)
cor(train_num$TotRmsAbvGrd,train_num$SalePrice)
cor(train_num$X1stFlrSF,train_num$SalePrice)
cor(train_num$TotalBsmtSF,train_num$SalePrice)
cor(train_num$GarageCars, train_num$SalePrice)
cor(train_num$GarageArea, train_num$SalePrice)

train_num[["GarageArea"]] <- NULL 
train_num[["X1stFlrSF"]] <- NULL 
train_num[["TotRmsAbvGrd "]] <- NULL 
test_num[["GarageArea"]] <- NULL 
test_num[["X1stFlrSF"]] <- NULL 
test_num[["TotRmsAbvGrd "]] <- NULL 
train <- cbind(train_cat,train_num)
test <- cbind(test_cat,test_num)
#-------------------------------------
data <- rbind(train[1:51], test)
dim(data)
numeric_attribute <- names(Filter(is.numeric,data))
data_num <- data[numeric_attribute]
categorical_attribute <- names(Filter(is.character,data))
data_cat <- data[categorical_attribute]

library(GGally)
a=ggcorr(train_num[1:33], low = 'darkgreen', mid ='#C3DE02', high='black', label = TRUE, label_size = 3, label_round = 2, label_alpha = FALSE, label_color ='white')$data
temp = subset(a, x=="SalePrice")
head(temp)
Useful = subset(temp, abs(coefficient) > 0.1)

RigaUtile=Useful$y
Dataset = data_num[, colnames(data_num) %in% RigaUtile]  
data_num<- cbind(Dataset)
Dataset <- NULL

data <- cbind(data_cat,data_num)

train = cbind(data[1:1100,], train_num[33])
test = data[1101:dim(data)[1], ]
numeric_attribute <- names(Filter(is.numeric,train))
train_num <- train[numeric_attribute]
categorical_attribute <- names(Filter(is.character,train))
train_cat <- train[categorical_attribute]

numeric_attribute <- names(Filter(is.numeric,test))
test_num <- test[numeric_attribute]
categorical_attribute <- names(Filter(is.character,test))
test_cat <- test[categorical_attribute]
#--------------------------DUMMY

New_data <- cbind(train_cat)
dummy_train <- dummy.data.frame(New_data, sep = ".")


New_data2 <- cbind(test_cat)
dummy_test <- dummy.data.frame(New_data2, sep = ".")


a <- c(names(dummy_train))
b <- c(names(dummy_test))
Dummy_def <- intersect(a,b)
h <- setdiff(b, a)
h
k <- setdiff(a,b)
k
dummy_train[,"Condition1.RRNn"] <- 0*seq(1:1100)
dummy_train[,"Exterior2nd.Other"] <- 0*seq(1:1100)
dummy_test[,"LotShape.IR3"] <- 0*seq(1:360)
dummy_test[,"Exterior1st.AsphShn"] <- 0*seq(1:360)
dummy_test[,"Exterior1st.CBlock"] <- 0*seq(1:360)
dummy_test[,"Exterior1st.ImStucc"] <- 0*seq(1:360)
dummy_test[,"Exterior1st.Stone"] <- 0*seq(1:360)
dummy_test[,"Exterior2nd.AsphShn"] <- 0*seq(1:360)
dummy_test[,"Exterior2nd.CBlock"] <- 0*seq(1:360)
dummy_test[,"Foundation.Wood"] <- 0*seq(1:360)
dummy_test[,"Electrical.Mix"] <- 0*seq(1:360)
dummy_test[,"SaleType.Con"] <- 0*seq(1:360)
dummy_test[,"SaleType.ConLw"] <- 0*seq(1:360)
train <- cbind(dummy_train,train_num)
test <- cbind(dummy_test,test_num)
#---------------------------------
#---------------------------MODELS Lm
library(tidyverse)
library(caret)
library(xgboost)
set.seed(123)
training.samples <- train$SalePrice %>% 
createDataPartition(p = 0.7, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]
#train senza near-zero e Na e i correlati tra di loro numerici con dummy
model0 <- train(
  SalePrice ~., data= train.data, method = "lm",importance=T,
  trControl = trainControl(method ="repeatedcv", number = 10, repeats=10)
)
predicted.classes <- model0 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <- (sum(abs(test.data$SalePrice-predicted.classes)^2))
sqrt(a/329)



set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]

model0.2 <- train(
  SalePrice ~., data= train.data, method = "lm",importance=T,
  trControl = trainControl(method ="repeatedcv", number = 10, repeats=10)
)
predicted.classes <- model0.2 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <- (sum(abs(test.data$SalePrice-predicted.classes)^2))
sqrt(a/109)
#---------------------------Models rf
library(tidyverse)
library(caret)
library(xgboost)
set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]

model1 <- train(
  SalePrice ~., data= train.data, method = "rf",importance=T,
  trControl = trainControl(method ="cv", number = 10)
)

predicted.classes <- model1 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <-sum((test.data$SalePrice-predicted.classes)^2) 
b <- (a/329)
sqrt(b)





set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]

model1.2 <- train(
  SalePrice ~., data= train.data, method = "rf",importance=T,
  trControl = trainControl(method ="cv", number = 10)
)
predicted.classes <- model1.2 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <- (sum(abs(test.data$SalePrice-predicted.classes)^2))
sqrt(a/109)
#----------------------------Model xgbtree
library(tidyverse)
library(caret)
library(xgboost)
library(plyr)
set.seed(123)
training.samples <- train$SalePrice %>% 
createDataPartition(p = 0.7, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]
#train senza near-zero e Na e i correlati tra di loro numerici con dummy
model2 <- train(
  SalePrice ~., data= train.data0, method = "xgbTree",importance=T,
  trControl = trainControl(method ="repeatedcv", number = 10, repeats=10)
)

predicted.classes <- model2 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <-sum((test.data$SalePrice-predicted.classes)^2) 
b <- (a/329)
sqrt(b)
ss



set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]

model2.1 <- train(
  SalePrice ~., data= train.data, method = "xgbTree",importance=T,
  trControl = trainControl(method ="repeatedcv", number = 10, repeats=10)
)

predicted.classes <- model2.1 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <-sum((test.data$SalePrice-predicted.classes)^2) 
b <- (a/109)
sqrt(b)




#--------------- tree regression molta varianza
set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]


library("rpart")
model3 <- rpart(
  formula = SalePrice ~ .,
  data    = train.data,
  method  = "anova"
)


predicted.classes <- model3 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <- (sum(abs(test.data$SalePrice-predicted.classes)^2))
sqrt(a)



train <- cbind(train_num,train_cat)
test <- cbind(test_num,test_cat)
set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]


library("rpart")
model3.1 <- rpart(
  formula = SalePrice ~ .,
  data    = train.data,
  method  = "anova"
)


predicted.classes <- model3.1 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <- (sum(abs(test.data$SalePrice-predicted.classes)^2))
sqrt(a)

#--------------------Lasso
set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]
model4 <- train(
  SalePrice ~., data= train.data, method = "glmnet",importance=T,
  trControl = trainControl(method ="repeatedcv", number = 10, repeats=10),
  preProc = c("center","scale"),tuneGrid = expand.grid(alpha = 1,
                                                       lambda = 0))

set.seed(123)
training.samples <- train$SalePrice %>% 
  createDataPartition(p = 0.9, list = FALSE)
train.data  <- train[training.samples, ]
test.data <- train[-training.samples, ]
model4.1 <- train(
  SalePrice ~., data= train.data, method = "glmnet",importance=T,
  trControl = trainControl(method ="repeatedcv", number = 10, repeats=10),
  preProc = c("center","scale"),tuneGrid = expand.grid(alpha = 1,
                                                       lambda = 0))


predicted.classes <- model4.1 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <-sum((test.data$SalePrice-predicted.classes)^2) 
b <- (a/109)
sqrt(b)







predicted.classes <- model4.1 %>% predict(test.data)
head(predicted.classes)
ss= mean(abs((test.data$SalePrice-predicted.classes)))
a <-sum((test.data$SalePrice-predicted.classes)^2) 
b <- (a/109)
sqrt(b)
#--------------------------------------- predict on the test
y.hat = predict(model0, test)
write.table(y.hat, "preds-lm.csv", row.names = FALSE, col.names = FALSE)
y.hat = predict(model1, test)
write.table(y.hat, "preds-rf.csv", row.names = FALSE, col.names = FALSE)
y.hat = predict(model2, test)
write.table(y.hat, "preds-boosting.csv", row.names = FALSE, col.names = FALSE)

  

#definitive
y.hat = predict(model2.1, test)
write.table(y.hat, "predsD.csv", row.names = FALSE, col.names = FALSE)
predsD.csv


