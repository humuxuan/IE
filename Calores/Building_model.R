#创建模型

## 导入包
library(caret)
library(Metrics)

## 读数据删掉index
combine = read.csv('combine.csv')
combine = combine[,-1]

## 划分traing data and test data
TrainingIndex <- createDataPartition(combine$Calories, p=0.8, list = FALSE)
TrainingSet <- combine[TrainingIndex,] # Training Set
TestingSet <- combine[-TrainingIndex,] # Test Set

## Building Random for linear regression 
model <- lm(Calories ~., data = TrainingSet)
summary(model)

## put them in the test dataset
combine_test = TestingSet[,-8]
predict_calaores = predict(model,newdata = combine_test)

## calcuate the MAE
mae(TestingSet$Calories, predict_calaores)

## stpre the model into to RDS file
saveRDS(model, "model.rds")
