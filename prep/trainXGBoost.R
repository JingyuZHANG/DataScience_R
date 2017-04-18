library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(dplyr)
set.seed(31)
data_fare<-data[,1:7]
#labels <- data_fare['fare']
class(data_fare)
data_tr <- select(data_fare, - fare)
l <- data_fare$fare
xgb.tr <- xgb.DMatrix(data = as.matrix(data_tr), label = l)
xgb.res <- xgb.train(xgb.tr, nrounds = 100)
xgb.res <- xgboost(xgb.tr,
  booster = "gbtree", 
  objective = "reg:linear", 
  max.depth = 5, 
  eta = 0.5, 
  nthread = 2, 
  nround = 2, 
  min_child_weight = 1, 
  subsample = 0.5, 
  colsample_bytree = 1, 
  num_parallel_tree = 1)

xgb <- xgb.train(params = list(eta = 0.1,
                               max.depth = 4,
                               objective = 'reg:linear'),
                 data = xgb.tr,
                 nrounds = 10,
                 nthread = 20,
                 verbose = 1,
                 maximize = 1)


xgb.train(params = list(eta = 0.1,
                        max.depth = 4,
                        objective = 'binary:logistic',
                        eval_metric = 'auc'),
          data = data.matrix(data_fare[,-1]),
          nrounds = 10000,
          nthread = 20,
          watchlist = list(valid = data_fare[,-1]),
          verbose = 1,
          maximize = 1,
          early_stopping_rounds = 50)


