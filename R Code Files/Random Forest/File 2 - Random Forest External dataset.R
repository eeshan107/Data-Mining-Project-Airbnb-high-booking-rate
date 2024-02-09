library(tidyverse)
library(caret)
library(tree)
library(class)
library(lubridate)
library(tibble)
library(tm)
library(text2vec)
library(SnowballC)
library(glmnet)
library(vip)
library(naivebayes)
library(ranger)
library(xgboost)
library(ROCR)
library(mlr)

# load
new_train_x_data <- readRDS('final_train_x_population_data.rds')
new_train_y <- read_csv("airbnb_train_y_2023.csv")
new_test_x_data <- readRDS('final_test_x_population_data.rds')

# transform to dummies
dumvar <- dummyVars(~., data = new_train_x_data, fullRank = TRUE)
train_dum <- data.frame(predict(dumvar, newdata = new_train_x_data))
test_dum <- data.frame(predict(dumvar, newdata = new_test_x_data))
dum_df <- cbind(train_dum, new_train_y$high_booking_rate)
names(dum_df)[names(dum_df) == "new_train_y$high_booking_rate"] <- "high_booking_rate"

# model selection
# To do: 1. decide parameters 2. train and validate 3. select best

# cross validation
K <- 4
# Create K equally size folds
folds <- cut(seq(1, nrow(dum_df)), breaks = K, labels = FALSE)

aucs_rf <- rep(0, K)
for (i in c(1:K)) {
  validIndexes <- which(folds == i, arr.ind = TRUE)
  inner_validData <- dum_df[validIndexes, ]
  inner_trainData <- dum_df[-validIndexes, ]
  inner_train_y <- inner_trainData$high_booking_rate
  inner_train_x <- inner_trainData[1:ncol(inner_trainData)-1]
  inner_valid_y <- inner_validData$high_booking_rate
  inner_valid_x <- inner_validData[1:ncol(inner_trainData)-1]
  
  # train: random forest
  rf_mod <- ranger(x = inner_train_x, y = as.factor(inner_train_y),
                   mtry=100, num.trees=300,
                   importance="impurity",
                   probability = TRUE)
  inner_prob_rf <- predict(rf_mod, data=inner_valid_x)$predictions[,2]
  inner_preds_rf <- prediction(inner_prob_rf, as.factor(inner_valid_y))
  inner_auc_rf <- performance(inner_preds_rf, measure = "auc")@y.values[[1]]
  aucs_rf[i] <- inner_auc_rf
}

auc_rf <- mean(aucs_rf)
auc_rf
# 4 fold cross validation, sub_8 dataset: auc = 0.8772391

