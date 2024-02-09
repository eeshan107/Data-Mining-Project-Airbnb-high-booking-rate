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

# load
new_train_x_data <- readRDS('sub_8_small_train_x_data.rds')
new_train_y <- read_csv("airbnb_train_y_2023.csv")
new_test_x_data <- readRDS('sub_8_small_train_x_data.rds')

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

aucs_bst <- rep(0, K)
for (i in c(1:K)) {
  dum_df$high_booking_rate <- ifelse(dum_df$high_booking_rate == "YES", 1, 0)
  validIndexes <- which(folds == i, arr.ind = TRUE)
  inner_validData <- dum_df[validIndexes, ]
  inner_trainData <- dum_df[-validIndexes, ]
  inner_train_y <- inner_trainData$high_booking_rate
  inner_train_x <- inner_trainData[1:ncol(inner_trainData)-1]
  inner_valid_y <- inner_validData$high_booking_rate
  inner_valid_x <- inner_validData[1:ncol(inner_trainData)-1]
  
  # train: boost
  
  bst_mod <- xgboost(data = as.matrix(inner_train_x), 
                     label = as.numeric(as.character(inner_train_y)), 
                     max.depth = 50, eta = 1, nrounds = 1000,  
                     objective = "binary:logistic", verbose = 0)
  inner_prob_bst <- predict(bst_mod, as.matrix(inner_valid_x))
  inner_preds_bst <- prediction(inner_prob_bst, as.numeric(as.character(inner_valid_y)))
  inner_auc_bst <- performance(inner_preds_bst, measure = "auc")@y.values[[1]]
  aucs_bst[i] <- inner_auc_bst
}

auc_bst <- mean(aucs_bst)
auc_bst

# debug
i <- 1
# dum_df$high_booking_rate <- ifelse(dum_df$high_booking_rate == "YES", 1, 0)
# validIndexes <- which(folds == i, arr.ind = TRUE)
# inner_validData <- dum_df[validIndexes, ]
# inner_trainData <- dum_df[-validIndexes, ]
# inner_train_y <- inner_trainData$high_booking_rate
# inner_train_x <- inner_trainData[1:ncol(inner_trainData)-1]
# inner_valid_y <- inner_validData$high_booking_rate
# inner_valid_x <- inner_validData[1:ncol(inner_trainData)-1]
# 
# # train: boost
# 
# bst_mod <- xgboost(data = as.matrix(inner_train_x), 
#                    label = as.numeric(as.character(inner_train_y)), 
#                    max.depth = 50, eta = 1, nrounds = 1000,  
#                    objective = "binary:logistic", verbose = 0)
# inner_prob_bst <- predict(bst_mod, as.matrix(inner_valid_x))
# inner_preds_bst <- prediction(inner_prob_bst, as.numeric(as.character(inner_valid_y)))
# inner_auc_bst <- performance(inner_preds_bst, measure = "auc")@y.values[[1]]
# aucs_bst[i] <- inner_auc_bst