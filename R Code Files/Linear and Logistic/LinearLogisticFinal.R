library(tidyverse)


#Logistic model
train_lm <- cbind(selected_train_x, new_train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score),
         high_booking_rate = as.factor(high_booking_rate)) 
train_lm1 <- train_lm %>%
  select(-c(perfect_rating_score))

logistic_rate <- glm(high_booking_rate ~ ., data = train_lm1, family = "binomial")
probs_rate <- predict(logistic_rate, newdata = new_test_x_data, type = "response")

probs_rate
summary(logistic_rate)

#linear model
lm_rate <- lm(high_booking_rate ~ ., data = train_lm1)
probs_rate_lm <- predict(lm_rate, newdata = new_test_x_data)
#write.table(probs_rate_lm, "lm_high_booking_rate_group17.csv", row.names = FALSE)


















library(nnet)

model_nn <- nnet(high_booking_rate ~ ., data = train_lm1, size = 10, maxit = 100)
