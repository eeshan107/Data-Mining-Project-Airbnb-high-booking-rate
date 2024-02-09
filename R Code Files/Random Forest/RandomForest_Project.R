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
train_x <- read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")

# data clean
clean_data <- function(data) {
  new_data <- data %>%
      mutate(accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm=TRUE), accommodates),
             availability_30 = as.numeric(ifelse(is.na(availability_30), mean(availability_30, na.rm = TRUE), availability_30)),
             availability_365 = as.numeric(ifelse(is.na(availability_365), mean(availability_365, na.rm = TRUE), availability_365)),
             availability_60 = as.numeric(ifelse(is.na(availability_60), mean(availability_60, na.rm = TRUE), availability_60)),
             availability_90 = as.numeric(ifelse(is.na(availability_90), mean(availability_90, na.rm = TRUE), availability_90)),
             bathrooms = ifelse(is.na(bathrooms), mean(bathrooms, na.rm=TRUE), bathrooms),
             bedrooms = ifelse(is.na(bedrooms), mean(bedrooms, na.rm=TRUE), bedrooms),
             bed_category = as.factor(ifelse(bed_type=='Real Bed','bed','other')),
             beds = ifelse(is.na(beds), mean(beds, na.rm=TRUE), beds),
             cancellation_policy = as.factor(ifelse(cancellation_policy=="super_strict_30" | cancellation_policy=="super_strict_60","strict",cancellation_policy)), # Grouping Strict and Super Stict
             city = as.factor(ifelse(is.na(city), "MISSING", city)),
             city_name = as.factor(city_name),
             cleaning_fee = gsub('\\$','',cleaning_fee),
             cleaning_fee = gsub(',','',cleaning_fee),
             cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
             cleaning_fee = as.numeric(cleaning_fee),
             country = as.factor(country),
             country_code = as.factor(country_code),
             experiences_offered = as.factor(experiences_offered),
             extra_people = gsub('\\$','',extra_people),
             extra_people = gsub(',','',extra_people),
             extra_people = as.numeric(extra_people),
             # first_review = as.Date(first_review),
             first_review_days =  cumsum(yday(ymd(first_review)) - yday(ymd("2018-02-26"))),
             host_acceptance_rate = gsub('\\%','',host_acceptance_rate),
             host_acceptance_rate = as.numeric(host_acceptance_rate),
             host_acceptance_rate = ifelse(is.na(host_acceptance_rate), mean(host_acceptance_rate, na.rm = TRUE), host_acceptance_rate),
             host_acceptance = as.factor(ifelse(is.na(host_acceptance_rate), 'MISSING', ifelse(host_acceptance_rate == 100, 'ALL', ifelse(host_acceptance_rate < 100, 'SOME', host_acceptance_rate)))),
             host_has_profile_pic = ifelse(is.na(host_has_profile_pic), FALSE, host_has_profile_pic),
             host_identity_verified = ifelse(is.na(host_identity_verified), FALSE, host_identity_verified),
             host_is_superhost = ifelse(is.na(host_is_superhost), FALSE, host_is_superhost),
             host_listings_count = ifelse(is.na(host_listings_count), mean(host_listings_count, na.rm=TRUE), host_listings_count),
             host_location = as.factor(ifelse(is.na(host_location), "MISSING", host_location)),
             host_neighbourhood = as.factor(ifelse(is.na(host_neighbourhood), "MISSING", host_neighbourhood)),
             host_response_rate = gsub('\\%','',host_response_rate),
             host_response_rate = as.numeric(host_response_rate),
             host_response = as.factor(ifelse(is.na(host_response_rate), 'MISSING', ifelse(host_response_rate == 100, 'ALL', ifelse(host_response_rate < 100, 'SOME', host_response_rate)))),
             host_response_time = as.factor(host_response_time),
             # host_since = as.Date(host_since),
             host_days =  cumsum(yday(ymd(host_since)) - yday(ymd("2018-02-26"))),
             host_days = ifelse(is.na(host_days), mean(host_days, na.rm=TRUE), host_days),
             host_total_listings_count = ifelse(is.na(host_total_listings_count), mean(host_total_listings_count, na.rm=TRUE), host_total_listings_count),
             is_business_travel_ready = ifelse(is.na(is_business_travel_ready), FALSE, is_business_travel_ready),
             jurisdiction_names = as.factor(ifelse(is.na(jurisdiction_names), "MISSING", jurisdiction_names)),
             license = as.factor(ifelse(is.na(license), "MISSING", license)),
             market = ifelse(is.na(market), "OTHER", market),
             market = as.factor(ifelse(table(market)[as.character(market)] >= 300, 
                                       as.character(market), "OTHER")),
             maximum_nights = ifelse(is.na(maximum_nights), mean(maximum_nights, na.rm=TRUE), maximum_nights),
             minimum_nights = ifelse(is.na(minimum_nights), mean(minimum_nights, na.rm=TRUE), minimum_nights),
             monthly_price = gsub('\\$','',monthly_price),
             monthly_price = gsub(',','',monthly_price),
             monthly_price = as.numeric(monthly_price),
             monthly_price = ifelse(is.na(monthly_price), mean(monthly_price, na.rm=TRUE), monthly_price),
             neighbourhood = as.factor(ifelse(is.na(neighbourhood), "MISSING", neighbourhood)),
             price = gsub('\\$','',price),
             price = gsub(',','',price),
             price = as.numeric(price),
             price = ifelse(is.na(price), mean(price, na.rm=TRUE), price),
             property_category = as.factor(ifelse(property_type %in% c('Apartment','Serviced apartment','Loft'), 'apartment', ifelse(property_type %in% c('Bed & Breakfast', 'Boutique hotel', 'Hostel'), 'hotel', ifelse(property_type %in% c('Townhouse', 'Condominium'), 'condo', ifelse(property_type %in% c('Bungalow', 'House'), 'house', 'other'))))),
             room_type = as.factor(room_type),
             smart_location = as.factor(ifelse(is.na(smart_location), "MISSING", smart_location)),
             security_deposit = gsub('\\$','',security_deposit),
             security_deposit = gsub(',','',security_deposit),
             security_deposit = as.numeric(security_deposit),
             security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm=TRUE), security_deposit),
             square_feet = ifelse(is.na(square_feet), mean(square_feet, na.rm=TRUE), square_feet),
             state = as.factor(ifelse(is.na(state), "MISSING", state)),
             weekly_price = gsub('\\$','',weekly_price),
             weekly_price = gsub(',','',weekly_price),
             weekly_price = as.numeric(weekly_price),
             weekly_price = ifelse(is.na(weekly_price), mean(weekly_price, na.rm=TRUE), weekly_price),
             zipcode = as.factor(ifelse(is.na(zipcode), "MISSING", zipcode))
      )
  new_data <- rowid_to_column(new_data, "ID")
  return (new_data)
}

# original cleaned data
clean_train_x <- clean_data(train_x)
summary(clean_train_x)
selected_train_x <- clean_train_x %>%
  select(ID, accommodates, availability_30, availability_365, availability_60, 
         availability_90, bathrooms, bed_category, bedrooms, beds, 
         cancellation_policy, cleaning_fee, city_name, country, country_code,
         extra_people, first_review_days, guests_included, host_acceptance_rate, host_acceptance,
         host_has_profile_pic, host_identity_verified, host_is_superhost, host_listings_count,
         host_response, host_total_listings_count,
         instant_bookable, is_business_travel_ready, is_location_exact, jurisdiction_names, 
         latitude, longitude, market, maximum_nights, minimum_nights, monthly_price, 
         price, property_category, require_guest_phone_verification, 
         require_guest_profile_picture, requires_license, room_type, security_deposit, 
         square_feet, state, weekly_price)
# city, host_location, host_neighbourhood, license, neighbourhood, smart_location, zipcode

clean_test_x <- clean_data(test_x) 
summary(clean_test_x)
selected_test_x <- clean_test_x %>%
  select(ID, accommodates, availability_30, availability_365, availability_60, 
         availability_90, bathrooms, bed_category, bedrooms, beds, 
         cancellation_policy, cleaning_fee, city_name, country, country_code,
         extra_people, first_review_days, guests_included, host_acceptance_rate, host_acceptance,
         host_has_profile_pic, host_identity_verified, host_is_superhost, host_listings_count,
         host_response, host_total_listings_count,
         instant_bookable, is_business_travel_ready, is_location_exact, jurisdiction_names, 
         latitude, longitude, market, maximum_nights, minimum_nights, monthly_price, 
         price, property_category, require_guest_phone_verification, 
         require_guest_profile_picture, requires_license, room_type, security_deposit, 
         square_feet, state, weekly_price)



cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    # removePunctuation %>% #remove all punctuation
    removeWords(stopwords(kind="en")) %>% #remove stopwords
    # stemDocument %>%
    word_tokenizer 
}

text_process <- function(train_text, train_id, nameText, test_text, test_id) {
  it_train = itoken(text, 
                    preprocessor = tolower, #preprocessing by converting to lowercase
                    tokenizer = cleaning_tokenizer, 
                    ids = id, 
                    progressbar = FALSE)
  vocab = create_vocabulary(it_train)
  vectorizer = vocab_vectorizer(vocab)
  dtm = create_dtm(it_train, vectorizer)
  dtm_df <- data.frame(as.matrix(dtm), stringsAsFactors=FALSE)
  colnames(dtm_df) <- paste(colnames(dtm_df), nameText, sep="_")
  return (dtm_df)
}



it_train_amenities = itoken(clean_train_x$amenities, 
                  preprocessor = tolower, #preprocessing by converting to lowercase
                  tokenizer = cleaning_tokenizer, 
                  ids = clean_train_x$ID, 
                  progressbar = FALSE)
vocab_amenities = create_vocabulary(it_train_amenities)
vectorizer_amenities = vocab_vectorizer(vocab_amenities)
dtm_train_amenities = create_dtm(it_train_amenities, vectorizer_amenities)
dtm_train_df_amenities <- data.frame(as.matrix(dtm_train_amenities), stringsAsFactors=FALSE)
colnames(dtm_train_df_amenities) <- paste(colnames(dtm_train_df_amenities), "amenities", sep="_")
it_test_amenities = itoken(clean_test_x$amenities, 
                            preprocessor = tolower, #preprocessing by converting to lowercase
                            tokenizer = cleaning_tokenizer, 
                            ids = clean_test_x$ID, 
                            progressbar = FALSE)
dtm_test_amenities = create_dtm(it_test_amenities, vectorizer_amenities)
dtm_test_df_amenities <- data.frame(as.matrix(dtm_test_amenities), stringsAsFactors=FALSE)
colnames(dtm_test_df_amenities) <- paste(colnames(dtm_test_df_amenities), "amenities", sep="_")

it_train_host_verifications = itoken(clean_train_x$host_verifications, 
                                     preprocessor = tolower, #preprocessing by converting to lowercase
                                     tokenizer = cleaning_tokenizer, 
                                     ids = clean_train_x$ID, 
                                     progressbar = FALSE)
vocab_host_verifications = create_vocabulary(it_train_host_verifications)
vectorizer_host_verifications = vocab_vectorizer(vocab_host_verifications)
dtm_train_host_verifications = create_dtm(it_train_host_verifications, vectorizer_host_verifications)
dtm_train_df_host_verifications <- data.frame(as.matrix(dtm_train_host_verifications), stringsAsFactors=FALSE)
colnames(dtm_train_df_host_verifications) <- paste(colnames(dtm_train_df_host_verifications), "host_verifications", sep="_")
it_test_host_verifications = itoken(clean_test_x$host_verifications, 
                                   preprocessor = tolower, #preprocessing by converting to lowercase
                                   tokenizer = cleaning_tokenizer, 
                                   ids = clean_test_x$ID, 
                                   progressbar = FALSE)
dtm_test_host_verifications = create_dtm(it_test_host_verifications, vectorizer_host_verifications)
dtm_test_df_host_verifications <- data.frame(as.matrix(dtm_test_host_verifications), stringsAsFactors=FALSE)
colnames(dtm_test_df_host_verifications) <- paste(colnames(dtm_test_df_host_verifications), "host_verifications", sep="_")

train_x_data <- cbind(cbind(selected_train_x, dtm_train_df_amenities), dtm_train_df_host_verifications)
# write.table(train_x_data, "sub2_train_x.csv")

test_x_data <- cbind(cbind(selected_test_x, dtm_test_df_amenities), dtm_test_df_host_verifications)
# write.table(test_x_data, "sub2_test_x.csv")

# train <- cbind(train_x_data, train_y) %>%
#   mutate(high_booking_rate = as.factor(high_booking_rate))%>%
#   select(-c(perfect_rating_score))


# model selection

temp_df <- cbind(train_x_data, train_y$high_booking_rate)
names(temp_df)[names(temp_df) == "train_y$high_booking_rate"] <- "high_booking_rate"
dumvar <- dummyVars(~., data = temp_df, fullRank = TRUE)
dum_df <- data.frame(predict(dumvar, newdata = temp_df))


# split and train
train_rows <- sample(nrow(dum_df),.7*nrow(dum_df))
airbnb_train <- dum_df[train_rows,]
airbnb_valid <- dum_df[-train_rows,]
airbnb_train_x <- airbnb_train[,-422]
airbnb_train_y <- airbnb_train$high_booking_rate
airbnb_valid_x <- airbnb_valid[,-422]
airbnb_valid_y <- airbnb_valid$high_booking_rate


rf_mod <- ranger(x = airbnb_train_x, y = as.factor(airbnb_train_y),
                 mtry=15, num.trees=500,
                 importance="impurity",
                 probability = TRUE)
prob_rf <- predict(rf_mod, data=airbnb_valid_x)$predictions[,2]
preds_rf <- prediction(prob_rf, as.factor(airbnb_valid_y))
auc_rf <- performance(preds_rf, measure = "auc")@y.values[[1]]
auc_rf
#write.table(prob_rf, "high_booking_rate_group0.csv", row.names = FALSE)
library(ggplot2)

library(caret)
importance(rf_mod)
varImpPlot(rf_mod)
library(pROC)

roc_rf <- roc(as.numeric(as.character(airbnb_valid_y)), prob_rf, levels=c(0,1))

plot(roc_rf, col = "red", main = "ROC curve for random forest")

