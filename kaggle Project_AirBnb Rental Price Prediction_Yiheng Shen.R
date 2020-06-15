#read data: deal with NAs
data = read.csv('analysisData.csv',na.strings = c("NA","N/A",""))
scoringData = read.csv('scoringData.csv',na.strings = c("NA","N/A",""))

#load packages
library(ggplot2)
library(caret)
library(caTools)
library(dplyr)
library(ISLR)
library(gplots)
library(ROCR)
library(zipcode)
library(rpart)
library(rpart.plot)
library(randomForest)
set.seed(27)

#data cleaning
## deal with NAs of numeric
#host_listings_count
data$host_listings_count[is.na(data$host_listings_count)] <- 0
scoringData$host_listings_count[is.na(scoringData$host_listings_count)] <- 0
#beds
data$beds[is.na(data$beds)] <- median(data$beds, na.rm=TRUE)
scoringData$beds[is.na(scoringData$beds)] <- median(scoringData$beds, na.rm=TRUE)
#security_deposit
data$security_deposit[is.na(data$security_deposit)] <- median(data$security_deposit, na.rm=TRUE)
scoringData$security_deposit[is.na(scoringData$security_deposit)] <- median(scoringData$security_deposit, na.rm=TRUE)
#cleaning_fee
data$cleaning_fee[is.na(data$cleaning_fee)] <- median(data$cleaning_fee, na.rm=TRUE)
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- median(scoringData$cleaning_fee, na.rm=TRUE)
#first_review
last_day <- as.Date('2019-08-30')
data$first_review = as.numeric(last_day-as.Date(data$first_review))
scoringData$first_review = as.numeric(last_day-as.Date(scoringData$first_review))
data$first_review[is.na(data$first_review)] <- median(data$first_review, na.rm=TRUE)
scoringData$first_review[is.na(scoringData$days)] <- median(scoringData$first_review, na.rm=TRUE)
#last_review
last_day <- as.Date('2019-08-30')
data$last_review = as.numeric(last_day-as.Date(data$last_review))
scoringData$last_review = as.numeric(last_day-as.Date(scoringData$last_review))
data$last_review[is.na(data$last_review)] <- median(data$last_review, na.rm=TRUE)
scoringData$last_review[is.na(scoringData$last_review)] <- median(scoringData$last_review, na.rm=TRUE)
#reviews_per_month
data$reviews_per_month[is.na(data$reviews_per_month)] <- median(data$reviews_per_month, na.rm=TRUE)
scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)] <- median(scoringData$reviews_per_month, na.rm=TRUE)
##deal with dummies
#host_about
data$host_about <- ifelse(is.na(data$host_about),0,1)
scoringData$host_about <- ifelse(is.na(scoringData$host_about),0,1)
data$host_about = as.factor(data$host_about)
scoringData$host_about = as.factor(scoringData$host_about)
#host_is_superhost>>dummy
data$host_is_superhost <- ifelse(data$host_is_superhost == "t",1, 0)
scoringData$host_is_superhost <- ifelse(scoringData$host_is_superhost == "t",1, 0)
data$host_is_superhost[is.na(data$host_is_superhost)] <- 0
scoringData$host_is_superhost[is.na(scoringData$host_is_superhost)] <- 0
data$host_is_superhost = as.factor(data$host_is_superhost)
scoringData$host_is_superhost = as.factor(scoringData$host_is_superhost)
#host_identity_verified>>dummy
data$host_identity_verified <- ifelse(data$host_identity_verified == "t", 1, 0)
scoringData$host_identity_verified <- ifelse(scoringData$host_identity_verified == "t", 1, 0)
data$host_identity_verified[is.na(data$host_identity_verified)] <- 0
scoringData$host_identity_verified[is.na(scoringData$host_identity_verified)] <- 0
data$host_identity_verified = as.factor(data$host_identity_verified)
scoringData$host_identity_verified = as.factor(scoringData$host_identity_verified)
#host_has_profile_pic
data$host_has_profile_pic <- ifelse(data$host_has_profile_pic == "t",1,0)
scoringData$host_has_profile_pic <- ifelse(scoringData$host_has_profile_pic == "t",1,0)
data$host_has_profile_pic[is.na(data$host_has_profile_pic)] <- 0
scoringData$host_has_profile_pic[is.na(scoringData$host_has_profile_pic)] <- 0
data$host_has_profile_pic = as.factor(data$host_has_profile_pic)
scoringData$host_has_profile_pic = as.factor(scoringData$host_has_profile_pic)
#neighbourhood_group_cleansed
data$neighbourhood_group_cleansed = as.factor(data$neighbourhood_group_cleansed)
scoringData$neighbourhood_group_cleansed = as.factor(scoringData$neighbourhood_group_cleansed)
#property_type
#scoringData$property_type[scoringData$property_type == "Casa particular (Cuba)"] <- "Other"
#scoringData$property_type[scoringData$property_type == "Castle"] <- "Other"
#scoringData$property_type[scoringData$property_type == "Farm stay"] <- "Other"
data$property_type = as.factor(data$property_type)
scoringData$property_type = as.factor(scoringData$property_type)
#summary(scoringData$property_type)
#room_type
data$room_type = as.factor(data$room_type)
scoringData$room_type = as.factor(scoringData$room_type)
#cancellation_policy
data$cancellation_policy <- ifelse(data$cancellation_policy == "flexible",1,0)
scoringData$cancellation_policy <- ifelse(scoringData$cancellation_policy == "flexible",1,0)
data$cancellation_policy = as.factor(data$cancellation_policy)
scoringData$cancellation_policy = as.factor(scoringData$cancellation_policy)
#review_scores_rating2
data$review_scores_rating2 = (data$review_scores_rating)^10
#minimum_nights2
data$minimum_nights2 = (data$minimum_nights)^(1/200)
#first_review2 
data$first_review2 = (data$first_review)^(2)
#review_scores_cleanliness2
data$review_scores_cleanliness2 = (data$review_scores_cleanliness)^4
#last_review2
data$last_review2 = (data$last_review)^(1/200)
#calculated_host_listings_count_private_rooms
data$calculated_host_listings_count_private_rooms2 = (data$calculated_host_listings_count_private_rooms)^(1/200)
#review_scores_value
data$review_scores_value2 = (data$review_scores_value)^200
#zipcode
data$zipcode = as.factor(data$zipcode)

#data exploration
colSums(is.na(data))
max(data$price)
min(data$price)
table(data$neighbourhood_cleansed)
table(data$neighbourhood_group_cleansed)
table(data$host_since)
table(data$property_type)
cor(data$price, data$beds)
cor(data$price, data$number_of_reviews)
cor(data$price, data$review_scores_rating)
cor(data$price, data$minimum_nights)
cor(data$price, data$bathrooms)
cor(data$price, data$accommodates)
cor(data$price, data$guests_included)
cor(data$price, data$security_deposit)
cor(data$price, data$availability_365)
cor(data$price, data$first_review)
cor(data$price, data$days)
cor(data$price, data$cleaning_fee)
OutVals = boxplot(data$number_of_reviews)$out
OutVals

#split
set.seed(27)
split = sample.split(Y = data$price, SplitRatio = 0.8)
train = data[split,]
test = data[!split,]

#feature selection on numeric variables: 
#'host_listings_count','accommodates','bathrooms','bedrooms','beds','security_deposit',
#'cleaning_fee','guests_included','extra_people','minimum_nights','maximum_nights',
#'minimum_minumum_nights','maximum_minimum_nights','minimum_maximum_nights','maximum_maximum_nights',
#'minimum_nights_avg_ntm','maximum_nights_avg_ntm','availability_365','number_of_reviews',number_of_reviews_ltm',
#'first_review','last-review','review_scores_rating','review_scores_accuracy','review_scores_cleanliness',
#'review_scores_checkin','review_scores_communication','review_scores_value','calculated_host_listings_count',
#'calculated_host_listings_count_entire_homes','calculated_host_listings_count_private_rooms','calculated_host_listings_count_shared_rooms',
#'reviews_per_month'
start_mod = lm(price~1,data)
empty_mod = lm(price~1,data)
full_mod = lm(price~host_listings_count+accommodates+bathrooms+bedrooms+beds+security_deposit+
                    cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+
                    minimum_minimum_nights+maximum_minimum_nights+minimum_maximum_nights+maximum_maximum_nights+
                    minimum_nights_avg_ntm+maximum_nights_avg_ntm+availability_365+number_of_reviews+
                    number_of_reviews_ltm+first_review+last_review+review_scores_rating+review_scores_accuracy+
                    review_scores_cleanliness+review_scores_checkin+review_scores_communication+
                    review_scores_value+calculated_host_listings_count+calculated_host_listings_count_entire_homes+
                    calculated_host_listings_count_private_rooms+calculated_host_listings_count_shared_rooms+
                    reviews_per_month,data)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')
summary(hybridStepwise)

#first test on linear model
model1 = lm(price~accommodates + cleaning_fee + bathrooms +
              review_scores_rating2 + calculated_host_listings_count_private_rooms2 + 
              review_scores_value2 + bedrooms + security_deposit + number_of_reviews_ltm + 
              extra_people + calculated_host_listings_count_shared_rooms + first_review2+
              minimum_nights2 + review_scores_cleanliness2 + beds + last_review2 + 
              +host_is_superhost+host_has_profile_pic+host_identity_verified
            +neighbourhood_group_cleansed+property_type
            +room_type+cancellation_policy, train)
pred1 = predict(model1,newdata=scoringData)
rmse1= sqrt(mean((pred1-test$price)^2)); rmse1

#second test on random forest
Tree = randomForest(price~accommodates + cleaning_fee + bathrooms +
                       review_scores_rating2 + calculated_host_listings_count_private_rooms2 + 
                       review_scores_value2 + bedrooms + security_deposit + number_of_reviews_ltm + 
                       extra_people + calculated_host_listings_count_shared_rooms + first_review2+
                       minimum_nights2 + review_scores_cleanliness2 + beds + last_review2 + 
                     +host_is_superhost+host_has_profile_pic+host_identity_verified
                     +neighbourhood_group_cleansed+property_type
                     +room_type+cancellation_policy, train, ntree = 1000)

predTree = predict(Tree, newdata=test)
rmseTree= sqrt(mean((predTree-test$price)^2)); rmseTree

TreeF = randomForest(price~accommodates + cleaning_fee + bathrooms +
                       review_scores_rating + calculated_host_listings_count_private_rooms + 
                       review_scores_value + bedrooms + security_deposit + number_of_reviews_ltm + 
                       extra_people + calculated_host_listings_count_shared_rooms + 
                       minimum_nights + review_scores_cleanliness + beds + last_review + 
                       +host_is_superhost+host_has_profile_pic+host_identity_verified
                     +neighbourhood_group_cleansed+property_type
                     +room_type+cancellation_policy, data, ntree = 100000)
predTreeF = predict(TreeF,newdata=scoringData)
rmseTreeF= sqrt(mean((predTree-test$price)^2)); rmseTreeF

TreeV = randomForest(price~accommodates + cleaning_fee + bathrooms +
                       review_scores_rating + calculated_host_listings_count_private_rooms + 
                       review_scores_value + bedrooms + security_deposit + number_of_reviews_ltm + 
                       extra_people + calculated_host_listings_count_shared_rooms + 
                       minimum_nights + review_scores_cleanliness + beds + last_review + 
                       +host_is_superhost+host_has_profile_pic+host_identity_verified
                     +neighbourhood_group_cleansed+property_type
                     +room_type+cancellation_policy, data, ntree = 100000)
predTreeV = predict(TreeV,newdata=scoringData)
rmseTreeV= sqrt(mean((predTree-test$price)^2)); rmseTreeV

#third test on gbm
boost = gbm(price~accommodates + cleaning_fee + bathrooms + zipcode
              review_scores_rating2 + calculated_host_listings_count_private_rooms2 + 
              review_scores_value2 + bedrooms + security_deposit + number_of_reviews_ltm + 
              extra_people + calculated_host_listings_count_shared_rooms + first_review2+
              minimum_nights2 + review_scores_cleanliness2 + beds + last_review2 + 
              +host_is_superhost+host_has_profile_pic+host_identity_verified
            +neighbourhood_group_cleansed+property_type
            +room_type+cancellation_policy, data=train,distribution="gaussian",
            n.trees = 10000,interaction.depth = 3,shrinkage = 0.001)
predBoost = predict(boost,newdata=test,n.trees = 10000)
rmseBoost = sqrt(mean((predBoost-test$price)^2)); rmseBoost

boost2 = gbm(price~zipcode + accommodates + cleaning_fee + bathrooms + review_scores_rating2 + 
               calculated_host_listings_count_private_rooms2 + review_scores_value2 + 
               bedrooms + security_deposit + number_of_reviews_ltm + extra_people + 
               calculated_host_listings_count_shared_rooms + minimum_nights2 + 
               review_scores_cleanliness2 + beds + last_review2 + review_scores_checkin + 
               host_listings_count + maximum_minimum_nights + minimum_nights_avg_ntm + 
               guests_included + calculated_host_listings_count + minimum_minimum_nights + 
               number_of_reviews + first_review2 +
               host_is_superhost+host_has_profile_pic+host_identity_verified+
               neighbourhood_group_cleansed+property_type+
               room_type+cancellation_policy, data=train,distribution="gaussian", n.trees = 1000000,interaction.depth = 40,shrinkage = 0.001)
predBoost2 = predict(boost2,newdata=test,n.trees = 1000000)
rmseBoost2 = sqrt(mean((predBoost2-test$price)^2)); rmseBoost2

boost3 = gbm(price~zipcode + accommodates + cleaning_fee + bathrooms + review_scores_rating + 
               calculated_host_listings_count_private_rooms + review_scores_value + 
               bedrooms + security_deposit + number_of_reviews_ltm + extra_people + 
               calculated_host_listings_count_shared_rooms + minimum_nights + 
               review_scores_cleanliness + beds + last_review + review_scores_checkin + 
               host_listings_count + maximum_minimum_nights + minimum_nights_avg_ntm + 
               guests_included + calculated_host_listings_count + minimum_minimum_nights + 
               number_of_reviews + first_review+
               host_is_superhost+host_has_profile_pic+host_identity_verified+
               neighbourhood_group_cleansed+property_type+
               room_type+cancellation_policy, data=train,distribution="gaussian", n.trees = 1000000,interaction.depth = 44,shrinkage = 0.01)
predBoost3 = predict(boost3,newdata=test,n.trees = 1000000)
rmseBoost3 = sqrt(mean((predBoost3-test$price)^2)); rmseBoost3

#final gbm
boostS = gbm(price~, zipcode + accommodates + cleaning_fee + bathrooms + review_scores_rating2 + 
               calculated_host_listings_count_private_rooms2 + review_scores_value2 + 
               bedrooms + security_deposit + number_of_reviews_ltm + extra_people + 
               calculated_host_listings_count_shared_rooms + minimum_nights2 + 
               review_scores_cleanliness2 + beds + last_review2 + review_scores_checkin + 
               host_listings_count + maximum_minimum_nights + minimum_nights_avg_ntm + 
               guests_included + calculated_host_listings_count + minimum_minimum_nights + 
               number_of_reviews + first_review2 +
               host_is_superhost+host_has_profile_pic+host_identity_verified+
               neighbourhood_group_cleansed+property_type+
               room_type+cancellation_policy, data,distribution="gaussian", n.trees = 1000000,interaction.depth = 44,shrinkage = 0.01)
predBoostS = predict(boostS,newdata=scoringData,n.trees = 1000000)

#submission
submissionFile = data.frame(id = scoringData$id, price = predBoostS)
write.csv(submissionFile, 'last_submission_yihengshen.csv',row.names = F)
