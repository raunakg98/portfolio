#install libraries
#install.packages("ROCR")

#load libraries
library(tidyverse)
library(caret)
library(ROCR)
library(SnowballC)
library(glmnet)
library(randomForest)
library(gbm)
#get rid of scientific notations
options(scipen = 999)

#load data files
train_x <- read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")

###merge train_x and test_x to clean the data together
merged_df <- rbind(train_x, test_x)
merged_df <- rownames_to_column(merged_df, var = "row_num")
merged_df$row_num <- parse_number(merged_df$row_num)

############data cleaning
merged_df <- merged_df %>%
  mutate(cancellation_policy = as.factor(ifelse(cancellation_policy == "super_strict_30" | cancellation_policy == "super_strict_60",
                                                "strict",cancellation_policy)),
         cleaning_fee = parse_number(merged_df$cleaning_fee),
         cleaning_fee = ifelse(is.na(cleaning_fee),0,cleaning_fee),
         has_cleaning_fee = as.factor(ifelse(cleaning_fee>0,"YES","NO")),
         price = parse_number(merged_df$price),
         price = ifelse(is.na(price),0,price),
         bedrooms = ifelse(is.na(bedrooms),mean(merged_df$bedrooms, na.rm = TRUE),bedrooms),
         beds = ifelse(is.na(beds),mean(merged_df$beds, na.rm = TRUE),beds),
         host_total_listings_count = ifelse(is.na(merged_df$host_total_listings_count),
                                            mean(host_total_listings_count, na.rm = TRUE),host_total_listings_count),
         price_per_person = price/accommodates,
         has_cleaning_fee = as.factor(ifelse(cleaning_fee>0, 'YES','NO')),
         bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed","other")),
         property_category = as.factor(case_when(property_type=='Apartment' ~ "apartment",
                                                 property_type=='Serviced apartment' ~ "apartment",
                                                 property_type=='Loft' ~ "apartment",
                                                 property_type=='Bed & Breakfast' ~ "hotel",
                                                 property_type=='Boutique hotel' ~ "hotel",
                                                 property_type=='Hostel' ~ "hotel",
                                                 property_type=='Townhouse' ~ "condo",
                                                 property_type=='Condominium' ~ "condo",
                                                 property_type=='House' ~ "house",
                                                 property_type=='Bungalow' ~ "house",
                                                 TRUE ~ 'other')),
         bed_type = as.factor(bed_type),
         room_type = as.factor(room_type),
         property_category = as.factor(property_category))

property_category_medians <- merged_df %>% group_by(property_category)
property_category_medians <- property_category_medians %>% summarise(property_ppp_medeian = median(price_per_person))
merged_df <- merge(x=merged_df,y=property_category_medians, by="property_category")
merged_df <- merged_df %>% mutate(ppp_ind = case_when(price_per_person > property_ppp_medeian ~ 1,
                                              price_per_person <= property_ppp_medeian ~ 0),
                                  market = ifelse(is.na(market),"MISSING",market))

#summary(merged_df)
#cleaning part 3
merged_df <- merged_df %>% group_by(market)%>%mutate(count=n())%>%ungroup()%>%
  mutate(bathrooms = ifelse(is.na(merged_df$bathrooms),median(merged_df$bathrooms, na.rm = TRUE),bathrooms),
         host_is_superhost = ifelse(is.na(merged_df$host_is_superhost),FALSE,host_is_superhost),
         host_is_superhost = factor(host_is_superhost, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         extra_people = parse_number(merged_df$extra_people),
         charges_for_extra = as.factor(ifelse(extra_people>0,"YES","NO")),
         host_acceptance_rate = parse_number(merged_df$host_acceptance_rate),
         host_acceptance = as.factor(case_when(host_acceptance_rate==100 ~ "ALL",
                                               is.na(host_acceptance_rate) ~ "MISSING",
                                               host_acceptance_rate<100 ~ "SOME")),
         host_response_rate = parse_number(merged_df$host_response_rate),
         host_response = as.factor(case_when(host_response_rate==100 ~ "ALL",
                                             is.na(host_response_rate) ~ "MISSING",
                                             host_response_rate<100 ~ 'SOME')),
         has_min_nights = as.factor(ifelse(minimum_nights>1,"YES","NO")),
         market = as.factor(case_when(count<470 ~ "OTHER",TRUE ~ merged_df$market)))
#summary(merged_df)

###########raunak_cleaning
merged_df <- merged_df %>% group_by(state)%>%mutate(count=n())%>%ungroup()%>%
  mutate(host_has_profile_pic = ifelse(is.na(merged_df$host_has_profile_pic),FALSE,host_has_profile_pic),
         host_has_profile_pic = factor(host_has_profile_pic, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         host_identity_verified = ifelse(is.na(merged_df$host_identity_verified),FALSE,host_identity_verified),
         host_identity_verified = factor(host_identity_verified, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         instant_bookable = factor(instant_bookable, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         is_business_travel_ready = ifelse(is.na(merged_df$is_business_travel_ready),FALSE,is_business_travel_ready),
         is_business_travel_ready = factor(is_business_travel_ready, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         is_location_exact = factor(is_location_exact, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         license_status = as.factor(ifelse(is.na(license), "NO",
                                  ifelse(grepl("pending", license, ignore.case = TRUE), "PENDING", "YES"))),
         monthly_price = parse_number(merged_df$monthly_price),
         monthly_price = ifelse(is.na(monthly_price),0,monthly_price),
         require_guest_phone_verification = factor(require_guest_phone_verification, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         require_guest_profile_picture = factor(require_guest_profile_picture, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         requires_license = factor(requires_license, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         security_deposit = parse_number(merged_df$security_deposit),
         security_deposit = ifelse(is.na(security_deposit),0,security_deposit),
         has_security_deposit = as.factor(cut(security_deposit, 
                                      breaks=c(-Inf, 0,100, 200, 400, Inf),
                                      labels=c("No-Deposit","Low", "Medium-Low", "Medium", "High"),
                                      include.lowest=TRUE)),
         state = as.factor(case_when(count<300 ~ "OTHER",TRUE ~ merged_df$state)),
         area = as.factor(cut(square_feet, 
                              breaks=c(-Inf,372, 750, 913, Inf),
                              labels=c("Small", "Medium-Small", "Medium", "Large"),
                              include.lowest=TRUE)),
         weekly_price = parse_number(merged_df$weekly_price),
         weekly_price = ifelse(is.na(weekly_price),0,weekly_price),
         host_total_listings_count = ifelse(is.na(merged_df$host_total_listings_count),
                                            mean(host_total_listings_count, na.rm = TRUE),host_total_listings_count)
  )
merged_df <- merged_df[, order(names(merged_df))]
#summary(merged_df)
#hist(parse_number(merged_df$security_deposit))
##############cleaning end

#reorder shuffled rows
merged_df <- merged_df[order(merged_df$row_num),]

#split into train test after cleaning
train_x_clean <- merged_df[1:99981, ]
test_x_clean <- merged_df[99982:112186, ]

#train_x[99981,]
#train_x_clean[99981,]
#test_x[12205,]
#test_x_clean[12205,]

#join the training y to the training x file
#also turn the target variables into factors
train <- cbind(train_x_clean, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score))%>%select(-high_booking_rate)

test <- test_x_clean

numeric_cols <- sapply(train, is.numeric)

#find highly correlated predictors
cor(train[, numeric_cols], use = "pairwise.complete.obs")

#exclude monthly_price and beds
#train <- train %>% select(-monthly_price, -beds)

# EXAMPLE PREDICTIONS FOR CONTEST 1

#split data into training and validation
train_insts = sample(nrow(train), .7*nrow(train))
data_train <- train[train_insts,]
data_valid <- train[-train_insts,]
modeling_df <- train %>% select(perfect_rating_score,accommodates,bathrooms,bed_category,bed_type,bedrooms,beds,cancellation_policy,
                                charges_for_extra,guests_included,has_cleaning_fee, has_min_nights,has_security_deposit,host_acceptance,host_has_profile_pic,
                                host_identity_verified ,host_is_superhost,host_response,host_total_listings_count,instant_bookable ,is_business_travel_ready,
                                is_location_exact, license_status,market,monthly_price,ppp_ind,price,property_category,require_guest_phone_verification,
                                require_guest_profile_picture, requires_license,room_type,state,weekly_price)
modeling_df_test <- test %>% select(accommodates,bathrooms,bed_category,bed_type,bedrooms,beds,cancellation_policy,
                                    charges_for_extra,guests_included,has_cleaning_fee, has_min_nights,has_security_deposit,host_acceptance,host_has_profile_pic,
                                    host_identity_verified ,host_is_superhost,host_response,host_total_listings_count,instant_bookable ,is_business_travel_ready,
                                    is_location_exact, license_status,market,monthly_price,ppp_ind,price,property_category,require_guest_phone_verification,
                                    require_guest_profile_picture, requires_license,room_type,state,weekly_price)

#create dummy variables for all variables
dummy <- dummyVars( ~ . , data=modeling_df,fullRank = TRUE)
dummy_test <- dummyVars(~.,data=modeling_df_test,fullRank = TRUE)

#one hot encoding for train
one_hot_airbnb <- data.frame(predict(dummy, newdata = modeling_df))
#one_hot_airbnb$perfect_rating_score = as.factor(one_hot_airbnb$perfect_rating_score)
#one_hot_airbnb <- select(one_hot_airbnb, -(perfect_rating_score.YES))

#one hot encoding for test
one_hot_airbnb_test <- data.frame(predict(dummy_test, newdata = modeling_df_test))

# remove the target variable from the matrix of features
airbnb_x <- select(one_hot_airbnb, -(perfect_rating_score.YES))

# movies_y is a factor
airbnb_y <- one_hot_airbnb$perfect_rating_score.YES

#split the data into train and valid
train_insts = sample(nrow(one_hot_airbnb), .7*nrow(one_hot_airbnb))
data_train <- airbnb_x[train_insts,]
data_valid <- airbnb_x[-train_insts,]

data_train_y <- airbnb_y[train_insts]
data_test_y <- airbnb_y[-train_insts]
#logformula <- perfect_rating_score~accommodates+bedrooms+beds+cancellation_policy+has_cleaning_fee+host_total_listings_count+price+ppp_ind+property_category+bed_category+
#  bathrooms+charges_for_extra+host_acceptance+host_response+has_min_nights+market+host_is_superhost

#family="binomial" yields logistic regression; family="gaussian" yields linear regression
#alpha = 1 yields the lasso penalty, and alpha = 0 the ridge penalty
#basic, default
glm.out.ridge <- glmnet(data_train, data_train_y, alpha = 0, family="binomial")
glm.out.lasso <- glmnet(data_train, data_train_y, alpha = 1, family="binomial")

#shows coefficient values vs. lambda
plot(glm.out.ridge, xvar = "lambda")
plot(glm.out.lasso, xvar = "lambda")
##############
data_valid_matrix <- as.matrix(data_valid)
preds <- predict(glm.out.lasso, newx = data_valid_matrix, type = "response")
###############
#what are the coefficients for an unregularized model?
unreg <- glmnet(data_train, data_train_y, family = "binomial", alpha = 0, lambda = 0)
coef(unreg)

#what if we have a very large lambda?
lotsofreg <- glmnet(data_train, data_train_y, family = "binomial", alpha = 0, lambda = 10)
coef(lotsofreg)
############## random forest ##########################
##start with random forest
##mtry = number of variables to try at each split 
#set mtry ~= sqrt(17) = 4 
#set 1000 trees (this is something you can tune)

rf.mod <- randomForest(perfect_rating_score~.,
                       data=train,
                       subset=train_insts,
                       mtry=15, ntree=10000,
                       importance=TRUE)

rf_preds <- predict(rf.mod, newdata=data_valid)
rf_acc <- mean(ifelse(rf_preds==data_valid$perfect_rating_score,1,0))

rf.mod
rf_acc

importance(rf.mod)
varImpPlot(rf.mod)
#############################################################
#seq function to generate a large list
grid <- 10^seq(-1,-4,length=100)
grid

# define a function to calculate accuracy
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}

#storage vector
accs <- rep(0, length(grid))

for(i in c(1:length(grid))){
  lam = grid[i] #current value of lambda
  
  #train a lasso model with lambda = lam
  glmout <- glmnet(data_train, data_train_y, family = "binomial", alpha = 1, lambda = lam)
  
  #make predictions as usual
  data_valid_matrix <- as.matrix(data_valid)
  preds <- predict(glmout, newx = data_valid_matrix, type = "response")
  
  #classify and compute accuracy
  classifications <- ifelse(preds > .5, 1, 0)
  inner_acc <- accuracy(classifications, data_test_y)
  accs[i] <- inner_acc
}

#plot fitting curve - easier to read if we plot logs
plot(log10(grid), accs)

best_validation_index <- which.max(accs)
best_lambda <- grid[best_validation_index]

best_lambda

#print coefficients for best lambda
coef(glmnet(data_train, data_train_y, family = "binomial", alpha = 1, lambda = best_lambda))
optimal_lasso = glmnet(data_train, data_train_y, family = "binomial", alpha = 1, lambda = best_lambda)
#logistic_perfect <- glm(perfect_rating_score~., data = data_train, family = "binomial")
data_valid_matrix <- as.matrix(data_valid)
probs_perfect <- predict(optimal_lasso, newx = data_valid_matrix, type = "response")
probs_perfect <- ifelse(is.na(probs_perfect), 0, probs_perfect)

#make binary classifications based on BEST CUTOF (make sure to check for NAs!)
classifications_perfect <- ifelse(probs_perfect > .46, 1, 0)
classifications_perfect <- ifelse(is.na(classifications_perfect), 0, classifications_perfect)
summary(classifications_perfect)

valid_actuals <- data_test_y
valid_actuals <- ifelse(valid_actuals == 1, 1, 0)
valid_actuals <- ifelse(is.na(valid_actuals), 0, valid_actuals)
valid_actuals <- as.factor(valid_actuals)

valid_classifications <- as.factor(classifications_perfect)
CM = confusionMatrix(data = valid_classifications, #predictions
                     reference = valid_actuals, #actuals
                     positive="1")
#CM$table
ACC <- CM$overall["Accuracy"]
ACC
# True Positives (were predicted to be positives, are actually positives)
TP <- CM$table[2,2]
# True Negatives (were predicted to be negatives, are actually negatives)
TN <- CM$table[1,1]
# False Positives (were predicted to be positives, are actually negatives)
FP <- CM$table[2,1]
# False Negatives (were predicted to be negatives, are actually positives)
FN <- CM$table[1,2]
#find TPR
TPR <- TP/(TP+FN)
#find FPR
TNR <- TN/(TN+FP)
FPR <- 1-TNR
ACC
TPR
FPR

####ROCR
preds_full <- prediction(probs_perfect,data_valid$perfect_rating_score)

#create roc curve
roc_full <- performance(preds_full, "sens", "spec")
plot(roc_full, avg= "threshold", colorize=TRUE, lwd= 3,
     main= "With ROCR you can produce standard plots like ROC curves ...")
plot(roc_full, lty=3, col="grey78", add=TRUE)


#plot curve
plot(roc_full)

coords <- coords(roc_full, "threshold", "youden")
optimal_cutoff <- coords[[1]]

#output your predictions
#they must be in EXACTLY this format
#a .csv file with the naming convention targetvariable_groupAAA.csv, where you replace targetvariable with your chosen target, and AAA with your group name
#in exactly the same order as they are in the test_x file


#create dummy variables for all variables
dummy_test <- dummyVars( ~ . , data=modeling_df_test,fullRank = TRUE)

#one hot encoding
one_hot_airbnb_test <- data.frame(predict(dummy_test, newdata = modeling_df_test))

logistic_perfect <- glm(perfect_rating_score~., data = one_hot_airbnb, family = "binomial")
probs_perfect <- predict(logistic_perfect, newdata = one_hot_airbnb_test, type = "response")
probs_perfect <- ifelse(is.na(probs_perfect), 0, probs_perfect)

probs_perfect
valid_actuals

# For perfect_rating_score, each row should be a binary YES (is perfect) or NO (not perfect)
#make binary classifications (make sure to check for NAs!)
classifications_perfect <- ifelse(probs_perfect > .45, "YES", "NO")
classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
summary(classifications_perfect)


#this code creates sample outputs in the correct format
write.table(classifications_perfect, "perfect_rating_score_group14.csv", row.names = FALSE)