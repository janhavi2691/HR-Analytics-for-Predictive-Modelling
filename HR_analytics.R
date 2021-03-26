#This is a HR Analytics case study implemented in R #
#Load data
train_hr<-read.csv("aug_train.csv",na.strings = c("","","NA"),
                   stringsAsFactors = T)
test_hr<-read.csv("aug_test.csv",na.strings = c("","","NA"),
                  stringsAsFactors = T)
#Get dimensions of loaded data
dim(train_hr)
dim(test_hr)
#View entire data
View(train_hr)
View(test_hr)
summary(train_hr)
summary(test_hr)

# combine train data and test data so that we can perform data imputation 
# on both the datasets simultaneously 
install.packages("dplyr")
library(dplyr)
# rbind(): needs same no of columns for train and test data ; our train data 
# contains one extra column i.e. target ; so we cant use rbind()
dim(train_hr)#19158    14
dim(test_hr) # 2129   13
combined_data<-bind_rows(train_hr,test_hr)
dim(combined_data)#21287    14
summary(combined_data$target)
# imputing missing values from the combined data 
colSums(is.na(combined_data))
# gender,enrolled_university,education_level,major_discipline,experience
# company_size,company_type,last_new_job

library(VIM)
k=sqrt(nrow(combined_data))
k
combined_data<-kNN(combined_data,variable = c("gender","enrolled_university",
                                              "education_level","major_discipline","experience",
                                              "company_size","company_type","last_new_job"),k=21)
summary(combined_data)

# imputing missing values with mode values
# imputing gender 
summary(combined_data$gender)
combined_data$gender[is.na(combined_data$gender)]<-"Male"

summary(combined_data$enrolled_university)
combined_data$enrolled_university[is.na(combined_data$enrolled_university)]<-"no_enrollment"

summary(combined_data$education_level)
combined_data$education_level[is.na(combined_data$education_level)]<-"Graduate"

summary(combined_data$major_discipline)
combined_data$major_discipline[is.na(combined_data$major_discipline)]<-"STEM"

summary(combined_data$experience)
combined_data$experience[is.na(combined_data$experience)]<-">20"

summary(combined_data$company_size)
combined_data$company_size[is.na(combined_data$company_size)]<-"50-99"

summary(combined_data$company_type)
combined_data$company_type[is.na(combined_data$company_type)]<-"Pvt Ltd"

summary(combined_data$last_new_job)
combined_data$last_new_job[is.na(combined_data$last_new_job)]<-"1"

colSums(is.na(combined_data))

summary(combined_data)
summary(combined_data$city)
levels(combined_data$city)
combined_data$enrollee_id<-NULL
combined_data$city<-NULL

summary(combined_data$experience)
# 1,2,3,4,5 = 1 to 5
# 6,7,8,9,10 = 6 to 10
# 11,12,13,14,15= 11 to 15
# 16,17,18,19 and 20 = 16 to 20
# level reduction 
combined_data$experience<-as.character(combined_data$experience)
combined_data$experience[combined_data$experience=="1"|
                           combined_data$experience=="2"|
                           combined_data$experience=="3"|
                           combined_data$experience=="4"|
                           combined_data$experience=="5"]<-"1 to 5"


combined_data$experience[combined_data$experience=="6"|
                           combined_data$experience=="7"|
                           combined_data$experience=="8"|
                           combined_data$experience=="9"|
                           combined_data$experience=="10"]<-"6 to 10"

combined_data$experience[combined_data$experience=="11"|
                           combined_data$experience=="12"|
                           combined_data$experience=="13"|
                           combined_data$experience=="14"|
                           combined_data$experience=="15"]<-"11 to 15"


combined_data$experience[combined_data$experience=="16"|
                           combined_data$experience=="17"|
                           combined_data$experience=="18"|
                           combined_data$experience=="19"|
                           combined_data$experience=="20"]<-"16 to 20"

combined_data$experience<-as.factor(combined_data$experience)
summary(combined_data$experience)
summary(combined_data)

train_hr<-combined_data[1:19158,] 
test_hr<-combined_data[19159:21287,]

#implementing BLR on train data
table(train_hr$target) 
# no of observations for "0" are 3-4 times greater than no of observations 
# in "1" 

# We need to change threshold from 0.5 
# we need to take uniform data and implement BLR on it 

# uniform data contains almost similar no of observations for all categories 
# 4771 "1" 
# 14981 "0" out of this we will select random 5000 "0" 

# we need to use sample() as we want to generate sample of 5000 random "0" 
# from 14981 "0" 

# we need to use set.seed() as we may need to replicate the same random no 
# in future

set.seed(200)
index <- sample(14381,5000)
head(index)

# To check whether the data is balanced (equal no. of samples of both classes "0" and "1") or not
one<-subset(train_hr,train_hr$target=="1") # 4777 
zero<-subset(train_hr,train_hr$target=="0")# 14381 
dim(one) 
dim(zero) 
seleted_zero<-zero[index,] # 5000 
dim(seleted_zero)


hr_uniformdata <- rbind(one,seleted_zero)
dim(hr_uniformdata)

# implement BLR on given uniform data 
# as data contains 12 columns are most of them are factor ; it will be 
# difficult to study the coefficints as it will create large no of coefficients for the model

#exp: 10  levels : 9 coefficients
#Step wise regression to know the significant variables


null_hr <- glm(target~1,data = hr_uniformdata,family = "binomial")
full_hr <- glm(target~.,data =  hr_uniformdata, family = "binomial")

step(null_hr, direction = "forward", scope = list(lower = null_hr, upper= full_hr))  


hr_analytics <- glm(formula = target ~ city_development_index + company_size_imp + 
                      education_level + company_size + company_type + last_new_job + 
                      experience + company_type_imp + enrolled_university + relevent_experience + 
                      gender + training_hours, family = "binomial", data = hr_uniformdata)


#predict the probability of every observation being 1 -looking for job change

pred_prob_churn <- predict(hr_analytics,hr_uniformdata,type = "response")
head(pred_prob_churn)

#when we take uniform data: throshold must be 0.5
pred_target_uniform <- ifelse(pred_prob_churn > 0.5, "1" , "0")
head(pred_target_uniform)


#Compare our predictions with actual target values
#make use of table function to create confusion matrix

table(actual = hr_uniformdata$target, predicted = pred_target_uniform) 
#     predicted
#actual  0    1             ## 3695 are actual non churners predicted as non churners
#0      3695 1305           ## 1305 are actual non churners predicted as churners
#1      1237 3540           ## 1237 are actual churners predicted as non churners
                            ## 3540 are actual churners predicted as churners

##Accuracy = (correctly identified "1" + correctly identified "0")/ total no of observations
accuracy = (3695+3540)/nrow(hr_uniformdata);accuracy

##Misclassification = (wrongly identified "1" + wrongly identified "0")/ total no of observations
                 #  = 1- accuracy
missclassification = (1237+1305) /nrow(hr_uniformdata);missclassification


##sensitivity = (correctly identified "1" / total no of actual "1"s) = TPR = 1- FNR
sensitivity  = 3540 / (3540+1237);sensitivity


##Specificity = (correctly identified "0" / total no of actual "0"s) = TNR = 1 - FPR
specificity = 3695 / (3695+1305);specificity



#As accuracy, sensitivity and Specificity all are > 0.6, model is a good fit
#Implement the model on full data# this model is created on uniform/balanced data

hr_predict_prob_complete <- predict(hr_analytics,train_hr,type = "response")
head(hr_predict_prob_complete)
#using threshold 0.5 lets categorize prob to "1" or "0"

hr_pred_prob_complete <- ifelse(hr_predict_prob_complete > 0.5, "1","0")
head(hr_pred_prob_complete)

table(actual= train_hr$target,predicted=hr_pred_prob_complete)

##Accuracy = (correctly identified "1" + correctly identified "0")/ total no of observations
accuracy1 = (3540+10638)/nrow(train_hr);accuracy1

##Misclassification = (wrongly identified "1" + wrongly identified "0")/ total no of observations
#  = 1- accuracy
missclassification1 = (3743+1237) /nrow(train_hr);missclassification1


##sensitivity = (correctly identified "1" / total no of actual "1"s) = TPR = 1- FNR
sensitivity1  = 3540 / (3540+1237);sensitivity1


##Specificity = (correctly identified "0" / total no of actual "0"s) = TNR = 1 - FPR
specificity1 = 10638 / (10638+3743);specificity1
#Good acc, specifivity, sensitivity > 0.6 Hence good fit for unbalanced data


###### model without converting into balanced data #########
# without taking uniform data ; we can run BLR but in that case we need to 
# manipulate value of threshold. 
null_hr<-glm(target~1,data = train_hr,family = "binomial") 
full_hr<-glm(target~.,data = train_hr,family = "binomial")

step(null_hr,direction = "forward", scope = list(lower = null_hr, upper = full_hr) )

hr_analytics_traindata <- glm(formula = target ~ city_development_index + company_size_imp + 
          education_level + company_size + company_type + experience + 
          last_new_job + company_type_imp + enrolled_university + relevent_experience + 
          training_hours + major_discipline_imp, family = "binomial", 
        data = train_hr)
pred_prob_traindata <- predict(hr_analytics_traindata,train_hr,type = "response")
pred_target_traindata <- ifelse(pred_prob_traindata>0.5,"1","0")
table(Actual = train_hr$target, Predicted = pred_target_traindata)

accuracy2 = (13427+1462)/nrow(train_hr);accuracy2 #0.7771688
sensitivity2  = 1462 / (1462+3315);sensitivity2 #0.3060498
specificity3 = 13427 / (13427+954);specificity3 #0.9336625
# acc and specficity > 0.6 but sensitivity < 0.6 bad fit model bcoz data is unbalanced


install.packages("ROCR")
library(ROCR)
#Apply threshold of start = 0.1 till 0.9 for pred_prob_traindata to compute predicted target 
#Compares actual and predicted target using every step of threshold
#we want high values and similar values of specifivity and sensitivity
pred=prediction(pred_prob_traindata,train_hr$target) 
# x axis = "tpr"= sensitivity, y axis = "tnr" = specifivity 
perf= performance(pred,"tpr","tnr") 
# Plot performace given by perf and cutoff sequence startts at 0.1 till 1.0 and increment = 0.1
plot(perf,colorize=T,print.cutoffs.at=seq(0.2,0.3,0.03)) 
# 0.23 is finding good cut off as tpr = 0.78, tnr = 0.7 as per plot
# consider threshold = 0.23 and recompute our  targets
pred_target_traindata_new <- ifelse(pred_prob_traindata>0.23,"1","0")
table(Actual = train_hr$target, Predicted = pred_target_traindata_new)

accuracy4 = (10147+3748)/nrow(train_hr);accuracy4 #0.7252845
sensitivity4  = 3748 / (3748+1029);sensitivity4 #0.7845928
specificity4 = 10147 / (10147+4234);specificity4 #0.7055838



#########Predicting values of target on test data############

# 1. Prediction using uniform model
pred_prob_testdata <- predict(hr_analytics,test_hr,type="response")
summary(test_hr$target)
test_hr$target <- ifelse(pred_prob_testdata > 0.5, "1","0")


#2. Prediction using 0.23 threshold
pred_prob_testdata_0.23 <- predict(hr_analytics_traindata,test_hr,type = "response")
test_hr$target_0.23 <- ifelse(pred_prob_testdata_0.23 > 0.23 , "1", "0")

head(test_hr)
#Compare value of target with target_0.23
# find how many values are similar 
# create column and write TRUE if both the value matches else write false 
# count no of TRUE and FALSE
test_hr$comp_both <- ifelse(test_hr$target == test_hr$target_0.23, "T","F")
head(test_hr,50)
true_cnt <- nrow(test_hr$comp_both == "T");true_cnt

