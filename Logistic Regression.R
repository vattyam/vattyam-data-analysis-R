# Download the data set as german_credit

# Load the german.csv file into the dataframe german_credit dataframe

setwd("F:/Data Analytics/Assignment_October 2")
german_credit <- read.csv("german.csv")

# Load all the libraries used in the R file

library(ggplot2)
library(caTools)
library(MASS)
library(car)
library(Hmisc)
library(ROCR)
library(caret)

# Exploratory Data Analysis

# Checkpoint 1: Data Understanding  and Data Exploration

# Check the structure of dataset using str() function

str(german_credit)

# check the summary of the dataset using summary() function

summary(german_credit)

# Univariate analysis of 5 significant variables 

checking_account_aes <- ggplot(german_credit,aes(x=german_credit$Status.of.existing.checking.account))
plot1 <- checking_account_aes + geom_bar(col = "red",fill = "blue") + xlab("Status of exisitng checking account") + ylab("Frequency") + ggtitle("Status of Existing Checking Account - Univariate Analysis")  

credit_history_aes <- ggplot(german_credit,aes(x=german_credit$Credit.history))
plot2 <- credit_history_aes + geom_bar(col = "blue",fill = "red") + xlab("Credit history") + ylab("Frequency") + ggtitle("Credit History - Univariate Analysis")

purpose_aes <- ggplot(german_credit,aes(x = german_credit$Purpose))
plot3 <- purpose_aes + geom_bar(col = "red",fill = "dark green") + xlab("Purpose") + ylab("Frequency") + ggtitle("Purpose - Univariate Analysis")

duration_in_month_aes <- ggplot(german_credit,aes(x=german_credit$Duration.in.month))
plot4 <- duration_in_month_aes + geom_histogram(binwidth = 10,col="blue",fill="orange") + xlab("Duration in month") + ylab("Frequency") + ggtitle("Duration in month - Univariate Analysis")

present_employment_since_aes <- ggplot(german_credit,aes(x=german_credit$Present.employment.since.))
plot5 <- present_employment_since_aes + geom_bar(col = "blue",fill = "maroon") + xlab("Present employment since") + ylab("Frequency") + ggtitle("Present employment since - Univariate Analysis") 

# Checkpoint 2: Data Cleaning and Transformation

# Identifying missing values 

sum(is.na(german_credit))

# There are no missing values in the dataframe german_credit

# Outlier treatment for numeric variables 

# Identify the outliers using boxplots for numeric variables 

boxplot(german_credit$Duration.in.month,col = "red")
boxplot(german_credit$Credit.amount,col = "blue")
boxplot(german_credit$Age.in.Years,col = "brown")

# Identify outliers uisng percentile distribution

quantile(german_credit$Duration.in.month,seq(0,1,0.01))
quantile(german_credit$Credit.amount,seq(0,1,0.01))
quantile(german_credit$Age.in.Years,seq(0,1,0.01))

# There are no abnormal values for Age.in.Years and Duration.in.month variables and hence outlier treatment is not needed for them
# For Credit.amount amount variable, the outliers are removed that are identified using box plot 

# Calculate the Inter Quartile Range 

lower_hinge <- quantile(german_credit$Credit.amount,probs = 0.25)
upper_hinge <- quantile(german_credit$Credit.amount,probs = 0.75)

IQR <- (upper_hinge-lower_hinge) 
steps<- 1.5*IQR 

# Remove the values that are above the upper whisker in the Credit.amount variable

german_credit <- subset(german_credit,german_credit$Credit.amount < (upper_hinge + steps))


# Variable transformation to prepare the data for logistic regression as the model can take 
# only numeric variables 

# Dummy encoding for catogerical variables 

dummy_1 <- data.frame(model.matrix( ~Status.of.existing.checking.account, data = german_credit))
dummy_1<-dummy_1[,-1]

dummy_2 <- data.frame(model.matrix( ~Credit.history, data = german_credit))
dummy_2 <-dummy_2[,-1]

dummy_3 <- data.frame(model.matrix( ~Purpose, data = german_credit))
dummy_3 <-dummy_3[,-1]

dummy_4 <- data.frame(model.matrix( ~Savings.account.bonds, data = german_credit))
dummy_4 <-dummy_4[,-1]

dummy_5 <- data.frame(model.matrix( ~Present.employment.since., data = german_credit))
dummy_5 <-dummy_5[,-1]

dummy_6 <- data.frame(model.matrix( ~Personal.status.and.sex, data = german_credit))
dummy_6 <-dummy_6[,-1]

dummy_7 <- data.frame(model.matrix( ~Other.debtors...guarantors, data = german_credit))
dummy_7 <-dummy_7[,-1]

dummy_8 <- data.frame(model.matrix( ~Property, data = german_credit))
dummy_8 <-dummy_8[,-1]

dummy_9 <- data.frame(model.matrix( ~Other.installment.plans, data = german_credit))
dummy_9 <-dummy_9[,-1]

dummy_10 <- data.frame(model.matrix( ~Housing., data = german_credit))
dummy_10 <-dummy_10[,-1]

dummy_11 <- data.frame(model.matrix( ~Job_status, data = german_credit))
dummy_11 <-dummy_11[,-1]

# Converting the factor variables with two levels to numeric without using dummy encoding

levels(german_credit$Telephone.) <- c(0,1)
german_credit$Telephone. <- as.numeric((levels(german_credit$Telephone.)))[german_credit$Telephone.]

levels(german_credit$foreign.worker) <- c(0,1)
german_credit$foreign.worker <- as.numeric((levels(german_credit$foreign.worker)))[german_credit$foreign.worker]

# combine all the dummy variables and the numeric variables into the dataset 

german_credit_1 <- cbind(german_credit[ , c(2,5,8,11,13,16,18,19,20,21)], dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6,dummy_7,dummy_8,dummy_9,dummy_10,dummy_11)

german_credit <- german_credit_1

#Checkpoint 3: Splitting the Dataset into train and test in the ratio of 70:30

set.seed(100)
split_german_credit <- sample.split(german_credit$Default_status, SplitRatio = 0.7)

train <- german_credit[split_german_credit,]
test <- german_credit[!(split_german_credit),]

#Checkpoint 4:Modeling

# Initial model with all the variables 

model_1 <- glm(Default_status ~ ., data = train, family = "binomial")
summary(model_1)

# Using stepAIC function for stepwise selection of variables 

model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)

# Checking for a VIF value of > 3

vif(model_2)

# Remove Personal.status.and.sexA93 as it has high p-value

model_3 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Present.residence.since + foreign.worker + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                 Credit.historyA34 + PurposeA41 + PurposeA410 + PurposeA42 + 
                 PurposeA43 + PurposeA49 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74 +  
                 Other.debtors...guarantorsA102 + Other.installment.plansA143 + 
                 Housing.A152, family = "binomial", data = train)

summary(model_3)
vif(model_3)

# Remove Other.debtors...guarantorsA102 as it has higher p-value

model_4 <-  glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  Present.residence.since + foreign.worker + Status.of.existing.checking.accountA13 + 
                  Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                  Credit.historyA34 + PurposeA41 + PurposeA410 + PurposeA42 + 
                  PurposeA43 + PurposeA49 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                  Present.employment.since.A74 +  
                   Other.installment.plansA143 + 
                  Housing.A152, family = "binomial", data = train)

summary(model_4)
vif(model_4)

# Remove foreign.worker as it has higher p-value

model_5 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Present.residence.since + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                 Credit.historyA34 + PurposeA41 + PurposeA410 + PurposeA42 + 
                 PurposeA43 + PurposeA49 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74 +  
                 Other.installment.plansA143 + 
                 Housing.A152, family = "binomial", data = train)

summary(model_5)
vif(model_5)

# Remove PurposeA42  as it has higher p-value

model_6 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Present.residence.since + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                 Credit.historyA34 + PurposeA41 + PurposeA410 +  
                 PurposeA43 + PurposeA49 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74 +  
                 Other.installment.plansA143 + 
                 Housing.A152, family = "binomial", data = train)

summary(model_6)
vif(model_6)

# Remove PurposeA410 as it has higher p-value

model_7 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Present.residence.since + Status.of.existing.checking.accountA13 + 
                 Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                 Credit.historyA34 + PurposeA41 +   
                 PurposeA43 + PurposeA49 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                 Present.employment.since.A74 +  
                 Other.installment.plansA143 + 
                 Housing.A152, family = "binomial", data = train)


summary(model_7)
vif(model_7)

# Remove PurposeA49 as it has higher p-value

model_8 <-  glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  Present.residence.since + Status.of.existing.checking.accountA13 + 
                  Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                  Credit.historyA34 + PurposeA41 +   
                  PurposeA43 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                  Present.employment.since.A74 +  
                  Other.installment.plansA143 + 
                  Housing.A152, family = "binomial", data = train)
summary(model_8)
vif(model_8)

# Remove Status.of.existing.checking.accountA13 as it has higher p-value

model_9 <-  glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  Present.residence.since +  
                  Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                  Credit.historyA34 + PurposeA41 +   
                  PurposeA43 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                  Present.employment.since.A74 +  
                  Other.installment.plansA143 + 
                  Housing.A152, family = "binomial", data = train)

summary(model_9)
vif(model_9)

# Remove Other.installment.plansA143 as it has higher p-value

model_10 <-  glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                   Present.residence.since +  
                   Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                   Credit.historyA34 + PurposeA41 +   
                   PurposeA43 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                   Present.employment.since.A74 +  
                   Housing.A152, family = "binomial", data = train)

summary(model_10)
vif(model_10)


# Remove Present.residence.since as it has higher p-value

model_11 <-  glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                   Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                   Credit.historyA34 + PurposeA41 +   
                   PurposeA43 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                   Present.employment.since.A74 +  
                   Housing.A152, family = "binomial", data = train)

summary(model_11)
vif(model_11)

# Remove Installment.rate.in.percentage.of.disposable.income as it has higher p-value

model_12 <-  glm(formula = Default_status ~ Duration.in.month +  
                   Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                   Credit.historyA34 + PurposeA41 +   
                   PurposeA43 + Savings.account.bondsA64 + Savings.account.bondsA65 + 
                   Present.employment.since.A74 +  
                   Housing.A152, family = "binomial", data = train)

summary(model_12)
vif(model_12)

# Remove PurposeA43 as it has higher p-value

model_13 <-  glm(formula = Default_status ~ Duration.in.month +  
                   Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                   Credit.historyA34 + PurposeA41 +   
                    Savings.account.bondsA64 + Savings.account.bondsA65 + 
                   Present.employment.since.A74 +  
                   Housing.A152, family = "binomial", data = train)
summary(model_13)
vif(model_13)

# Remove Savings.account.bondsA64 as it has higher p-value

model_14 <-  glm(formula = Default_status ~ Duration.in.month +  
                   Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                   Credit.historyA34 + PurposeA41 +   
                    Savings.account.bondsA65 + 
                   Present.employment.since.A74 +  
                   Housing.A152, family = "binomial", data = train)
summary(model_14)
vif(model_14)

# Remove Savings.account.bondsA65 as it has higher p-value

model_15 <-  glm(formula = Default_status ~ Duration.in.month +  
                   Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                   Credit.historyA34 + PurposeA41 +   
                   Present.employment.since.A74 +  
                   Housing.A152, family = "binomial", data = train)

summary(model_15)
vif(model_15)

# Final model with 7 variables and with significance of *** and **

model_final <- glm(formula = Default_status ~ Duration.in.month +  
                     Status.of.existing.checking.accountA14 + Credit.historyA32 + 
                     Credit.historyA34 + PurposeA41 +   
                     Present.employment.since.A74 +  
                     Housing.A152, family = "binomial", data = train)

summary(model_final)
# As the coefficients from summary() output will be in the log odds, apply exponential 
# on them to get the coefficients in odds 

coefficients_odds <- coef(summary(model_final))
coefficients_odds[,"Estimate"] <- exp(coef(model_final))
coefficients_odds


#Checkpoint 5: Model Evaluation

# calculation of c-statistic value for train dataset

train$predicted_prob <- predict(model_final, type = "response")
rcorr.cens(train$predicted_prob,train$Default_status)

# calculation of c-statistic value for test dataset

test$predicted_prob <- predict(model_final,newdata = test,type = "response")
rcorr.cens(test$predicted_prob,test$Default_status)


# calculation of KS-statistic for train dataset


model_score <- prediction(train$predicted_prob,train$Default_status)
model_perf <- performance(model_score,"tpr","fpr")
ks_table <- attr(model_perf,"y.values")[[1]] - (attr(model_perf,"x.values")[[1]])

# KS statistic 

ks <- max(ks_table)

# KS statistic decile calculation

index <- which(ks_table == ks)
ks_decile <- index/(length(ks_table))

# plot the ROC curve for training data

plot(model_perf,col = "red")

# calculation of KS-statistic for test dataset 

model_score_test <- prediction(test$predicted_prob,test$Default_status)
model_perf_test <- performance(model_score_test,"tpr","fpr")
ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])

# KS statistic value for test dataset
ks_test <- max(ks_table_test)

# KS statistic decile calculation for test dataset
index_test <- which(ks_table_test == ks_test)
ks_test_decile <- index_test/(length(ks_table_test))

#Checkpoint 6: Threshold value

# Selecting threshold value for train dataset of 0.28 to improve Sensitivity and reduce false negatives 

confusionMatrix(as.numeric(train$predicted_prob > 0.28),train$Default_status,positive = '1')

# Applying threshold of 0.28 on test dataset 

confusionMatrix(as.numeric(test$predicted_prob > 0.28),test$Default_status,positive = '1')

