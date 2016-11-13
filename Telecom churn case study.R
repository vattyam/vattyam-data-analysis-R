# Set Working Directory

setwd("F:/Data Analytics/Telecom churn case study")

# Install and Load the required packages

library(MASS)
library(car)
library(e1071)
library(ROCR)
library(caret)
library(Hmisc)
library(caTools)
library(gridExtra)
library(class)

# Load the given files.

customer <- read.csv("customer_data.csv")
internet <- read.csv("internet_data.csv")
churn_data <- read.csv("churn_data.csv")


# Collate the 3 files in a single file.

customer_internet <- merge(customer,internet,by = "customerID")
churn <- merge(customer_internet,churn_data,by = "customerID")

# Understand the structure of the collated file.

str(churn)
summary(churn)


# Make bar charts to find interesting relationships between variables.

gender_aes <- ggplot(churn,aes(x = churn$gender,fill = churn$Churn))
plot1 <- gender_aes + geom_bar(position = "dodge") + xlab("Gender") + ylab("Frequency") + ggtitle("Impact of Gender on Churn") + guides(fill=guide_legend(title="Churn"))

senior_citizen_aes <- ggplot(churn,aes(x = factor(churn$SeniorCitizen),fill = churn$Churn))
plot2 <- senior_citizen_aes + geom_bar(position = "dodge") + xlab("Senior Citizen") + ylab("Frequency") + ggtitle("Impact of Senior Citizenship on Churn") + guides(fill=guide_legend(title="Churn"))

partner_aes <- ggplot(churn,aes(x = churn$Partner,fill = churn$Churn))
plot3 <- partner_aes + geom_bar(position = "dodge") + xlab("Partner") + ylab("Frequency") + ggtitle("Impact of Partner on Churn") + guides(fill=guide_legend(title="Churn"))

dependents_aes <- ggplot(churn,aes(x = churn$Dependents,fill = churn$Churn))
plot4 <- dependents_aes + geom_bar(position = "dodge") + xlab("Dependents") + ylab("Frequency") + ggtitle("Impact of Dependents on Churn") + guides(fill=guide_legend(title="Churn"))

multiplelines_aes <- ggplot(churn,aes(x = churn$MultipleLines, fill = churn$Churn))
plot5 <- multiplelines_aes + geom_bar(position = "dodge") + xlab("Multiple Lines ") + ylab("Frequency") + ggtitle("Impact of Multiple Lines on Churn") + guides(fill=guide_legend(title="Churn")) 

internet_service_aes <- ggplot(churn,aes(x = churn$InternetService,fill = churn$Churn))
plot6 <- internet_service_aes + geom_bar(position = "dodge") + xlab("Internet Service") + ylab("Frequency") + ggtitle("Impact of Internet Service on Churn") + guides(fill=guide_legend(title="Churn"))

online_security_aes <- ggplot(churn,aes(x = churn$OnlineSecurity,fill = churn$Churn))
plot7 <- online_security_aes + geom_bar(position = "dodge") + xlab("Online Security") + ylab("Frequency") + ggtitle("Impact of Online Security on Churn") + guides(fill=guide_legend(title="Churn"))

online_backup_aes <- ggplot(churn,aes(x = churn$OnlineBackup,fill = churn$Churn))
plot8 <- online_backup_aes + geom_bar(position = "dodge") + xlab("Online Backup") + ylab("Frequency") + ggtitle("Impact of Online Backup on Churn") + guides(fill=guide_legend(title="Churn"))

device_protection_aes <- ggplot(churn,aes(x = churn$DeviceProtection,fill = churn$Churn))
plot9 <- device_protection_aes + geom_bar(position = "dodge") + xlab("Device Protection") + ylab("Frequency") + ggtitle("Impact of Device Protection on Churn") + guides(fill=guide_legend(title="Churn"))

tech_support_aes <- ggplot(churn,aes(x = churn$TechSupport,fill = churn$Churn))
plot10 <- tech_support_aes + geom_bar(position = "dodge") + xlab("Tech Support") + ylab("Frequency") + ggtitle("Impact of Tech Support on Churn") + guides(fill=guide_legend(title="Churn"))

streaming_tv_aes <- ggplot(churn,aes(churn$StreamingTV,fill = churn$Churn))
plot11 <- streaming_tv_aes + geom_bar(position = "dodge") + xlab("Streaming TV") + ylab("Frequency") + ggtitle("Impact of Streaming TV on Churn") + guides(fill=guide_legend(title="Churn"))

streaming_movies_aes <- ggplot(churn,aes(churn$StreamingMovies,fill = churn$Churn))
plot12 <- streaming_movies_aes + geom_bar(position = "dodge") + xlab("Streaming Movies") + ylab("Frequency") + ggtitle("Impact of Streaming Movies on Churn") + guides(fill=guide_legend(title="Churn"))

tenure_aes <- ggplot(churn,aes(churn$tenure,fill = churn$Churn))
plot13 <- tenure_aes + geom_histogram(position = "dodge") + xlab("Tenure") + ylab("Frequency") + ggtitle("Impact of Tenure on Churn") + guides(fill=guide_legend(title="Churn"))

phone_service_aes <- ggplot(churn,aes(churn$PhoneService,fill = churn$Churn))
plot14 <- phone_service_aes + geom_bar(position = "dodge") + xlab("Phone Service") + ylab("Frequency") + ggtitle("Impact of Phone Service on Churn") + guides(fill=guide_legend(title="Churn"))

contract_aes <- ggplot(churn,aes(churn$Contract,fill = churn$Churn))
plot15 <- contract_aes + geom_bar(position = "dodge") + xlab("Contract") + ylab("Frequency") + ggtitle("Impact of Contract on Churn") + guides(fill=guide_legend(title="Churn"))

paperless_billing_aes <- ggplot(churn,aes(churn$PaperlessBilling,fill = churn$Churn))
plot16 <- paperless_billing_aes + geom_bar(position = "dodge") + xlab("Paperless Billing") + ylab("Frequency") + ggtitle("Impact of Paperless Billing on Churn") + guides(fill=guide_legend(title="Churn"))

payment_method_aes <- ggplot(churn,aes(churn$PaymentMethod,fill = churn$Churn))
plot17 <- payment_method_aes + geom_bar(position = "dodge") + xlab("Payment Method") + ylab("Frequency") + ggtitle("Impact of Payment Method on Churn") + guides(fill=guide_legend(title="Churn"))

monthly_charges_aes <- ggplot(churn,aes(churn$MonthlyCharges,fill = churn$Churn))
plot18 <- monthly_charges_aes + geom_histogram(position = "dodge") + xlab("Monthly Charges") + ylab("Frequency") + ggtitle("Impact of Monthly Charges on Churn") + guides(fill=guide_legend(title="Churn"))

total_charges_aes <- ggplot(churn,aes(churn$TotalCharges,fill = churn$Churn))
plot19 <- total_charges_aes + geom_histogram(position = "dodge") + xlab("Total Charges") + ylab("Frequency") + ggtitle("Impact of Total Charges on Churn") + guides(fill=guide_legend(title="Churn"))

# Combine plots into grids rather than displaying each individual plot separately 

grid.arrange(plot1,plot2,plot3,plot4,ncol = 4)
grid.arrange(plot5,plot6,plot7,plot8,ncol = 4)
grid.arrange(plot9,plot10,plot11,plot12,ncol = 4)
grid.arrange(plot13,plot14,plot15,plot16,ncol = 4)
grid.arrange(plot17,plot18,plot19,ncol = 3)

# Make Box plots for numeric variables to look for outliers. 

boxplot(churn$tenure, col = "red")
boxplot(churn$MonthlyCharges, col = "blue")
boxplot(churn$TotalCharges, col = "brown")

# Perform De-Duplication if required

churn <- unique(churn)

#no duplicates are present in the dataset

# Identify the missing values in the collated dataset 

sapply(churn, function(x) sum(is.na(x)))

# Only TotalCharges column have missing values and very few are present, hence removing them

churn<- subset(churn,is.na(churn$TotalCharges)!= T)

# Using boxplot.stats() to identify the outliers in the numeric variables 

boxplot.stats(churn$tenure)
boxplot.stats(churn$MonthlyCharges)
boxplot.stats(churn$TotalCharges)

# No Outliers are present in any of the numeric variables, hence Outlier treatment is not required  

# Remove customerID column from the dataset, as it is just and identifier for customer and is not required for analysis

churn <- churn[,-1]

# K-NN Model:

# Bring the data in the correct format to implement K-NN model.

dummy_1 <- data.frame(model.matrix( ~MultipleLines, data = churn))
dummy_1 <- dummy_1[,-1]

dummy_2 <- data.frame((model.matrix( ~InternetService, data = churn)))
dummy_2 <- dummy_2[,-1]

dummy_3 <- data.frame((model.matrix( ~OnlineSecurity, data = churn)))
dummy_3 <- dummy_3[,-1]

dummy_4 <- data.frame((model.matrix( ~OnlineBackup, data = churn)))
dummy_4 <- dummy_4[,-1]

dummy_5 <- data.frame((model.matrix( ~DeviceProtection, data = churn)))
dummy_5 <- dummy_5[,-1]

dummy_6 <- data.frame((model.matrix( ~TechSupport, data = churn)))
dummy_6 <- dummy_6[,-1]

dummy_7 <- data.frame((model.matrix( ~StreamingTV, data = churn)))
dummy_7 <- dummy_7[,-1]

dummy_8 <- data.frame((model.matrix( ~StreamingMovies, data = churn)))
dummy_8 <- dummy_8[,-1]

dummy_9 <- data.frame((model.matrix( ~Contract, data = churn)))
dummy_9 <- dummy_9[,-1]

dummy_10 <- data.frame((model.matrix( ~PaymentMethod, data = churn)))
dummy_10 <- dummy_10[,-1]

levels(churn$gender) <- c(0,1)
churn$gender <- as.numeric((levels(churn$gender)))[churn$gender]

levels(churn$Partner) <- c(0,1)
churn$Partner <- as.numeric((levels(churn$Partner)))[churn$Partner]

levels(churn$Dependents) <- c(0,1)
churn$Dependents <- as.numeric((levels(churn$Dependents)))[churn$Dependents]

levels(churn$PhoneService) <- c(0,1)
churn$PhoneService <- as.numeric((levels(churn$PhoneService)))[churn$PhoneService]

levels(churn$PaperlessBilling) <- c(0,1)
churn$PaperlessBilling <- as.numeric((levels(churn$PaperlessBilling)))[churn$PaperlessBilling]

# Bind all the dummy variables and other variables into the dataset 

churn_1 <- cbind(churn[ , c(1,2,3,4,13,14,16,18,19,20)], dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6,dummy_7,dummy_8,dummy_9,dummy_10)

# Convert two levels in Churn variable from "No", "Yes" to 0,1 

levels(churn_1$Churn) <- c(0,1)

# Scaling the numeric variables 

churn_1$tenure <- scale(churn_1$tenure)
churn_1$MonthlyCharges <- scale(churn_1$MonthlyCharges)
churn_1$TotalCharges <- scale(churn_1$TotalCharges)

# Divide the dataset into training and testing datasets

set.seed(100)
split_churn <- sample.split(churn_1$Churn, SplitRatio = 0.7)

train <- churn_1[split_churn,]
test <- churn_1[!(split_churn),]

# Implement the K-NN model for optimal K.

# Write true labels of the training dataset into a vector c1 

cl <- train[, 10]

#Training and testing data without the true labels

data_train <- train[,-10]
data_test <- test[, -10]

#Using the train() command to find the best K.

model <- train(
  Churn~., 
  data=train,
  method='knn',
  tuneGrid=expand.grid(.k=1:100),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15))

#k-Nearest Neighbors 

#4922 samples
#30 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 15 times) 
#Summary of sample sizes: 4429, 4429, 4429, 4430, 4431, 4430, ... 
#Resampling results across tuning parameters:
  
#  k    Accuracy   Kappa    
#1  0.7213054  0.2992936
#2  0.7176348  0.2899566
#3  0.7458507  0.3474268
#4  0.7477863  0.3517120
#5  0.7584199  0.3719838
#6  0.7616577  0.3799419
#7  0.7702437  0.3991306
#8  0.7699050  0.3982176
#9  0.7750655  0.4124716
#10  0.7748499  0.4127878
#11  0.7780063  0.4224755
#12  0.7790767  0.4264071
#13  0.7822847  0.4340867
#14  0.7817444  0.4323558
#15  0.7820958  0.4329346
#16  0.7820691  0.4330001
#17  0.7851025  0.4399873
#18  0.7854143  0.4409297
#19  0.7870262  0.4448475
#20  0.7866196  0.4435956
#21  0.7884341  0.4479207
#22  0.7883661  0.4477209
#23  0.7901283  0.4524377
#24  0.7900062  0.4521301
#25  0.7911841  0.4546635
#26  0.7915220  0.4550789
#27  0.7919967  0.4559490
#28  0.7914823  0.4544706
#29  0.7920106  0.4555990
#30  0.7921464  0.4563602
#31  0.7926338  0.4572740
#32  0.7933915  0.4594102
#33  0.7936352  0.4592200
#34  0.7930127  0.4578537
#35  0.7937305  0.4597896
#36  0.7937571  0.4599921
#37  0.7937170  0.4593211
#38  0.7930661  0.4576649
#39  0.7933921  0.4577511
#40  0.7931200  0.4571367
#41  0.7932281  0.4574369
#42  0.7930250  0.4567639
#43  0.7931873  0.4570619
#44  0.7932556  0.4572167
#45  0.7928492  0.4557033
#46  0.7936349  0.4580328
#47  0.7927678  0.4560269
#48  0.7929443  0.4559556
#49  0.7926727  0.4553850
#50  0.7921722  0.4539469
#51  0.7928490  0.4554562
#52  0.7921581  0.4537363
#53  0.7923476  0.4539126
#54  0.7919009  0.4530077
#55  0.7919956  0.4536425
#56  0.7915760  0.4527952
#57  0.7915760  0.4526774
#58  0.7911970  0.4515760
#59  0.7917932  0.4531244
#60  0.7912379  0.4519200
#61  0.7911974  0.4514901
#62  0.7911566  0.4514807
#63  0.7913600  0.4515445
#64  0.7909131  0.4505561
#65  0.7906693  0.4497706
#66  0.7906561  0.4495679
#67  0.7911295  0.4512830
#68  0.7909265  0.4508813
#69  0.7909806  0.4509154
#70  0.7905877  0.4499295
#71  0.7911564  0.4516088
#72  0.7914403  0.4525155
#73  0.7916570  0.4529199
#74  0.7918735  0.4533517
#75  0.7920899  0.4539699
#76  0.7919815  0.4536386
#77  0.7927132  0.4550928
#78  0.7923467  0.4542567
#79  0.7928075  0.4554128
#80  0.7928484  0.4553847
#81  0.7929569  0.4554513
#82  0.7924426  0.4537588
#83  0.7926586  0.4543908
#84  0.7929437  0.4550377
#85  0.7923884  0.4534156
#86  0.7921992  0.4525319
#87  0.7922940  0.4526234
#88  0.7919283  0.4516047
#89  0.7920372  0.4515594
#90  0.7921181  0.4514696
#91  0.7927548  0.4531238
#92  0.7925787  0.4525209
#93  0.7924436  0.4521007
#94  0.7921726  0.4513054
#95  0.7922134  0.4513153
#96  0.7916855  0.4501915
#97  0.7917127  0.4503044
#98  0.7916035  0.4498887
#99  0.7911839  0.4485341
#100  0.7910886  0.4481377

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was k = 36. 

#KNn with 36NN
impknn36 <- knn(data_train,data_test, cl, k = 36,
                prob = TRUE)
table(impknn36,test[,10])
confusionMatrix(impknn36, test[,10], positive ="1" )


#calculating the values for ROC curve

attr(impknn36,"prob")<-ifelse(impknn36==1,attr(impknn36,"prob"),1-attr(impknn36,"prob"))

preds_knn <- attr(impknn36,"prob")
pred_knn <- prediction(preds_knn,test$Churn)
perf_knn <- performance(pred_knn,"tpr","fpr")

# plotting the ROC curve

plot(perf_knn,col="red")

# calculating AUC

auc_knn <- performance(pred_knn,"auc")

--------------------------------------------------------------------------------------------------
  
# Naive Bayes Model:

# Split the data into training and testing datasets 

set.seed(100)
split_churn <- sample.split(churn$Churn, SplitRatio = 0.7)

train <- churn[split_churn,]
test <- churn[!(split_churn),]


# Implement the Naive Bayes algorithm.

model <- naiveBayes(Churn ~. , data = train)

pred_naive1 <- predict(model, test)
table(pred_naive1, test$Churn)

# Accuracy, Sensitivity and Specificity for Naive Bayes 

confusionMatrix(pred_naive1,test$Churn,positive = "Yes")

# Calculating values for ROC curve 

pred_raw <- predict(model,test,type = "raw")
pred_prob_naive <- pred_raw[,2]
real_values <- ifelse(test$Churn == "Yes",1,0)

predict_churn_naive <- prediction(pred_prob_naive,real_values)
perf_naive_roc <- performance(predict_churn_naive,"tpr","fpr")

# ROC curve for Naive bayes 

plot(perf_naive_roc,col= "red")

# AUC for Naive Bayes 

auc_naive_bayes <- performance(predict_churn_naive,"auc")

-------------------------------------------------------------------------------------------------------

# Logistic Regression:

# Bring the data in the correct format to implement Logistic regression model.

set.seed(100)
split_churn <- sample.split(churn_1$Churn, SplitRatio = 0.7)

train <- churn_1[split_churn,]
test <- churn_1[!(split_churn),]

# Convert Churn variable in train and test datasets to numeric 

train$Churn <- as.numeric(levels(train$Churn ))[train$Churn]
test$Churn <- as.numeric(levels(test$Churn ))[test$Churn]

# Select the variables using VIF criterion. 

model_1 <- glm(Churn ~ ., data = train, family = "binomial")
summary(model_1)

model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
vif(model_2)

# Remove SeniorCitizen as it has high p-value

model_3 <- glm(formula = Churn ~ tenure + PhoneService + 
                 PaperlessBilling + MonthlyCharges + TotalCharges + MultipleLinesYes + 
                 InternetServiceFiber.optic + InternetServiceNo + OnlineBackupYes + 
                 DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes + 
                 ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check, 
               family = "binomial", data = train)

summary(model_3)
vif(model_3)

# Display correlation matrix and identify variables having strong correlations 

cor_matrix <- data.frame(train$tenure,train$PhoneService,train$PaperlessBilling,train$MonthlyCharges,train$TotalCharges,
                         train$MultipleLinesYes,train$InternetServiceFiber.optic,train$InternetServiceNo,train$OnlineBackupYes,
                         train$DeviceProtectionYes,train$StreamingTVYes,train$StreamingMoviesYes,train$ContractOne.year,train$ContractTwo.year)


cor(cor_matrix)

# Remove MonthlyCharges as it is highly correlated to InternetServiceFiber.optic and has high VIF

model_4 <- glm(formula = Churn ~ tenure + PhoneService + 
                 PaperlessBilling + TotalCharges + MultipleLinesYes + 
                 InternetServiceFiber.optic + InternetServiceNo + OnlineBackupYes + 
                 DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes + 
                 ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check, 
               family = "binomial", data = train)


summary(model_4)
vif(model_4)

# Remove TotalCharges as it has high VIF and high p-value

model_5 <- glm(formula = Churn ~ tenure + PhoneService + 
                 PaperlessBilling + MultipleLinesYes + 
                 InternetServiceFiber.optic + InternetServiceNo + OnlineBackupYes + 
                 DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes + 
                 ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check, 
               family = "binomial", data = train)


summary(model_5)
vif(model_5)

# Remove OnlineBackupYes as it has high p-value

model_6 <- glm(formula = Churn ~ tenure + PhoneService + 
                 PaperlessBilling + MultipleLinesYes + 
                 InternetServiceFiber.optic + InternetServiceNo +  
                 DeviceProtectionYes + StreamingTVYes + StreamingMoviesYes + 
                 ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check, 
               family = "binomial", data = train)


summary(model_6)
vif(model_6)

# Remove DeviceProtectionYes as it has high p-value

model_7 <- glm(formula = Churn ~ tenure + PhoneService + 
                 PaperlessBilling + MultipleLinesYes + 
                 InternetServiceFiber.optic + InternetServiceNo +  
                  StreamingTVYes + StreamingMoviesYes + 
                 ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check, 
               family = "binomial", data = train)


summary(model_7)
vif(model_7)

# Remove StreamingTVYes as it has high p-value

model_8 <- glm(formula = Churn ~ tenure + PhoneService + 
                 PaperlessBilling + MultipleLinesYes + 
                 InternetServiceFiber.optic + InternetServiceNo +  
                  StreamingMoviesYes + 
                 ContractOne.year + ContractTwo.year + PaymentMethodElectronic.check, 
               family = "binomial", data = train)


summary(model_8)
vif(model_8)

# Final logistic regression model

final_logistic_model <- model_8


# As the coefficients from summary() output will be in the log odds, apply exponential 
# on them to get the coefficients in odds 

coefficients_odds <- coef(summary(final_logistic_model))
coefficients_odds[,"Estimate"] <- exp(coef(final_logistic_model))
coefficients_odds

# Implement the Logistic regression algorithm and use stepwise selection to select final variables

# calculation of c-statistic value for train dataset

train$predicted_prob <- predict(final_logistic_model, type = "response")
rcorr.cens(train$predicted_prob,train$Churn)

# calculation of c-statistic value for test dataset

test$predicted_prob <- predict(final_logistic_model,newdata = test,type = "response")
rcorr.cens(test$predicted_prob,test$Churn)


# calculation of KS-statistic for train dataset


model_score <- prediction(train$predicted_prob,train$Churn)
model_perf <- performance(model_score,"tpr","fpr")
ks_table <- attr(model_perf,"y.values")[[1]] - (attr(model_perf,"x.values")[[1]])
auc_logistic <- performance(model_score,"auc")

# KS statistic for train dataset 

ks <- max(ks_table)

# KS statistic decile calculation for train dataset 

index <- which(ks_table == ks)
ks_decile <- index/(length(ks_table))

# plot the ROC curve for training data

plot(model_perf,col = "red")

# calculation of KS-statistic for test dataset 

model_score_test <- prediction(test$predicted_prob,test$Churn)
model_perf_test <- performance(model_score_test,"tpr","fpr")
ks_table_test <- attr(model_perf_test,"y.values")[[1]] - (attr(model_perf_test,"x.values")[[1]])
auc_logistic_test <- performance(model_score_test,"auc")

# Plot ROC curve for testing data

plot(model_perf_test,col = "red")

# KS statistic value for test dataset

ks_test <- max(ks_table_test)

# KS statistic decile calculation for test dataset

index_test <- which(ks_table_test == ks_test)
ks_test_decile <- index_test/(length(ks_table_test))

# confusion matrices for different threshold values for train dataset 

confusionMatrix(as.numeric(train$predicted_prob > 0.3),train$Churn,positive = '1')
confusionMatrix(as.numeric(train$predicted_prob > 0.5),train$Churn,positive = '1')
confusionMatrix(as.numeric(train$predicted_prob > 0.7),train$Churn,positive = '1')

# confusion matrices for different threshold values for test dataset 

confusionMatrix(as.numeric(test$predicted_prob > 0.3),test$Churn,positive = '1')
confusionMatrix(as.numeric(test$predicted_prob > 0.5),test$Churn,positive = '1')
confusionMatrix(as.numeric(test$predicted_prob > 0.7),test$Churn,positive = '1')
------------------------------------------------------------------------------------------------------------
# SVM:

# Divide the dataset into train  and test datasets 

set.seed(100)
split_churn <- sample.split(churn_1$Churn, SplitRatio = 0.7)

train <- churn_1[split_churn,]
test <- churn_1[!(split_churn),]


# calculation of optimal cost using tune() function 

tune.svm <- tune(svm, Churn ~., data = train,kernel = "linear",ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 10, 100)),scale = F)
summary(tune.svm)


#Parameter tuning of 'svm':
  
#  - sampling method: 10-fold cross validation 

#- best parameters:
#  cost
#0.01

#- best performance: 0.2035786 

#- Detailed performance results:
#  cost     error dispersion
#1 1e-03 0.2657531 0.02617317
#2 1e-02 0.2035786 0.02366534
#3 1e-01 0.2062204 0.02203180
#4 5e-01 0.2080489 0.02223766
#5 1e+00 0.2080485 0.02162620
#6 1e+01 0.2068302 0.02196127
#7 1e+02 0.2076428 0.02179331


best.mod <- tune.svm$best.model
best.mod

# From the tune function, cost 0.01 has highest performance, hence building the model with it

svm.mod <- svm(Churn ~., data = train, kernel = "linear",cost = 0.01,probability = T, scale = F)  
summary(svm.mod)

# Calculation of AUC and plotting ROC curve 

pred_svm <- predict(svm.mod,test,probability =  TRUE)
table(predicted = pred_svm, truth = test$Churn)
confusionMatrix(pred_svm, test$Churn,positive = "1")
pred_svm_data_frame <- data.frame(attr(pred_svm,"probabilities"))
predict_churn <- prediction(pred_svm_data_frame$X1,test$Churn)

# AUC for SVM

auc_svm <- performance(predict_churn,"auc") 

# ROC curve for SVM 

perf_roc_svm <- performance(predict_churn,"tpr","fpr")
plot(perf_roc_svm,col = "red")

