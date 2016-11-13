# Read the carMPG.csv dataset and store it in carmileage dataframe

setwd("F:/Data Analytics/Assignment_September 18")
carmileage <- read.csv("carMPG.csv")

# Checkpoint 1: Business Understanding and Data Understanding

# Check the structure of the dataset

str(carmileage)

# Checkpoint 2: Data Cleaning and Preparation

# Stage-1 : convert the data types of variables into appriopriate type

carmileage$Cylinders <- as.factor(carmileage$Cylinders)
carmileage$Horsepower <- as.numeric(levels(carmileage$Horsepower))[carmileage$Horsepower]
carmileage$Model_year <- as.factor(carmileage$Model_year)
carmileage$Origin <- as.factor(carmileage$Origin)

# Check the structure of dataset again so that all variables are converted to expected type

str(carmileage)

# Stage 2 : Data Cleaning

# Check whether there are any NA values introduced by cooresion

mpg_na <- sum(is.na(carmileage$MPG))
cylinders_na <- sum(is.na(carmileage$Cylinders))
displacement_na <- sum(is.na(carmileage$Displacement))
horsepower_na <- sum(is.na(carmileage$Horsepower))
weight_na <- sum(is.na(carmileage$Weight))
acceleration_na <- sum(is.na(carmileage$Acceleration))
model_year_na <- sum(is.na(carmileage$Model_year))
origin_na <- sum(is.na(carmileage$Origin))
car_name_na <- sum(is.na(carmileage$Car_Name))

# Only Horsepower variable has NAs in it, as there are very few NAs, remove corresponding records

carmileage <- subset(carmileage,is.na(Horsepower)!= TRUE)  

# Outlier treatment : Verify the perecentile distributions of numeric variables 

quantile(carmileage$MPG,seq(0,1,0.01))
summary(carmileage$MPG)

quantile(carmileage$Displacement,seq(0,1,0.01))
summary(carmileage$Displacement)

quantile(carmileage$Horsepower,seq(0,1,0.01))
summary(carmileage$Horsepower)

quantile(carmileage$Weight,seq(0,1,0.01))
summary(carmileage$Weight)

quantile(carmileage$Acceleration,seq(0,1,0.01))
summary(carmileage$Acceleration)

# As there are no sudden increases in the variables in the above percentile distributions 
# we need not treat the variables for outliers 

# Stage 3 : Variable Transformation

# Convert the Car_Name field from 305  levels to 31 levels

carmileage$Car_Name <- sub(" .*", "", as.character(carmileage$Car_Name))
carmileage$Car_Name <- as.factor(carmileage$Car_Name)

levels(carmileage$Car_Name)[match("chevroelt",levels(carmileage$Car_Name))] <- "chevrolet"
levels(carmileage$Car_Name)[match("chevy",levels(carmileage$Car_Name))] <- "chevrolet"
levels(carmileage$Car_Name)[match("maxda",levels(carmileage$Car_Name))] <- "mazda"
levels(carmileage$Car_Name)[match("toyouta",levels(carmileage$Car_Name))] <- "toyota"
levels(carmileage$Car_Name)[match("vokswagen",levels(carmileage$Car_Name))] <- "volkswagen"
levels(carmileage$Car_Name)[match("vw",levels(carmileage$Car_Name))] <- "volkswagen"

# Convert 13 levels of Model_year from 13 levels to 4 levels 

levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2003"] <- "2003-2006"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2004"] <- "2003-2006"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2005"] <- "2003-2006"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2006"] <- "2003-2006"

levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2007"] <- "2007-2009"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2008"] <- "2007-2009"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2009"] <- "2007-2009"

levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2010"] <- "2010-2012"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2011"] <- "2010-2012"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2012"] <- "2010-2012"

levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2013"] <- "2013-2015"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2014"] <- "2013-2015"
levels(carmileage$Model_year)[levels(carmileage$Model_year)=="2015"] <- "2013-2015"

# Convert Cylinders from 5 levels to 2 levels 

levels(carmileage$Cylinders)[levels(carmileage$Cylinders)=="3"] <- "<=4"
levels(carmileage$Cylinders)[levels(carmileage$Cylinders)=="4"] <- "<=4"

levels(carmileage$Cylinders)[levels(carmileage$Cylinders)=="5"] <- ">4"
levels(carmileage$Cylinders)[levels(carmileage$Cylinders)=="6"] <- ">4"
levels(carmileage$Cylinders)[levels(carmileage$Cylinders)=="8"] <- ">4"

# Create Cylinders to numeric 

levels(carmileage$Cylinders) <- c(0,1)
carmileage$Cylinders <- as.numeric((levels(carmileage$Cylinders)))[carmileage$Cylinders]

# Create dummy variables for Model_year

dummy_2 <- data.frame(model.matrix( ~Model_year, data = carmileage))
dummy_2<-dummy_2[,-1]

# Create dummy variables for Origin

dummy_3 <- data.frame(model.matrix( ~Origin, data = carmileage))
dummy_3 <-dummy_3[,-1]

# Create dummy variables for Car_Name

dummy_4 <- data.frame(model.matrix( ~Car_Name, data = carmileage))
dummy_4 <-dummy_4[,-1]

# Combine all dummy variables and the numeric variables 

carmileage_1 <- cbind(carmileage[ , c(1,2,3,4,5,6)],dummy_2,dummy_3,dummy_4)

carmileage <- carmileage_1

# Create train and test datasets from carmileage dataset

set.seed(100)
indices= sample(1:nrow(carmileage), 0.7*nrow(carmileage))

train <- carmileage[indices,]
test <- carmileage[-indices,]


# Create first model using all the variables in the dataset 

model_1 <-lm(MPG~.,data=train)
summary(model_1)

# Using stepAIC function to remove very insignificant variables 

library(MASS)
step <- stepAIC(model_1, direction="both")

model_2 <-lm(formula = MPG ~ Cylinders + Displacement + Horsepower + Weight + 
               Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
               Origin3 + Car_Nameaudi + Car_Namedatsun + Car_Namefiat + 
               Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
               Car_Namerenault + Car_Nametriumph + Car_Namevolkswagen, data = train)


summary(model_2)

library(car)

vif(model_2)

# Remove Displacement as it has higher VIF and p-values

model_3 <- lm(formula = MPG ~ Cylinders + Horsepower + Weight + 
                Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                Origin3 + Car_Nameaudi + Car_Namedatsun + Car_Namefiat + 
                Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                Car_Namerenault + Car_Nametriumph + Car_Namevolkswagen, data = train)

vif(model_3)
summary(model_3)

# Remove Horsepower  as it has higher VIF and p-values

model_4 <- lm(formula = MPG ~ Cylinders + Weight + 
                Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                Origin3 + Car_Nameaudi + Car_Namedatsun + Car_Namefiat + 
                Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                Car_Namerenault + Car_Nametriumph + Car_Namevolkswagen, data = train)
vif(model_4)
summary(model_4)

# Find correlation between Cylinders and Weight variables 

cor(train$Cylinders,train$Weight)


# As there is high correlation between Cylinders and Weight variable and Cylinders 
# is slightly less significant when compared to Weight, remove the Cylinders variable

model_5 <- lm(formula = MPG ~ Weight + 
                Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                Origin3 + Car_Nameaudi + Car_Namedatsun + Car_Namefiat + 
                Car_Nameoldsmobile + Car_Nameplymouth + Car_Namepontiac + 
                Car_Namerenault + Car_Nametriumph + Car_Namevolkswagen, data = train)

vif(model_5)
summary(model_5)


#Remove Car_Nameplymouth as it has high p-value

model_6 <- lm(formula = MPG ~ Weight + 
                Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                Origin3 + Car_Nameaudi + Car_Namedatsun + Car_Namefiat + 
                Car_Nameoldsmobile + Car_Namepontiac + 
                Car_Namerenault + Car_Nametriumph + Car_Namevolkswagen, data = train)
vif(model_6)
summary(model_6)

#Remove Car_Nameaudi as it has high p-value

model_7 <- lm(formula = MPG ~ Weight + 
                Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                Origin3 + Car_Namedatsun + Car_Namefiat + 
                Car_Nameoldsmobile + Car_Namepontiac + 
                Car_Namerenault + Car_Nametriumph + Car_Namevolkswagen, data = train)

vif(model_7)
summary(model_7)

# Remove Car_Nameoldsmobile as it has high p-value

model_8 <- lm(formula = MPG ~ Weight + 
                Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                Origin3 + Car_Namedatsun + Car_Namefiat  
                + Car_Namepontiac + 
                Car_Namerenault + Car_Nametriumph + Car_Namevolkswagen, data = train)
vif(model_8)
summary(model_8)

# Remove Car_Nametriumph as it has high p-value

model_9 <- lm(formula = MPG ~ Weight + 
                 Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                 Origin3 + Car_Namedatsun + Car_Namefiat  
               + Car_Namepontiac + 
                 Car_Namerenault + Car_Namevolkswagen, data = train)

vif(model_9)
summary(model_9)

# Remove Car_Namedatsun as it has high p-value

model_10 <- lm(formula = MPG ~ Weight + 
                 Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                 Origin3 + Car_Namefiat  
               + Car_Namepontiac + 
                 Car_Namerenault + Car_Namevolkswagen, data = train)

vif(model_10)
summary(model_10)

# Remove Car_Namerenault as it has high p-value

model_11 <- lm(formula = MPG ~ Weight + 
                 Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                 Origin3 + Car_Namefiat  
               + Car_Namepontiac + 
                 Car_Namevolkswagen, data = train)

vif(model_11)
summary(model_11)

# Remove Car_Namepontiac as it has high p-value

model_12 <- lm(formula = MPG ~ Weight + 
                 Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                 Origin3 + Car_Namefiat  
               +  
                 Car_Namevolkswagen, data = train)


vif(model_12)
summary(model_12)

# Remove Car_Namefiat as it has high p-value

model_13 <- lm(formula = MPG ~ Weight + 
                 Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + 
                 Origin3 + Car_Namevolkswagen, data = train)  
                          

vif(model_13)
summary(model_13)

# Remove Origin3 as it has high p-value

model_14 <- lm(formula = MPG ~ Weight + 
                 Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + Car_Namevolkswagen, data = train) 

vif(model_14)
summary(model_14)


# Chech point 4 :  Test model_14 on the test dataset 

predict_1 <- predict(model_14,test[,c(5,7,8,9,40)])
test$test_mpg <- predict_1
cor(test$MPG,test$test_mpg)
cor(test$MPG,test$test_mpg)^2

# Check point 5 : Model acceptance or rejection 

# The model has a R^2 value of 0.8236 and Adjusted R^2 value of 0.8203
# The R^2 value when tested on the test dataset is 0.8112995
# All the variables in the model have VIF < 2
# The model have only 5 variables which is as per business requirement 

# Final model

model_14 <- lm(formula = MPG ~ Weight + 
                 Model_year2007.2009 + Model_year2010.2012 + Model_year2013.2015 + Car_Namevolkswagen, data = train)

# plot the multiple regression model 

plot(model_14)
