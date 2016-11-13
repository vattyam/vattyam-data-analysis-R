# Read the loan file into a dataframe named loan

setwd("F:/Data Analytics/Assignment_August 28")
loan <- read.csv("loan.csv",header = T)

# Remove the unwanted rows from the loan dataframe

loan <- loan[-c(39787,39788,39789,42539,42540,42541,42542),]

# Find number of NAs in each of the driver variables

na_annual_inc <- sum(is.na(loan$annual_inc))

na_loan_amnt <- sum(is.na(loan$loan_amnt))

na_funded_amnt <- sum(is.na(loan$funded_amnt))

na_int_rate <- sum(is.na(loan$int_rate))

na_grade <- sum(is.na(loan$grade))

na_dti <- sum(is.na(loan$dti))

na_emp_length <- sum(is.na(loan$emp_length))

na_purpose <- sum(is.na(loan$purpose))

na_home_ownership <- sum(is.na(loan$home_ownership))

na_loan_status <- sum(is.na(loan$loan_status))

# Remove records with NAs as very few are present and only annual_inc has NAs

loan <- subset(loan,is.na(loan$annual_inc)!= T)

# Remove the Fully Paid loans from loan dataframe as they are not useful for our analysis

loan <- subset(loan,loan_status != "Fully Paid")
loan <- subset(loan,loan_status != "Does not meet the credit policy. Status:Fully Paid")

# Create a new column named loan_status_1  with three levels current_new, default_new and late such that
# rows with loan_status as "current" or "in grace period" are tagged as  "current_new"
# rows with loan_status as " default" or "charged off" are tagged as "default_new"
# rows with loan_status as " late 16- 30 days" "late 31-120 days" are tagged as "late"

loan["loan_status_1"] <- NA

for(i in  1:nrow(loan))
{
  if(loan$loan_status[i] == "Current" | loan$loan_status[i] == "In Grace Period")
  {
    loan$loan_status_1[i] <- "current_new"
  }

else if(loan$loan_status[i] == "Default" | loan$loan_status[i] == "Charged Off" | loan$loan_status[i] == "Does not meet the credit policy. Status:Charged Off")

{
   loan$loan_status_1[i] <- "default_new"
}

else
{

loan$loan_status_1[i] <- "late"
}
}

utils::View(loan)


# Convert int_rate column into numeric by removing % from the values

loan$int_rate <- as.numeric(sub("%","",as.character(loan$int_rate)))

# Create int_rate_grp such that int_rate < 10 is tagged "Low"; 
# int_rate (10-18) is tagged "Medium"; int_rate (>18) is tagged "High"

loan["int_rate_grp"] <- NA

for(i in  1:nrow(loan))
{
  if(loan$int_rate[i] < 10) 
  {
    loan$int_rate_grp[i] <- "Low"
  }
  
  else if(loan$int_rate[i] >= 10 & loan$int_rate[i] <= 18)
    
  {
    loan$int_rate_grp[i] <- "Medium"
  }
  
  else
  {
    
    loan$int_rate_grp[i] <- "High"
  }
}

utils::View(loan)
  
# Display different levels of emp_length variable

levels(loan$emp_length)

# There are some entries with value n/a in the emp_length, we shall remove them 
# for our analysis

loan <- subset(loan,emp_length != "n/a")


# Create emp_len_grp such that emp_length (0-4) is tagged as "Junior"; 
# emp_length (5-8) is tagged as "Mid-level"; emp_length (>8) is tagged as "Senior"

loan$emp_len_grp <- NA

for(i in  1:nrow(loan))
{
  if(loan$emp_length[i] == "< 1 year" | loan$emp_length[i] == "1 year" | loan$emp_length[i] == "2 years" | loan$emp_length[i] == "3 years" | loan$emp_length[i] == "4 years") 
  {
    loan$emp_len_grp[i] <- "Junior"
  }
  
  else if(loan$emp_length[i] == "5 years" | loan$emp_length[i] == "6 years" | loan$emp_length[i] == "7 years" | loan$emp_length[i] == "8 years") 
    
  {
    loan$emp_len_grp[i] <- "Mid-level"
  }
  
  else
  {
    
    loan$emp_len_grp[i] <- "Senior"
  }
}

utils::View(loan)

# Convert int_rate_grp, emp_len_grp and loan_status_1 into factor variables 

loan$int_rate_grp <- as.factor(loan$int_rate_grp)

loan$emp_len_grp <- as.factor(loan$emp_len_grp)

loan$loan_status_1 <- as.factor(loan$loan_status_1)


# Univariate Analysis :Summary Statistics for all driver variables 

summary_annual_inc <- summary(loan$annual_inc)

summary_loan_amnt <- summary(loan$loan_amnt)

summary_funded_amnt <- summary(loan$funded_amnt)

summary_int_rate_grp <- summary(loan$int_rate_grp)

summary_grade <- summary(loan$grade)

summary_dti <- summary(loan$dti)

summary_emp_len_grp <- summary(loan$emp_len_grp)

summary_purpose <- summary(loan$purpose)

summary_home_ownership <- summary(loan$home_ownership)

summary_loan_status <- summary(loan$loan_status)

summary_loan_status_1 <- summary(loan$loan_status_1)

# Univariate Analysis :Distribution plots for all driver variables 

library(ggplot2)

annual_inc_plot <- ggplot(loan,aes(loan$annual_inc)) + geom_histogram(col = "red",fill = "blue") + xlab("Annual Income")

loan_amnt_plot <- ggplot(loan,aes(loan$loan_amnt)) + geom_histogram(col = "red",fill = "blue") + xlab("Loan Amount")

funded_amnt_plot <- ggplot(loan,aes(loan$funded_amnt)) + geom_histogram(col = "red",fill = "blue") + xlab("Funded Amount")

int_rate_grp_plot <- ggplot(loan,aes(factor(loan$int_rate_grp))) + geom_bar(col = "red",fill = "blue") + xlab("Interest Rate Group")

grade_plot <- ggplot(loan,aes(factor(loan$grade))) + geom_bar(col = "red",fill = "blue") + xlab("Grade")

dti_plot <- ggplot(loan,aes(loan$dti)) + geom_histogram(col = "red",fill = "blue") + xlab("DTI")

emp_len_grp_plot <- ggplot(loan,aes(factor(loan$emp_len_grp))) + geom_bar(col = "red",fill = "blue") + xlab("Employment Duration")

purpose_plot <- ggplot(loan,aes(factor(loan$purpose))) + geom_bar(col = "red",fill = "blue") + xlab("Purpose")

home_ownership_plot <- ggplot(loan,aes(factor(loan$home_ownership))) + geom_bar(col = "red",fill = "blue") + xlab("Home Ownership")

loan_status_plot <- ggplot(loan,aes(factor(loan$loan_status))) + geom_bar(col = "red",fill = "blue") + xlab("Loan Status")

loan_status_1_plot <- ggplot(loan,aes(factor(loan$loan_status_1))) + geom_bar(col = "red",fill = "blue") + xlab("Loan Status")

# Univariate Analysis : Outlier treatment for annual_inc variable 

count_outliers_annual_inc <- length(boxplot.stats(loan$annual_inc)$out)

# As there are few outliers in annual_inc, we remove the corresponding records from our analysis

lower_hinge <- quantile(loan$annual_inc,probs = 0.25)
upper_hinge <- quantile(loan$annual_inc,probs = 0.75)
IQR <- (upper_hinge-lower_hinge) 
steps<- 1.5*IQR
loan <- subset(loan,(annual_inc >=lower_hinge-steps & annual_inc <= upper_hinge+steps))

# Univariate Analysis : Outlier treatment for loan_amnt variable 

count_outliers_loan_amnt <- length(boxplot.stats(loan$loan_amnt)$out)

# As there are few outliers in loan_amnt, we remove the corresponding records from our analysis

lower_hinge <- quantile(loan$loan_amnt,probs = 0.25)
upper_hinge <- quantile(loan$loan_amnt,probs = 0.75)
IQR <- (upper_hinge-lower_hinge) 
steps<- 1.5*IQR
loan <- subset(loan,(loan_amnt >=lower_hinge-steps & loan_amnt <= upper_hinge+steps))

# Univariate Analysis : Outlier treatment for funded_amnt variable 

count_outliers_funded_amnt <- length(boxplot.stats(loan$funded_amnt)$out)

# As there are few outliers in funded_amnt, we remove the corresponding records from our analysis

lower_hinge <- quantile(loan$funded_amnt,probs = 0.25)
upper_hinge <- quantile(loan$funded_amnt,probs = 0.75)
IQR <- (upper_hinge-lower_hinge) 
steps<- 1.5*IQR
loan <- subset(loan,(funded_amnt >=lower_hinge-steps & funded_amnt <= upper_hinge+steps))

# Univariate Analysis : Outlier treatment for dti variable

count_outliers_dti <- length(boxplot.stats(loan$dti)$out)
# The outlier count is turned out to be 0 and hence we need not treat any outliers 

# Multivariate Analysis: Finding correlations for all different pairs of continuous variables

cor_annual_inc_loan_amnt <- cor(loan$annual_inc,loan$loan_amnt)

cor_annual_inc_funded_amnt <- cor(loan$annual_inc,loan$funded_amnt)

cor_annual_inc_dti <- cor(loan$annual_inc,loan$dti)

cor_loan_amnt_funded_amnt <- cor(loan$loan_amnt,loan$funded_amnt)

car_loan_amnt_dti <- cor(loan$loan_amnt,loan$dti)

cor_funded_amnt_dti <- cor(loan$funded_amnt,loan$dti)

# Multivariate Analysis: plots to show how the continuous variables vary across the three levels of 
# loan_status_1; for e.g. how annual_inc is distributed across the levels default_new, late and current_new

loan_status_1_plot1 <- ggplot(loan,aes(loan$annual_inc,fill = factor(loan$loan_status_1),position = "stack")) + geom_histogram() + xlab("Annual Income") + guides(fill=guide_legend(title="loan_status_1")) 

loan_status_1_plot2 <- ggplot(loan,aes(loan$loan_amnt,fill = factor(loan$loan_status_1),position = "stack")) + geom_histogram() + xlab("Loan Amount")    + guides(fill=guide_legend(title="loan_status_1"))

loan_status_1_plot3 <- ggplot(loan,aes(loan$funded_amnt,fill = factor(loan$loan_status_1),position = "stack")) + geom_histogram() + xlab("Funded Amount") + guides(fill=guide_legend(title="loan_status_1"))

loan_status_1_plot4 <- ggplot(loan,aes(factor(loan$int_rate_grp),fill = factor(loan$loan_status_1),position = "stack")) + geom_bar() + xlab("Interest Rate") + guides(fill=guide_legend(title="loan_status_1"))

loan_status_1_plot5 <- ggplot(loan,aes(factor(loan$grade),fill = factor(loan$loan_status_1),position = "stack")) + geom_bar() + xlab("Grade") + guides(fill=guide_legend(title="loan_status_1"))   

loan_status_1_plot6 <- ggplot(loan,aes(loan$dti,fill = factor(loan$loan_status_1),position = "stack")) + geom_histogram() + xlab("DTI") + guides(fill=guide_legend(title="loan_status_1"))

loan_status_1_plot7 <- ggplot(loan,aes(factor(loan$emp_len_grp),fill = factor(loan$loan_status_1),position = "stack")) + geom_bar() + xlab("Employment Duration") + guides(fill=guide_legend(title="loan_status_1"))

loan_status_1_plot8 <- ggplot(loan,aes(factor(loan$purpose),fill = factor(loan$loan_status_1),position = "stack")) + geom_bar() + xlab("Purpose") + guides(fill=guide_legend(title="loan_status_1"))

loan_status_1_plot9 <- ggplot(loan,aes(factor(loan$home_ownership),fill = factor(loan$loan_status_1),position = "stack")) + geom_bar() + xlab("Home Ownership") + guides(fill=guide_legend(title="loan_status_1"))

loan_status_1_plot10 <- ggplot(loan,aes(factor(loan$loan_status),fill = factor(loan$loan_status_1),position = "stack")) + geom_bar() + xlab("Loan Status") + guides(fill=guide_legend(title="loan_status_1"))

# Multivariate Analysis: plots to show how the continuous variables vary across the three levels of 
# int_rate_grp; for e.g. how annual_inc is distributed across the levels Low, Medium and High

int_rate_grp_plot1 <- ggplot(loan,aes(loan$annual_inc,fill = factor(loan$int_rate_grp),position = "stack")) + geom_histogram() + xlab("Annual Income") + guides(fill=guide_legend(title="int_rate_grp")) 

int_rate_grp_plot2 <- ggplot(loan,aes(loan$loan_amnt,fill = factor(loan$int_rate_grp),position = "stack")) + geom_histogram() + xlab("Loan Amount")    + guides(fill=guide_legend(title="int_rate_grp"))

int_rate_grp_plot3 <- ggplot(loan,aes(loan$funded_amnt,fill = factor(loan$int_rate_grp),position = "stack")) + geom_histogram() + xlab("Funded Amount") + guides(fill=guide_legend(title="int_rate_grp"))

int_rate_grp_plot4 <- ggplot(loan,aes(factor(loan$grade),fill = factor(loan$int_rate_grp),position = "stack")) + geom_bar() + xlab("Grade") + guides(fill=guide_legend(title="int_rate_grp"))   

int_rate_grp_plot5 <- ggplot(loan,aes(loan$dti,fill = factor(loan$int_rate_grp),position = "stack")) + geom_histogram() + xlab("DTI") + guides(fill=guide_legend(title="int_rate_grp"))

int_rate_grp_plot6 <- ggplot(loan,aes(factor(loan$emp_len_grp),fill = factor(loan$int_rate_grp),position = "stack")) + geom_bar() + xlab("Employment Duration") + guides(fill=guide_legend(title="int_rate_grp"))

int_rate_grp_plot7 <- ggplot(loan,aes(factor(loan$purpose),fill = factor(loan$int_rate_grp),position = "stack")) + geom_bar() + xlab("Purpose") + guides(fill=guide_legend(title="int_rate_grp"))

int_rate_grp_plot8 <- ggplot(loan,aes(factor(loan$home_ownership),fill = factor(loan$int_rate_grp),position = "stack")) + geom_bar() + xlab("Home Ownership") + guides(fill=guide_legend(title="int_rate_grp"))

int_rate_grp_plot9 <- ggplot(loan,aes(factor(loan$loan_status),fill = factor(loan$int_rate_grp),position = "stack")) + geom_bar() + xlab("Loan Status") + guides(fill=guide_legend(title="int_rate_grp"))


# Test hypotheses (95 % conf. level) for two levels of loan_status_1 - default_new and current_new

loan_default_new <- subset(loan,loan_status_1 == "default_new")

loan_current_new <- subset(loan,loan_status_1 == "current_new")

# Confirm at 95% confidence level if the means of annual_inc at default_new level and current_new 
# level of loan_status_1 differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_loan_default_current_annual_inc <- var.test(loan_default_new$annual_inc,loan_current_new$annual_inc,conf.level = 0.95)

# From the p value of above variance test variances does not differ significantly

t_loan_default_current_annual_inc <- t.test(loan_default_new$annual_inc,loan_current_new$annual_inc,var.equal = TRUE,conf.level = 0.95)

# Confirm at 95% confidence level if the means of loan_amnt at default_new level and current_new 
# level of loan_status_1 differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_loan_default_current_loan_amnt <- var.test(loan_default_new$loan_amnt,loan_current_new$loan_amnt,conf.level = 0.95)

# From the p value of above variance test variances does not differ significantly

t_loan_default_current_loan_amnt <- t.test(loan_default_new$loan_amnt,loan_current_new$loan_amnt,var.equal = TRUE,conf.level = 0.95)

# Confirm at 95% confidence level if the means of funded_amnt at default_new level and current_new 
# level of loan_status_1 differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_loan_default_current_funded_amnt <- var.test(loan_default_new$funded_amnt,loan_current_new$funded_amnt,conf.level = 0.95)

# From the p value of above variance test variances does not differ significantly

t_loan_default_current_funded_amnt <- t.test(loan_default_new$funded_amnt,loan_current_new$funded_amnt,var.equal = TRUE,conf.level = 0.95)

#t_loan_default_current_int_rate <- t.test(loan_default_new$int_rate,loan_current_new$int_rate,conf.level = 0.95)

# Confirm at 95% confidence level if the means of dti at default_new level and current_new 
# level of loan_status_1 differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_loan_default_current_dti <- var.test(loan_default_new$dti,loan_current_new$dti,conf.level = 0.95)

# From the p value of above variance test variances does not differ significantly

t_loan_default_current_dti <- t.test(loan_default_new$dti,loan_current_new$dti,var.equal = TRUE,conf.level = 0.95)


# Test hypotheses (95 % conf. level) for two levels of int_rate_grp - high and low

int_rate_grp_high <- subset(loan,int_rate_grp == "High")

int_rate_grp_low <- subset(loan,int_rate_grp == "Low")

# Confirm at 95% confidence level if the means of annual_inc at high and low int_rate_grp 
# differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_int_high_low_annual_inc <- var.test(int_rate_grp_high$annual_inc,int_rate_grp_low$annual_inc,conf.level = 0.95)

# From the p value of above variance test variances does not differ significantly

t_int_high_low_annual_inc <- t.test(int_rate_grp_high$annual_inc,int_rate_grp_low$annual_inc,var.equal = TRUE,conf.level = 0.95)


# Confirm at 95% confidence level if the means of loan_amnt at high and low int_rate_grp
# differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_int_high_low_loan_amnt <- var.test(int_rate_grp_high$loan_amnt,int_rate_grp_low$loan_amnt,conf.level = 0.95)

# From the p value of above variance test variances differ significantly

t_int_high_low_loan_amnt <- t.test(int_rate_grp_high$loan_amnt,int_rate_grp_low$loan_amnt,var.equal = FALSE,conf.level = 0.95)

# Confirm at 95% confidence level if the means of funded_amnt at high and low int_rate_grp
# differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_int_high_low_funded_amnt <- var.test(int_rate_grp_high$funded_amnt,int_rate_grp_low$funded_amnt,conf.level = 0.95)

# From the p value of above variance test variances differ significantly

t_int_high_low_funded_amnt <- t.test(int_rate_grp_high$funded_amnt,int_rate_grp_low$funded_amnt,var.equal = FALSE,conf.level = 0.95)

#t_int_high_low_int_rate <- t.test(int_rate_grp_high$int_rate,int_rate_grp_low$int_rate,conf.level = 0.95)

# Confirm at 95% confidence level if the means of dti at high and low int_rate_grp
# differ significantly or not 

# variance test to determine var.equal = TRUE/FALSE

var_int_high_low_dti <- var.test(int_rate_grp_high$dti,int_rate_grp_low$dti,conf.level = 0.95)

# From the p value of above variance test variances differ significantly

t_int_high_low_dti <- t.test(int_rate_grp_high$dti,int_rate_grp_low$dti,var.equal = FALSE,conf.level = 0.95)

# Write loan dataframe onto a CSV file for analysis in Tableau

write.csv(loan, file = "F:\\Data Analytics\\Assignment_August 28\\loan_tableau.csv", row.names = FALSE)

