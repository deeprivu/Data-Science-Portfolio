# Business goal - to understand what factors XYZ should focus on, in order to curb attrition. 
# In other words, XYZ wants to know what changes they should make to their workplace, in order 
# to get most of their employees to stay. Also, XYZ wants to know which of these variables 
# is most important and needs to be addressed right away?

# Set Working Directory

#setwd(path)

# ============================================================================
# STEP 1 - Install && Load Libraries
# ============================================================================

library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(GGally)
library(caret)
library(cowplot)
library(caTools)
library(tidyr)
library(dplyr)

# ============================================================================
# Step 2 - Load Data
# ============================================================================

general_data <- read.csv("general_data.csv")
employee_survey_data <- read.csv("employee_survey_data.csv")
manager_survey_data <- read.csv("manager_survey_data.csv")
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)


# ============================================================================
# Step 3 - Data Understanding
# ============================================================================
# Age                       -> Age of the employee
# Attrition                 -> Whether the employee left in the previous year or not
# BusinessTravel            -> How frequently the employees travelled for business purposes in the last year
# Department                -> Department in company
# DistanceFromHome          -> Distance from home in kms
# Education                 -> Education Level
# EducationField            -> Field of education
# EmployeeCount             -> Employee count
# EmployeeNumber            -> Employee number/id
# EnvironmentSatisfaction   -> Work Environment Satisfaction Level
# Gender                    -> Gender of employee
# JobInvolvement            -> Job Involvement Level
# JobLevel                  -> Job level at company on a scale of 1 to 5
# JobRole                   -> Name of job role in company
# JobSatisfaction           -> Job Satisfaction Level
# MaritalStatus             -> Marital status of the employee
# MonthlyIncome             -> Monthly income in rupees per month
# NumCompaniesWorked        -> Total number of companies the employee has worked for
# Over18                    -> Whether the employee is above 18 years of age or not
# PercentSalaryHike         -> Percent salary hike for last year
# PerformanceRating         -> Performance rating for last year
# RelationshipSatisfaction  -> Relationship satisfaction level
# StandardHours             -> Standard hours of work for the employee
# StockOptionLevel          -> Stock option level of the employee
# TotalWorkingYears         -> Total number of years the employee has worked so far
# TrainingTimesLastYear     -> Number of times training was conducted for this employee last year
# WorkLifeBalance           -> Work life balance level
# YearsAtCompany            -> Total number of years spent at the company by the employee
# YearsSinceLastPromotion   -> Number of years since last promotion
# YearsWithCurrManager      -> Number of years under current manager	

#Let us examine the structure of the dataset
str(general_data)         # 4410 obs of 24 variables including the target variable
str(employee_survey_data) # 4410 obs of 4 variables
str(manager_survey_data)  # 4410 obs of 3 variables including the target variable
str(in_time)              # 4410 obs of 262 variables including the target variable
str(out_time)             # 4410 obs of 262 variables including the target variable

# ******************************************** #
# Collate the data together in one single file #
# ******************************************** #

# **********************************************************#
# ****Intime and outtime data frame processing *************#
# **********************************************************#
# Assiging 1st column name to "EmployeeID" in in_time and out_time dataset
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# Total Number of working days
# 1. Substract number of Saturday & Sunday. 2. Total number of Columns which have only NA.
# Count total number of Columns which have 
Date1 <- as.Date("2015-01-01")
Date2 <- as.Date("2015-12-31")    
sum(!weekdays(seq(Date1, Date2, "days")) %in% c("Saturday", "Sunday"))
# Result : 261 Days
sum(apply(in_time,2,function(x) {all(is.na(x))}))
# Resut : 12
# In 2015, there were 12 public holiday. Hence total working days will be 
# Result : 261 - 12 = 249 days are the total number of working days in year 2015

# Convert the in_time data into long format
in_time <- gather(in_time, date, loginTimestamp, X2015.01.01:X2015.12.31)
in_time$date <- gsub(pattern="X",x=in_time$date, replacement="")

# Convert the out_time data into long format
out_time <- gather(out_time, date, logoutTimestamp, X2015.01.01:X2015.12.31)
out_time$date <- gsub(pattern="X",x=out_time$date, replacement="")

# Merge in_time AND out_time to employee INTIME and OUTTIME in one data frame
inout_time <- merge(in_time, out_time, by=c("EmployeeID","date"))
inout_time$workingHours  <- round(difftime(inout_time$logoutTimestamp,inout_time$loginTimestamp,units="hours"),digits=2)

#Summarise inoutTime Data
inout_time <- inout_time %>% na.omit() %>% group_by(EmployeeID) %>%
  summarise(totalWorkingDays=n(),avgWorkingHours=round(mean(workingHours),digits=2))

#Calculate the number of Off days
inout_time$takenLeave <- (249 - inout_time$totalWorkingDays)

# *************************************************************#
# ****employee_survey_data data processing ********************#
# *************************************************************#
# Convert the below variables into factor and assign the level based on data dictionary
# EnvironmentSatisfaction, 
# JobSatisfaction, 
# WorkLifeBalance
# ************************************************************#
employee_survey_data$EnvironmentSatisfaction <- as.factor(employee_survey_data$EnvironmentSatisfaction)
levels(employee_survey_data$EnvironmentSatisfaction) <- c("Low","Medium","High","Very High")

# Convert Job Satisfaction to readable categories for better visualization
employee_survey_data$JobSatisfaction <- as.factor(employee_survey_data$JobSatisfaction)
levels(employee_survey_data$JobSatisfaction) <- c("Low","Medium","High","Very High")

# Convert Work-Life balance to readable categories for better visualization
employee_survey_data$WorkLifeBalance <- as.factor(employee_survey_data$WorkLifeBalance)
levels(employee_survey_data$WorkLifeBalance) <- c("Bad","Good","Better","Best")

# *************************************************************#
# ****general_data data processing ********************#
# *************************************************************#
# Convert the below variables into factor and assign the level based on data dictionary
# Education, 
# JobSatisfaction, 
# WorkLifeBalance
# ************************************************************#
# Making variables to Factor based on the data-dictionary
general_data$Education <- as.factor(general_data$Education)
levels(general_data$Education) <- c("Below College","College","Bachelor","Master", "Doctor")
general_data$StockOptionLevel <- as.factor(general_data$StockOptionLevel)
general_data$JobLevel <- as.factor(general_data$JobLevel)



# *************************************************************#
# ****manager_survey_data data processing ********************#
# *************************************************************#
# Convert the below variables into factor and assign the level based on data dictionary
# JobInvolvement, 
# PerformanceRating
# ************************************************************#

manager_survey_data$JobInvolvement <- as.factor(manager_survey_data$JobInvolvement)
levels(manager_survey_data$JobInvolvement) <- c("Low","Medium","High","Very High")


manager_survey_data$PerformanceRating <- as.factor(manager_survey_data$PerformanceRating)
levels(manager_survey_data$PerformanceRating) <- c("Low", "Good","Excellent","Outstanding")

# Checking the total number of unique records in all the dataset.
# Result : All the dataset are having unique EmployeeID.
length(unique(tolower(general_data$EmployeeID)))          # 4410, confirming EmployeeID is key 
length(unique(tolower(employee_survey_data$EmployeeID)))  # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID)))   # 4410, confirming EmployeeID is key
length(unique(tolower(in_time$EmployeeID)))               # 4410, confirming EmployeeID is key
length(unique(tolower(out_time$EmployeeID)))              # 4410, confirming EmployeeID is key

# Checking the difference in records against genera_data with all the datasets.
# Result : There is no difference in EmployeeID within the datasets.
setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID)  # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)   # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,inout_time$EmployeeID)            # Identical EmployeeID across these datasets

# Merge all the data from all the dataset
employee_master <- merge(general_data,employee_survey_data, by="EmployeeID", all = F)
employee_master <- merge(employee_master,manager_survey_data, by="EmployeeID", all = F)
employee_master <- merge(employee_master,inout_time, by="EmployeeID", all = F)


View(employee_master) #master file
#write.csv(employee_master, "employee_master.csv", row.names = F)
# ============================================================================
# Step 4 - EDA - Exploratory Data Analysis
# ============================================================================
employee_master$Attrition <- as.factor(employee_master$Attrition)
# Structure of the employee_master
str(employee_master) # 4410 obs. of  32 variables

# Barcharts for categorical features with stacked employee_master information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")
# Plot 1
# BusinessTravel, Department, Gender, JobRole, MaritalStatus

plot_grid(ggplot(employee_master, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position="fill")+bar_theme1, 
          ggplot(employee_master, aes(x=Department,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=Gender,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=JobRole,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          align = "h")   

# Plot 2
# EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating

plot_grid(ggplot(employee_master, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position="fill")+bar_theme1, 
          ggplot(employee_master, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          align = "h") 

# Plot 3
# Education, EducationField, StockOptionLevel, JobLevel
plot_grid(ggplot(employee_master, aes(x=Education,fill=Attrition))+ geom_bar(position="fill")+bar_theme1, 
          ggplot(employee_master, aes(x=EducationField,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          ggplot(employee_master, aes(x=JobLevel,fill=Attrition))+ geom_bar(position="fill")+bar_theme1,
          align = "h") 

# Analysis Result : - 
# Based on above 3 Plots on categorical variable, 
# we have below observations regarding Higher-attrition rate:-
# BusinessTravel = Travel_Frequently, 
# Department = Human Resources, 
# JobRole = Research Director, 
# MaritalStatus = Single
# EnvironmentSatisfaction = Low, 
# JobSatisfaction = Low,
# WorkLifeBalance = Bad, 
# JobInvolvement = Low, 
# Education = College ( but it is higher by very small margin), 
# EducationField = Human Resources, 
# StockOptionLevel = 2 ( but it is higher by very small margin), 
# JobLevel = 2 ( but it is higher by very small margin), 

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")
# Plot 4
# Age
plot_grid(ggplot(employee_master, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(employee_master, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Plot 5
# DistanceFromHome
plot_grid(ggplot(employee_master, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(employee_master, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Plot 6
# MonthlyIncome
plot_grid(ggplot(employee_master, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(employee_master, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 


# Boxplots of numeric variables relative to employee_master status

# Plot 7
# Age, DistanceFromHome, MonthlyIncome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears
plot_grid(ggplot(employee_master, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_master, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
         align = "v",nrow = 1)

# Plot 8
# TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager, NumCompaniesWorked

plot_grid(ggplot(employee_master, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_master, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Plot 9
# Box Plot on totalWorkingDays, avgWorkingHours, takenLeave
plot_grid(ggplot(employee_master, aes(x=Attrition,y=totalWorkingDays, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_master, aes(x=Attrition,y=avgWorkingHours, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_master, aes(x=Attrition,y=takenLeave, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Correlation among all the numeric variables.
# Creating numeric variables related vector
numericVariables <- c("Age","DistanceFromHome", "MonthlyIncome","NumCompaniesWorked", "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear", "YearsAtCompany", "YearsSinceLastPromotion","YearsWithCurrManager", "totalWorkingDays", "avgWorkingHours", "takenLeave")
ggpairs(employee_master[, numericVariables])
ggcorr(employee_master[, numericVariables], label = TRUE, label_alpha = TRUE)
# Higher correlations are among these variables from the above metrics: - 
# Age - TotalWorkingYear
# TotalWorkingYears - YearsAtCompany
# YearsAtCompany - YearsWithCurrManager


# ============================================================================
# Step 5 - DATA Preparation 
# ============================================================================
# 5.1 Removing useless variables
employee_master$EmployeeCount <- NULL
employee_master$Over18 <- NULL
employee_master$StandardHours <- NULL

# 5.2 Outlier treatments

# Outlier treatment and imputing missing value
# Boxplot showed outliers in MonthlyIncome but data seems to be valid
sapply(employee_master[,c("Age","DistanceFromHome","MonthlyIncome")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #Hence no treatment for the outlier

sapply(employee_master[,c("NumCompaniesWorked","PercentSalaryHike","PercentSalaryHike")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier

sapply(employee_master[,c("TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier

sapply(employee_master[,c("YearsSinceLastPromotion","YearsWithCurrManager","totalWorkingDays")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier

sapply(employee_master[,c("avgWorkingHours","takenLeave")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier

# 5.3 Missing value treatment
sapply(employee_master, function(x) sum(is.na(x))) 
# These below variables contains NA's
# NumCompaniesWorked = 19 
# TotalWorkingYears = 9
# EnvironmentSatisfaction = 25
# JobSatisfaction = 20
# WorkLifeBalance = 38
#
# Few Factor variables and continous variables are having NA's as we have seen earlier,
# Hence, we should remove otherwise it will create problem during dummy variable creation
employee_master <- na.omit(employee_master)
# 5.4 Feature standardisation
# Normalize all these continous variables with the help of SCALE

employee_master$Age                     <- scale(employee_master$Age) 
employee_master$DistanceFromHome        <- scale(employee_master$DistanceFromHome) 
employee_master$MonthlyIncome           <- scale(employee_master$MonthlyIncome) 
employee_master$NumCompaniesWorked      <- scale(employee_master$NumCompaniesWorked) 
employee_master$PercentSalaryHike       <- scale(employee_master$PercentSalaryHike) 
employee_master$TotalWorkingYears       <- scale(employee_master$TotalWorkingYears) 
employee_master$TrainingTimesLastYear   <- scale(employee_master$TrainingTimesLastYear) 
employee_master$YearsAtCompany          <- scale(employee_master$YearsAtCompany) 
employee_master$YearsSinceLastPromotion <- scale(employee_master$YearsSinceLastPromotion) 
employee_master$YearsWithCurrManager    <- scale(employee_master$YearsWithCurrManager) 
employee_master$totalWorkingDays        <- scale(employee_master$totalWorkingDays) 
employee_master$avgWorkingHours         <- scale(employee_master$avgWorkingHours) 
employee_master$takenLeave              <- scale(employee_master$takenLeave) 

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
employee_master$Attrition<- ifelse(employee_master$Attrition=="Yes",1,0)

# write.csv(employee_master, "employee_master_final.csv", row.names = F)

# Checking Attrition rate of prospect employee
AttritionRate <- sum(employee_master$Attrition)/nrow(employee_master)
AttritionRate # 0.1616279 , 16.16% attrition rate. 

# creating a dataframe of categorical features
str(employee_master)
employee_master_chr<- employee_master[,-c(1,2,3,6,13,14,15,17,18,19,20,21,27,28,29)]

# converting categorical attributes to factor
employee_master_fact<- data.frame(sapply(employee_master_chr, function(x) factor(x)))
str(employee_master_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_master_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_master_fact))[,-1]))

# Final dataset
employee_master_final<- cbind(employee_master[,c(1,2,3,6,13,14,15,17,18,19,20,21,27,28,29)],dummies) 
View(employee_master_final) #4300 obs. of  59 variables
# write.csv(employee_master, "employee_master_Model.csv", row.names = F)
# ****************************************************************************
# Logistic Regression - Model Building
# ****************************************************************************
# ============================================================================
# Step 6 - Create Model
# ============================================================================
########################################################################
# splitting the data between train and test
# separate training and testing data
# Check the final structure of the dataset before building the model
set.seed(100)
trainindices= sample(1:nrow(employee_master_final), 0.7*nrow(employee_master_final))
# Training Data
train = employee_master_final[trainindices,]
# Test Data
test = employee_master_final[-trainindices,]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2116.2....coeff..nullDev 2624.8...resDev 2000.2

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2) #AIC 2079.9....coeff..nullDev 2624.8...resDev 2009.9

#Removing multi-collinearity using VIF check
vif(model_2)

#Remove JobRole.xLaboratory.Technician from the model
#since it has large Pvalue, and it is not significant

model_3 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
  avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
  Department.xResearch...Development + Department.xSales + 
  Education.xCollege + EducationField.xOther + EducationField.xTechnical.Degree + 
  JobLevel.x2 + JobLevel.x5 +  
  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
  WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xMedium + 
  JobInvolvement.xVery.High,
  family = "binomial", data = train)

summary(model_3) #AIC 2080....coeff..nullDev 2624.8...resDev 2012.0
vif(model_3)

#Remove JobRole.xResearch.Scientist (0.256824) from the model
#since it has large P-value, and it is not significant

model_4 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xMedium + 
                 JobInvolvement.xVery.High,
               family = "binomial", data = train)

summary(model_4) #AIC 2079.3....coeff..nullDev 2624.8...resDev 2013.3
vif(model_4)

#Remove JobInvolvement.xMedium (0.155440) from the model
#since it has large P-value, and it is not significant

model_5 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + JobInvolvement.xLow + JobInvolvement.xVery.High,
               family = "binomial", data = train)

summary(model_5) #AIC 2079.3....coeff..nullDev 2624.8...resDev 2015.3
vif(model_5)

#Remove JobInvolvement.xLow (0.196371) from the model
#since it has large P-value, and it is not significant

model_6 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood + JobInvolvement.xVery.High,
               family = "binomial", data = train)

summary(model_6) #AIC 2078.9....coeff..nullDev 2624.8...resDev 2016.9
vif(model_6)

#Remove JobInvolvement.xVery.High (0.225650) from the model
#since it has large P-value, and it is not significant

model_7 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood ,
               family = "binomial", data = train)

summary(model_7) #AIC 2078.4....coeff..nullDev 2624.8...resDev 2018.4
vif(model_7)

#Remove EducationField.xTechnical.Degree (0.114799) from the model
#since it has large P-value, and it is not significant

model_8 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + EducationField.xOther +  
                 JobLevel.x2 + JobLevel.x5 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood ,
               family = "binomial", data = train)

summary(model_8) #AIC 2079....coeff..nullDev 2624.8...resDev 2021.0
vif(model_8)


#Remove JobRole.xSales.Executive (0.101695) from the model
#since it has large P-value, and it is not significant

model_9 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + EducationField.xOther +  
                 JobLevel.x2 + JobLevel.x5 +  
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood ,
               family = "binomial", data = train)

summary(model_9) #AIC 2079.7....coeff..nullDev 2624.8...resDev 2023.7
vif(model_9)

#Remove JobLevel.x2 (0.111748) from the model
#since it has large P-value, and it is not significant

model_10 <- glm(Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.xCollege + EducationField.xOther +  
                 JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                 EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                 JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                 WorkLifeBalance.xGood ,
               family = "binomial", data = train)

summary(model_10) #AIC 2080.2....coeff..nullDev 2624.8...resDev 2026.2
vif(model_10)

#Remove PercentSalaryHike  (0.09853) from the model
#since it has large P-value, and it is not significant

model_11 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  Education.xCollege + EducationField.xOther +  
                  JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_11) #AIC 2081.2....coeff..nullDev 2624.8...resDev 2029.2
vif(model_11)


#Remove StockOptionLevel.x1  (0.064402) from the model
#since it has large P-value, and it is less significant

model_12 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  Education.xCollege + EducationField.xOther +  
                  JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_12) #AIC 2082.6....coeff..nullDev 2624.8...resDev 2032.6
vif(model_12)

#Remove EducationField.xOther  (0.063605) from the model
#since it has large P-value, and it is less significant

model_13 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  Education.xCollege + JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_13) #AIC 2084.4....coeff..nullDev 2624.8...resDev 2036.4
vif(model_13)

#Remove Education.xCollege  (0.028920) from the model
#since it has large P-value, and it is less significant

model_14 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobLevel.x5 + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_14) #AIC 2087.1....coeff..nullDev 2624.8...resDev 2041.1
vif(model_14)

#Remove JobRole.xResearch.Director  (0.036264) from the model
#since it has large P-value, and it is less significant

model_15 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobLevel.x5 + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_15) #AIC 2089.2....coeff..nullDev 2624.8...resDev 2045.2
vif(model_15)

#Remove JobLevel.x5  (0.028400) from the model
#since it has large P-value, and it is less significant

model_16 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Department.xResearch...Development + Department.xSales + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_16) #AIC 2092.6....coeff..nullDev 2624.8...resDev 2050.6
vif(model_16)


# Remove BusinessTravel.xTravel_Rarely (0.009383)
# from the model because it is TWO STAR now, 
# since it has large P-value compare to others, and it is less significant

model_17 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Department.xSales + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_17) #AIC 2098....coeff..nullDev 2624.8...resDev 2058.0
vif(model_17)

# Remove JobRole.xManufacturing.Director (0.002876 ) **
# from the model because it is TWO STAR now, 
# since it has large P-value compare to others, and it is less significant

model_18 <- glm(Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + Department.xSales + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.xLow + 
                  EnvironmentSatisfaction.xVery.High + JobSatisfaction.xLow + 
                  JobSatisfaction.xVery.High + WorkLifeBalance.xBest + WorkLifeBalance.xBetter + 
                  WorkLifeBalance.xGood ,
                family = "binomial", data = train)

summary(model_18) #AIC 2105.8....coeff..nullDev 2624.8...resDev 2067.8
vif(model_18)

# ====================================================================
# Step 7 - Evaluate Model
# ====================================================================
######################################################################
# With 18 significant variables in the model
final_model<- model_18
#######################################################################
#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

# Let's see the summary 
summary(test_pred)
test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

# Matrix
table(test_pred_attrition,test_actual_attrition)

#Accuracy : 85.74%; Sensitivy : 27.27%; Specificity : 97.75%
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#######################################################################

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.
# Summary of test probability
summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"lightgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
# Let's choose a cutoff value of 0.1775 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1775, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.7565891
sens
# Sensitivity 
# 0.7545455 
spec
# Specificity 
# 0.7570093 
View(test)

######################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)

# on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

# 0.5115548
max(ks_table_test)

####################################################################
# Lift & Gain Chart 
# plotting the lift chart
# Loading dplyr package 
# require(dplyr)
# library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile


