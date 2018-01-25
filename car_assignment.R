################## Linear Regression Solution ###################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building
#Model Evaluation
################################################################

### Business Understanding:
# A Chinese automobile company Geely Auto aspires to enter the US market by setting
# up their manufacturing unit there and producing cars locally to give competition to their US and European counterparts
#
# AIM: We are required to model the price of cars with the available independent variables. It will be used by the
# management to understand how exactly the prices vary with the independent variables
#

################################################################

### Loading required libraries #####

library(tidyr)
library(dplyr)
library(MASS)
library(car)

################################################################

### Data Understanding ###

car_price <- read.csv("CarPrice_Assignment.csv")
str(car_price)  ## 205 obs. of 26 variables
summary(car_price)  ### 12 categorical variables and 14 continous (1 needs
### to be changed)

sum(duplicated(car_price$car_ID)) # No Duplicates :)
sapply(car_price, function(x)
  length(which(x == ""))) # checking for blank "" values; there are none


################################################################

### Data Preparation & Exploratory Data Analysis####

## symboling need to be changed from continous to categorical
car_price$symboling <- as.factor(car_price$symboling)

# SInce only car maker is required, so, separating the CarName
car_price$CarName <- as.character(car_price$CarName)
car_price <-
  separate(car_price,
           CarName,
           into = c("Car_Company", "Model"),
           sep = " ")
car_price$Car_Company <- as.factor(car_price$Car_Company)
summary(car_price)

car_price <-
  car_price[c(-1, -4)] ## Removing the car_ID and Model, since, they shall not be required for the model

# Few entries in Car_Company seems to have incorrect entries, treating accordingly
unique(car_price$Car_Company)
car_price$Car_Company <- as.character(car_price$Car_Company)
car_price$Car_Company[which(car_price$Car_Company == "maxda")] <-
  "mazda"
car_price$Car_Company[which(car_price$Car_Company == "nissan")] <-
  "Nissan"
car_price$Car_Company[which(car_price$Car_Company == "porcshce")] <-
  "porsche"
car_price$Car_Company[which(car_price$Car_Company == "toyouta")] <-
  "toyota"
car_price$Car_Company[which(car_price$Car_Company == "vokswagen")] <-
  "volkswagen"
car_price$Car_Company[which(car_price$Car_Company == "vw")] <-
  "volkswagen"
car_price$Car_Company <- as.factor(car_price$Car_Company)

# Few entries in drivewheel seems to have incorrect entries, treating accordingly
car_price$drivewheel <- as.character(car_price$drivewheel)
car_price$drivewheel[which(car_price$drivewheel == "4wd")] <- "fwd"
car_price$drivewheel <- as.factor(car_price$drivewheel)

# Few entries in enginetype seems to have incorrect entries, treating accordingly
car_price$enginetype <- as.character(car_price$enginetype)
car_price$enginetype[which(car_price$enginetype == "dohcv")] <-
  "dohc"
car_price$enginetype <- as.factor(car_price$enginetype)

# Checking the structure
unique(car_price$fuelsystem)
str(car_price)

## Determining the outliers in the numerical variables and treating them accordingly
colnames(select_if(car_price, is.numeric)) # Numerical Attributes
quantile(car_price$wheelbase, seq(0, 1, 0.01)) # No outliers for wheelbase
quantile(car_price$carlength, seq(0, 1, 0.01)) # No outliers for carlength
quantile(car_price$carwidth, seq(0, 1, 0.01)) # No outliers for carwidth
quantile(car_price$carheight, seq(0, 1, 0.01)) # No outliers for carheight
quantile(car_price$curbweight, seq(0, 1, 0.01)) # No outliers for curbweight
quantile(car_price$enginesize, seq(0, 1, 0.01)) # There is a jump b/w 97% - 100%
quantile(car_price$boreratio, seq(0, 1, 0.01)) # No outliers for boreratio
quantile(car_price$stroke, seq(0, 1, 0.01)) # No outliers for stroke
quantile(car_price$compressionratio, seq(0, 1, 0.01)) # Jump b/w 90%-91%
quantile(car_price$horsepower, seq(0, 1, 0.01)) # Outlier b/w 99%-100%
quantile(car_price$peakrpm, seq(0, 1, 0.01)) # No outliers for peakrpm
quantile(car_price$citympg, seq(0, 1, 0.01)) # No outliers for citympg
quantile(car_price$highwaympg, seq(0, 1, 0.01)) # No outliers for highwaympg

car_price$enginesize[which(car_price$enginesize > 231.0)] <- 231.0
car_price$compressionratio[which(car_price$compressionratio > 10.9400)] <-
  10.9400
car_price$horsepower[which(car_price$horsepower > 288.00)] <- 288.00

## Binning the categorical variables to reduce the total no. of dummy attributes, wherever possible
str(select_if(car_price, is.factor)) # Categorical Attributes

levels(car_price$symboling)[1:2] <- "safe"
levels(car_price$symboling)[2:3] <- "moderate_risk"
levels(car_price$symboling)[3:4] <- "high_risk"
summary(car_price$symboling) ## Binning as per risky level, in regards to Safety

car_price$cylindernumber <- as.character(car_price$cylindernumber)
car_price$cylindernumber[which(car_price$cylindernumber %in% c("two", "three", "four"))] <-
  "less_than_5_cyls"
car_price$cylindernumber[which(car_price$cylindernumber %in% c("five", "six"))] <-
  "5_6_cyls"
car_price$cylindernumber[which(car_price$cylindernumber %in% c("eight", "twelve"))] <-
  "more_than_6_cyls"
car_price$cylindernumber <- as.factor(car_price$cylindernumber)
summary(car_price$cylindernumber) ## Binning as per cylinder numbers

car_price$fuelsystem <- as.character(car_price$fuelsystem)
car_price$fuelsystem[which(car_price$fuelsystem %in% c("1bbl", "2bbl", "4bbl"))] <-
  "bbl_based"
car_price$fuelsystem[which(car_price$fuelsystem %in% c("idi", "spdi"))] <-
  "di_based"
car_price$fuelsystem[which(car_price$fuelsystem %in% c("mfi", "mpfi", "spfi"))] <-
  "fi_based"
car_price$fuelsystem <- as.factor(car_price$fuelsystem)
summary(car_price$fuelsystem) ## Binning as per fuel system

car_price$Car_Company <- as.character(car_price$Car_Company)
car_price$Car_Company[which(car_price$Car_Company %in% c("alfa-romero"))] <-
  "Italian"
car_price$Car_Company[which(car_price$Car_Company %in% c("audi", "bmw", "porsche", "volkswagen"))] <-
  "German"
car_price$Car_Company[which(car_price$Car_Company %in% c("buick", "chevrolet", "dodge", "mercury"))] <-
  "American"
car_price$Car_Company[which(
  car_price$Car_Company %in% c(
    "honda",
    "isuzu",
    "mazda",
    "mitsubishi",
    "Nissan",
    "subaru",
    "toyota"
  )
)] <- "Japan"
car_price$Car_Company[which(car_price$Car_Company %in% c("jaguar", "plymouth"))] <-
  "UK"
car_price$Car_Company[which(car_price$Car_Company %in% c("renault", "peugeot"))] <-
  "France"
car_price$Car_Company[which(car_price$Car_Company %in% c("volvo", "saab"))] <-
  "Sweden"
car_price$Car_Company <- as.factor(car_price$Car_Company)
summary(car_price$Car_Company) ## Binning as per Car Maker`s Origin


# Creating the function for creation of dummy variables for categorical variables
dummy_var_df <- function(...) {
  dummy_1 <- data.frame(model.matrix(~ ..., data = car_price))
  dummy_1 <- dummy_1[, -1]
  return(dummy_1)
}

# Combine the dummy variables and the numeric columns of car_price dataset, in a new dataset called car_price_1
car_price_1 <-
  cbind(
    car_price[, !names(car_price) %in% colnames(select_if(car_price, is.factor))],
    dummy_var_df(car_price$symboling),
    dummy_var_df(car_price$Car_Company),
    dummy_var_df(car_price$fueltype),
    dummy_var_df(car_price$aspiration),
    dummy_var_df(car_price$doornumber),
    dummy_var_df(car_price$carbody),
    dummy_var_df(car_price$drivewheel),
    dummy_var_df(car_price$enginelocation),
    dummy_var_df(car_price$enginetype),
    dummy_var_df(car_price$cylindernumber),
    dummy_var_df(car_price$fuelsystem)
  )
## Note: In the new dataset created, i.e. car_price_1, few columns have been created with the nomenclature
# like dummy_var_df(car_price$<attribute_name>) for the categorical variables having 2 distinct values
# Few others have been created like ...<attribute_name> due to the same being returned from function
# During interpretation, such variables shall be treated accordingly

########### Model Building ################
# In the provided dataset, price is the dependent variable
# Checking if all the variables are numerical type
str(car_price_1)

# separate training and testing data
#set.seed(100)
#### NOTE: set.seed has not been used in the analysis, in order to sample different sets of data every time, so, that, it can be ensured that
# the final model is not an over-fit

trainindices = sample(1:nrow(car_price_1), 0.75 * nrow(car_price_1))
train = car_price_1[trainindices, ]
test = car_price_1[-trainindices, ]


# Build model 1 containing all variables
model_1 <- lm(price ~ ., data = train)
summary(model_1)

# Using the step AIC Function to remove the insignificant variables
step <- stepAIC(model_1, direction = "both")
step

# Creating model_2 using the new model displayed in the above step
model_2 <-
  lm(
    formula = price ~ wheelbase + carlength + curbweight + enginesize +
      stroke + compressionratio + peakrpm + citympg + ...moderate_risk +
      ...France + ...German + ...Italian + `dummy_var_df(car_price$fueltype)` +
      `dummy_var_df(car_price$aspiration)` + ...wagon + `dummy_var_df(car_price$enginelocation)` +
      ...ohc + ...ohcv + ...rotor + ...more_than_6_cyls + ...di_based,
    data = train
  )
summary(model_2)

# To determine the VIF for various attributes
vif(model_2)

# curbweight, enginesize, `dummy_var_df(car_price$fueltype)`, carlength attributes have high VIF.
# dummy_var_df(car_price$fueltype)` have less significance, hence the same can be removed

# MOdel removing dummy_var_df(car_price$fueltype)`
model_3 <-
  lm(
    formula = price ~ wheelbase + carlength + curbweight + enginesize +
      stroke + compressionratio + peakrpm + citympg + ...moderate_risk +
      ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...wagon + `dummy_var_df(car_price$enginelocation)` +
      ...ohc + ...ohcv + ...rotor + ...more_than_6_cyls + ...di_based,
    data = train
  )
summary(model_3)

# Checking VIF for model_3
vif(model_3)

# curbweight, enginesize, carlength have high VIF

## curbweight & enginesize have high VIF, calculating the correlation b/w them
cor(train$curbweight, train$enginesize)
## We have high correlation here, hence, removing the lesser significant variable

# Creating model_4, removing curbweight
model_4 <- lm(
  formula = price ~ wheelbase + enginesize + carlength +
    stroke + compressionratio + peakrpm + citympg + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...wagon + `dummy_var_df(car_price$enginelocation)` +
    ...ohc + ...ohcv + ...rotor + ...more_than_6_cyls + ...di_based,
  data = train
)
summary(model_4)

# Checking VIF for model_4
vif(model_4)

# Creating model_5, removing carlength
model_5 <- lm(
  formula = price ~ wheelbase + enginesize +
    stroke + compressionratio + peakrpm + citympg + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...wagon + `dummy_var_df(car_price$enginelocation)` +
    ...ohc + ...ohcv + ...rotor + ...more_than_6_cyls + ...di_based,
  data = train
)
summary(model_5)

# Checking VIF for model_5
vif(model_5)

# enginesize, citympg, wheelbase have high VIF

# ## citympg & enginesize have high VIF, calculating the correlation b/w them
cor(train$citympg, train$enginesize)
## We have high correlation here, hence, removing the lesser significant variable

# Creating model_6, removing citympg
model_6 <- lm(
  formula = price ~ wheelbase + enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...wagon + `dummy_var_df(car_price$enginelocation)` +
    ...ohc + ...ohcv + ...rotor + ...more_than_6_cyls + ...di_based,
  data = train
)
summary(model_6)

# Checking VIF for model_6
vif(model_6)

# ## wheelbase & enginesize have high VIF, calculating the correlation b/w them
cor(train$wheelbase, train$enginesize)
## We have high correlation here, hence, removing the lesser significant variable

# Creating model_7, removing wheelbase
model_7 <- lm(
  formula = price ~ enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...wagon + `dummy_var_df(car_price$enginelocation)` +
    ...ohc + ...ohcv + ...rotor + ...more_than_6_cyls + ...di_based,
  data = train
)
summary(model_7)

# Checking VIF for model_7
vif(model_7)

# Creating model_8, removing ...ohc as it is less significant
model_8 <- lm(
  formula = price ~ enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...ohcv + ...rotor + ...more_than_6_cyls + ...di_based,
  data = train
)
summary(model_8)

# Checking VIF for model_8
vif(model_8)

# Creating model_9, removing ...di_based
model_9 <- lm(
  formula = price ~ enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...ohcv + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_9)

# Checking VIF for model_9
vif(model_9)

# Creating model_10, removing ...ohcv
model_10 <- lm(
  formula = price ~ enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_10)
# Adjusted R- square decreased significantly, so, retaining ...ohcv

# Creating model_11, removing ...more_than_6_cyls
model_11 <- lm(
  formula = price ~ enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk +
    ...France + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...rotor,
  data = train
)
summary(model_11)
# Adjusted R- square decreased significantly, so, retaining ...more_than_6_cyls

# Since, VIF values for rest of the attributes is <2, so, now removing the insignificant attributes based on p-values
# Creating model_12, removing ...France
model_12 <- lm(
  formula = price ~ enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk + ...German + ...Italian + `dummy_var_df(car_price$aspiration)` + ...ohcv + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_12)

# Creating model_13, removing ...Italian
model_13 <- lm(
  formula = price ~ enginesize +
    stroke + compressionratio + peakrpm + ...moderate_risk + ...German + `dummy_var_df(car_price$aspiration)` + ...ohcv + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_13)

# Creating model_14, removing compressionratio
model_14 <- lm(
  formula = price ~ enginesize +
    stroke + peakrpm + ...moderate_risk + ...German + `dummy_var_df(car_price$aspiration)` + ...ohcv + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_14)

# Creating model_15, removing peakrpm
model_15 <- lm(
  formula = price ~ enginesize +
    stroke + ...moderate_risk + ...German + `dummy_var_df(car_price$aspiration)` + ...ohcv + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_15)

# Creating model_16, removing `dummy_var_df(car_price$aspiration)`
model_16 <- lm(
  formula = price ~ enginesize +
    stroke + ...moderate_risk + ...German + ...ohcv + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_16)

# Creating model_17, removing `...moderate_risk
model_17 <- lm(
  formula = price ~ enginesize +
    stroke + ...German + ...ohcv + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_17)

# Creating model_18, removing `...ohcv
model_18 <- lm(
  formula = price ~ enginesize +
    stroke + ...German + ...rotor + ...more_than_6_cyls,
  data = train
)
summary(model_18)

# Creating model_18, removing `stroke
model_19 <-
  lm(
    formula = price ~ enginesize + ...German + ...rotor + ...more_than_6_cyls,
    data = train
  )
summary(model_19)
## Final model is model_19 having the following 4 important parametrs which can be used for predicting the Prices
## enginesize, German_Companies built cars ("audi","bmw","porsche","volkswagen"), rotor-type engines, having more than 6 cylinders

########### Model Evaluation ##############

# Predicting the Prices based upon the model_19 and then calculating the r-square with the calculated and test result
predict_test <- predict(model_19, test[, -14])
(cor(test$price, predict_test)) ^ 2

# On multiple runs, using randome samples, R-square has been calculated with a variance of <=5% in comparison to Adjusted R- square calulatred in model_19,
# hence, can be considered as good fit.