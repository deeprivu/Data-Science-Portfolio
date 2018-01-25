#### Importing the libraries and data into Workspace ####
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(ggthemes)
library(outliers)

loan <- read_csv("loan.csv")

#### Looking into the data to have a feel of the data ####
View(loan)
colnames(loan)[colSums(is.na(loan)) > 0] ##Display COlumns having NA values
nrow(loan) ##39717 rows
ncol(loan) ##111 columns
sum(duplicated(loan$id)) ##No Duplicates
sum(duplicated(loan$member_id)) ##No Duplicates
colnames(loan)[sapply(loan, function(x)
  length(which(x == "")))] ##Colnames having blank Values
str(loan)
summary(loan)

#### Resolving the Data Cleaning Issues in Data & removal of irrelevant columns####
ncol(loan) #111 Columns
loan <-
  loan[, colSums(is.na(loan)) != nrow(loan)] ##Removing attributes having only NA values
loan <-
  loan[, -which(colMeans(is.na(loan)) > 0.5)] ## Removing columns with more than 50% NA Values
ncol(loan) #54 Columns, 57 columns removed
loan <-
  Filter(function(x)
    (length(unique(x)) > 1), loan) #Removing attributes having a single unique value
ncol(loan) #48 Columns, 6 columns more removed
str(loan)

loan <-
  loan[, -which(names(loan) %in% c("id", "member_id", "url", "desc", "title", "emp_title"))]
#Removing the irrelevant columns for analysis with the following reasons:
# 1) id columns not required as we are not doing any analysis on that
# 2) url: No useful information can be extracted, other than redundant data
# 3) desc:Main objective of the loan has been documented under purpose table
# 4) title: redundant with purpose
# 5) emp_title: Irrelevant, as grade documents credit worthiness

## Looking into unique Values for selected COlumns(which can be converted into factors) to decide on how further analysis shall be done
sapply(loan[c(
  "term",
  "loan_status",
  "purpose",
  "zip_code",
  "emp_length",
  "grade",
  "sub_grade",
  "home_ownership",
  "verification_status",
  "addr_state",
  "delinq_2yrs",
  "pub_rec",
  "inq_last_6mths",
  "tax_liens",
  "pub_rec_bankruptcies",
  "collections_12_mths_ex_med",
  "chargeoff_within_12_mths"
)], function(x)
  unique(x))

#For emp_length, we have 12 distinct values, not imputing "n/a" with any value as the real value may lie anywhere in the range

## Double checking for few columns to be make it sure we have standarised data in respective columns
length(unique(toupper(loan$zip_code))) #823 distinct values, so, data is in standarised format
length(unique(toupper(loan$sub_grade))) #35 distinct values, so, data is in standarised format

## Further eliminating few columns based upon unique Values (Having 0 & NA:NA can be anything, so, irrelevant)
loan <-
  loan[, -which(
    names(loan) %in% c(
      "tax_liens",
      "collections_12_mths_ex_med",
      "chargeoff_within_12_mths"
    )
  )]

ncol(loan)

colnames(loan)[colSums(is.na(loan)) > 0] #4 columns have currently NA Values, we can impute values as per requirement
str(loan)

##### Data Manipulation and Creation of New USeful Attributes######
#Removing special characters from 2 columns
loan$revol_util <-
  as.numeric(gsub('%' , '', loan$revol_util , fixed = TRUE))
loan$int_rate <-
  as.numeric(gsub('%' , '' , loan$int_rate , fixed = TRUE))

#Deriving Issued Month & Year
loan <-
  mutate(
    loan,
    issue_year = paste('20', substr(issue_d, 5, 6), sep = ''),
    issue_month = substr(issue_d, 1, 3)
  )

### Seggregating the data into buckets for ease in analysis with the categorical Variable "loan_status". Choice of buckets has been explained below
### Will be useful for presentation to the Business, when range has been specified instead of continous Variable
unique(loan$purpose) #14 distinct values, so, choosing general terms for similar purpose
loan$purpose_bucket <-
  ifelse(
    loan$purpose %in% c("credit_card", "debt_consolidation"),
    "debt",
    ifelse(
      loan$purpose %in% c("house", "home_improvement", "moving"),
      "house",
      ifelse(
        loan$purpose %in% c("car", "vacation"),
        "leisure",
        ifelse(
          loan$purpose %in% c("wedding", "medical", "educational"),
          "essential",
          ifelse(
            loan$purpose %in% c("small_business", "major_purchase"),
            "business_purchase",
            "other"
          )
        )
      )
    )
  )

#bucket for home_ownership
loan$home_ownership_bucket <-
  ifelse(
    loan$home_ownership == "ANY" |
      loan$home_ownership == "NONE",
    "OTHER",
    loan$home_ownership
  )


#Function to create buckets
#Using qantile in order to treat Outliers accordingly
create_bucket <- function(column_name) {
  temp_quantile <-
    format(quantile(column_name, seq(0, 1, 0.1)), nsmall = 0)
  bucket <- cut(
    column_name,
    breaks = temp_quantile,
    labels = paste(temp_quantile[1:10], temp_quantile[2:11], sep = "-"),
    include.lowest = T
  )
  return(bucket)
}


#bucket for annual_income (Creating 10 bucket)
loan <-
  mutate(loan, annual_inc_bucket = create_bucket(round(loan$annual_inc /
                                                         1000, 0)))

#bucket for dti (Creating 10 bucket)
loan <- mutate(loan, dti_bucket = create_bucket(loan$dti))


#bucket for open_accounts (Creating 10 bucket)
loan <- mutate(loan, open_acc_bucket = create_bucket(loan$open_acc))

#bucket for total_accounts (Creating 10 bucket)
loan <-
  mutate(loan, total_acc_bucket = create_bucket(loan$total_acc))

#bucket for int_rate (Creating 10 bucket)
loan <- mutate(loan, int_rate_bucket = create_bucket(loan$int_rate))

#bucket for installment (Creating 10 bucket)
loan <-
  mutate(loan, installment_bucket = create_bucket(round(loan$installment /
                                                          10, 0)))

#bucket for loan_amount (Creating 10 bucket)
loan <-
  mutate(loan, loan_amnt_bucket = create_bucket(round(loan$loan_amnt / 100, 0)))

### Extrcating information for borrower's earliest reported credit line
d1 = as.Date("25-12-2017", format = "%d-%m-%Y")
loan <-
  mutate(loan, earliest_cr_line_year = format(as.Date(
    substr(loan$earliest_cr_line, 5, 9), format = "%y"
  ), "%Y"))
sort(unique(loan$earliest_cr_line_year)) # We see that substr(loan$earliest_cr_line, 5, 9) between 46 & 68 has been interpreted as
#2046-2068, instead of 1946 to 1968
loan$earliest_cr_line_year <-
  ifelse(
    as.numeric(substr(loan$earliest_cr_line_year, 3, 4)) >= 46 &
      as.numeric(substr(loan$earliest_cr_line_year, 3, 4)) <= 68,
    paste('19', substr(loan$earliest_cr_line_year, 3, 4), sep =
            ''),
    loan$earliest_cr_line_year
  )# Bit unconventional, but working

#bucket for earliest_cr_line_year (Creating 10 buckets)
loan <-
  mutate(loan, earliest_cr_line_year_bucket = create_bucket(as.numeric(loan$earliest_cr_line_year)))

#bucket for revol_util (Creating 10 buckets)
## It has nulls
summary(loan$revol_util)
## Imputing NA with median Value
loan$revol_util[is.na(loan$revol_util)] = median(loan$revol_util, na.rm =
                                                   TRUE)
loan <-
  mutate(loan, revol_util_bucket = create_bucket(loan$revol_util))

#Converting required COlumns to factors
loan[c(
  "term",
  "grade",
  "sub_grade",
  "home_ownership",
  "verification_status",
  "loan_status",
  "inq_last_6mths",
  "pub_rec",
  "zip_code",
  "delinq_2yrs",
  "addr_state",
  "emp_length",
  "purpose_bucket",
  "home_ownership_bucket",
  "earliest_cr_line_year_bucket",
  "revol_util_bucket"
)] <-
  lapply(loan[c(
    "term",
    "grade",
    "sub_grade",
    "home_ownership",
    "verification_status",
    "loan_status",
    "inq_last_6mths",
    "pub_rec",
    "zip_code",
    "delinq_2yrs",
    "addr_state",
    "emp_length",
    "purpose_bucket",
    "home_ownership_bucket",
    "earliest_cr_line_year_bucket",
    "revol_util_bucket"
  )], factor)
loan$issue_month <-
  factor(
    loan$issue_month,
    levels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  )
str(loan)
####### Data Analysis and Visualisation #########
## Statistics taken for numerical variables to understand the spread
summary(select_if(loan, is.numeric))

# Function to plot various graphs
frequency_distribution <- function(bucket_name) {
  temp_plot <- ggplot(loan, aes(x = bucket_name)) +
    geom_bar(
      aes(y = ..count..),
      width = 0.5,
      colour = "#1F3552",
      fill = "#4271AE"
    )  +
    theme_economist()
  return(temp_plot)
}

# Plotting Graphs in grids to understand the distribution of different buckets
theme_set(theme_cowplot(font_size = 12))
temp_plot1 <-
  frequency_distribution(loan$earliest_cr_line_year_bucket) + ggtitle("Frequency histogram of earliest_cr_line_year_bucket")
temp_plot2 <-
  frequency_distribution(loan$loan_amnt_bucket) + ggtitle("Frequency histogram of loan_amnt_bucket(in Hundred)")
temp_plot3 <-
  frequency_distribution(loan$total_acc_bucket) + ggtitle("Frequency histogram of total_acc_bucket")
temp_plot4 <-
  frequency_distribution(loan$open_acc_bucket) + ggtitle("Frequency histogram of open_acc_bucket")
plot_grid(temp_plot1, temp_plot2, temp_plot3, temp_plot4)

temp_plot5 <-
  frequency_distribution(loan$dti_bucket) + ggtitle("Frequency histogram of dti_bucket")
temp_plot6 <-
  frequency_distribution(loan$annual_inc_bucket) + ggtitle("Frequency histogram of annual_inc_bucket (in Thousand)")
temp_plot7 <-
  frequency_distribution(loan$home_ownership_bucket) + ggtitle("Frequency histogram of home_ownership_bucket")
temp_plot8 <-
  frequency_distribution(loan$purpose_bucket) + ggtitle("Frequency histogram of purpose_bucket")
plot_grid(temp_plot5, temp_plot6, temp_plot7, temp_plot8)

temp_plot9 <-
  frequency_distribution(loan$issue_year) + ggtitle("Frequency histogram of issue_year")
temp_plot10 <-
  frequency_distribution(loan$issue_month) + ggtitle("Frequency histogram of issue_month")
temp_plot11 <-
  frequency_distribution(loan$revol_util_bucket) + ggtitle("Frequency histogram of revol_util_bucket(in %)")
temp_plot12 <-
  frequency_distribution(loan$int_rate_bucket) + ggtitle("Frequency histogram of int_rate_bucket(in %)")
plot_grid(temp_plot9, temp_plot10, temp_plot11, temp_plot12)

temp_plot13 <-
  frequency_distribution(loan$installment_bucket) + ggtitle("Frequency histogram of installment_bucket(in Tens)")
temp_plot14 <-
  frequency_distribution(loan$delinq_2yrs) + ggtitle("Frequency histogram of delinq_2yrs")
plot_grid(temp_plot13, temp_plot14)

summary(loan$total_rec_late_fee)
summary(loan$collection_recovery_fee)

#Plotting graphs to understand the distribution of Continous Variables Vs Loan Status (Continous Vs Categorical)
#Function to plot the graphs (box-plots for distribution in every Loan Status)
create_box_plot <- function(Variable_entity) {
  temp_plot <-
    ggplot(loan, aes(x = loan_status, y = Variable_entity)) +
    geom_boxplot(fill = "#DF7401") +
    theme_solarized_2(light = FALSE) +
    scale_colour_solarized("blue")
  return(temp_plot)
}

# Finding the highly correlated columns
# Excluding pub_rec_bankruptcies as, uncertain how to Impute. NA can be anything and significantly changes analysis
loan_cor <-
  cor(select_if(loan[, colnames(loan) != 'pub_rec_bankruptcies'], is.numeric))
loan_cor <- as.data.frame(as.table(loan_cor))
loan_cor <-
  loan_cor[loan_cor$Freq > 0.95 &
             loan_cor$Freq != 1,] #Considering corelation>95%
loan_cor[!duplicated(loan_cor[, 3]),] ## We have 6 pairs of highly correlated Columns

## Plotting box_plots excluding one of highly correlated Column
box_plot1 <-
  create_box_plot(loan$loan_amnt) + ggtitle("loan_amnt Vs loan_status")
box_plot2 <-
  create_box_plot(loan$int_rate) + ggtitle("int_rate(in %) Vs loan_status")
box_plot3 <-
  create_box_plot(loan$revol_bal) + ggtitle("revol_bal Vs loan_status")
box_plot4 <-
  create_box_plot(loan$out_prncp) + ggtitle("out_prncp Vs loan_status")
plot_grid(box_plot1, box_plot2, box_plot3, box_plot4)

box_plot5 <-
  create_box_plot(loan$total_pymnt) + ggtitle("total_pymnt Vs loan_status")
box_plot6 <-
  create_box_plot(loan$total_rec_int) + ggtitle("total_rec_int Vs loan_status")
box_plot7 <-
  create_box_plot(scores(loan$total_rec_late_fee)) + ggtitle("total_rec_late_fee Vs loan_status")
box_plot8 <-
  create_box_plot(scores(loan$collection_recovery_fee)) + ggtitle("collection_recovery_fee Vs loan_status")
plot_grid(box_plot5, box_plot6, box_plot7, box_plot8)

## Brief summary of the continous variables for respective loan types
summary(loan[loan$loan_status == "Charged Off",]$loan_amnt)
summary(loan[loan$loan_status == "Fully Paid",]$loan_amnt)
summary(loan[loan$loan_status == "Current",]$loan_amnt)

summary(loan[loan$loan_status == "Charged Off",]$int_rate)
summary(loan[loan$loan_status == "Fully Paid",]$int_rate)
summary(loan[loan$loan_status == "Current",]$int_rate)

summary(loan[loan$loan_status == "Charged Off",]$revol_bal)
summary(loan[loan$loan_status == "Fully Paid",]$revol_bal)
summary(loan[loan$loan_status == "Current",]$revol_bal)

summary(loan[loan$loan_status == "Charged Off",]$out_prncp)
summary(loan[loan$loan_status == "Fully Paid",]$out_prncp)
summary(loan[loan$loan_status == "Current",]$out_prncp)

summary(loan[loan$loan_status == "Charged Off",]$total_pymnt)
summary(loan[loan$loan_status == "Fully Paid",]$total_pymnt)
summary(loan[loan$loan_status == "Current",]$total_pymnt)

summary(loan[loan$loan_status == "Charged Off",]$total_rec_int)
summary(loan[loan$loan_status == "Fully Paid",]$total_rec_int)
summary(loan[loan$loan_status == "Current",]$total_rec_int)

summary(loan[loan$loan_status == "Charged Off",]$total_rec_late_fee)
summary(loan[loan$loan_status == "Fully Paid",]$total_rec_late_fee)
summary(loan[loan$loan_status == "Current",]$total_rec_late_fee)

summary(loan[loan$loan_status == "Charged Off",]$collection_recovery_fee)
summary(loan[loan$loan_status == "Fully Paid",]$collection_recovery_fee)
summary(loan[loan$loan_status == "Current",]$collection_recovery_fee)

## Undergoing statisical significance test to test if observed differences are statistically significant
## Comparing FUlly Paid with that of Charged Off
## Null Hypothesis: The difference in means for the continuous variable concerned is zero, with 0.05 significance level

t.test(loan[loan$loan_status == "Charged Off",]$int_rate, loan[loan$loan_status ==
                                                                 "Fully Paid",]$int_rate, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$installment, loan[loan$loan_status ==
                                                                    "Fully Paid",]$installment, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$open_acc, loan[loan$loan_status ==
                                                                 "Fully Paid",]$open_acc, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$total_acc, loan[loan$loan_status ==
                                                                  "Fully Paid",]$total_acc, var.equal = F)

t.test(loan[loan$loan_status == "Charged Off",]$out_prncp, loan[loan$loan_status ==
                                                                  "Fully Paid",]$out_prncp, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$total_pymnt, loan[loan$loan_status ==
                                                                    "Fully Paid",]$total_pymnt, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$loan_amnt, loan[loan$loan_status ==
                                                                  "Fully Paid",]$loan_amnt, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$annual_inc, loan[loan$loan_status ==
                                                                   "Fully Paid",]$annual_inc, var.equal = F)

t.test(loan[loan$loan_status == "Charged Off",]$dti, loan[loan$loan_status ==
                                                            "Fully Paid",]$dti, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$revol_bal, loan[loan$loan_status ==
                                                                  "Fully Paid",]$revol_bal, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$revol_util, loan[loan$loan_status ==
                                                                   "Fully Paid",]$revol_util, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$total_rec_prncp, loan[loan$loan_status ==
                                                                        "Fully Paid",]$total_rec_prncp, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$total_rec_int, loan[loan$loan_status ==
                                                                      "Fully Paid",]$total_rec_int, var.equal = F)

t.test(loan[loan$loan_status == "Charged Off",]$last_pymnt_amnt, loan[loan$loan_status ==
                                                                        "Fully Paid",]$last_pymnt_amnt, var.equal = F)
t.test(loan[loan$loan_status == "Charged Off",]$total_rec_late_fee,
       loan[loan$loan_status == "Fully Paid",]$total_rec_late_fee,
       var.equal = F)

# Comparing the distribution of buckets Vs Loan Status (Categorical Vs Categorical)
# Summarising on Categorical Values, based on percentage

create_summary <- function(...) {
  loan %>% group_by(., ...) %>%
    summarise(
      .,
      Charged_Off_percent = round(sum(loan_status == "Charged Off") / n() * 100, 2),
      Current_percent = round(sum(loan_status == "Current") / n() * 100, 2),
      Fully_Paid_percent = round(sum(loan_status == "Fully Paid") / n() * 100, 2),
      count = n()
    )
}

create_graph <- function(data, col_name) {
  ggplot(data,
         aes(x = col_name)) + geom_bar(
           aes(y = Charged_Off_percent),
           width = 0.5,
           colour = "#1F3552",
           fill = "#4271AE",
           stat = "identity"
         ) + theme_economist()
}

term_summary <- create_summary(term)
summary_graph1 <-
  create_graph(term_summary, term_summary$term) + ggtitle("Term To Charged Off Percentage - Analysis")

grade_summary <- create_summary(grade)
summary_graph2 <-
  create_graph(grade_summary, grade_summary$grade) + ggtitle("Grade To Charged Off Percentage - Analysis")

create_summary(sub_grade)[order(-create_summary(sub_grade)$Charged_Off_percent),]
(
  loan[loan$emp_length != 'n/a',] %>% group_by(., emp_length) %>%
    summarise(
      .,
      Charged_Off_percent = round(sum(loan_status == "Charged Off") / n() * 100, 2),
      Current_percent = round(sum(loan_status == "Current") / n() * 100, 2),
      Fully_Paid_percent = round(sum(loan_status == "Fully Paid") / n() * 100, 2),
      count = n()
    )
) ### removing n/a

home_ownership_summary <- create_summary(home_ownership)
summary_graph3 <-
  create_graph(home_ownership_summary,
               home_ownership_summary$home_ownership) + ggtitle("home_ownership To Charged Off Percentage - Analysis")

verification_status_summary <- create_summary(verification_status)
summary_graph4 <-
  create_graph(verification_status_summary,
               verification_status_summary$verification_status) + ggtitle("verification_status To Charged Off Percentage - Analysis")

create_summary(addr_state)[order(-create_summary(addr_state)$Charged_Off_percent),]

inq_last_6mths_summary <- create_summary(inq_last_6mths)
summary_graph5 <-
  create_graph(inq_last_6mths_summary,
               inq_last_6mths_summary$inq_last_6mths) + ggtitle("Inquiry Last 6 Months To Charged Off Percentage - Analysis")

pub_rec_summary <- create_summary(pub_rec)
summary_graph6 <-
  create_graph(pub_rec_summary, pub_rec_summary$pub_rec) + ggtitle("pub_rec To Charged Off Percentage - Analysis")

int_rate_bucket_summary <- create_summary(int_rate_bucket)
summary_graph7 <-
  create_graph(int_rate_bucket_summary,
               int_rate_bucket_summary$int_rate_bucket) + ggtitle("Interest Rate To Charged Off Percentage - Analysis")

installment_bucket_summary <- create_summary(installment_bucket)
summary_graph8 <-
  create_graph(installment_bucket_summary,
               installment_bucket_summary$installment_bucket) + ggtitle("installment_bucket To Charged Off Percentage - Analysis")

purpose_bucket_summary <- create_summary(purpose_bucket)
summary_graph9 <-
  create_graph(purpose_bucket_summary,
               purpose_bucket_summary$purpose_bucket) + ggtitle("purpose_bucket To Charged Off Percentage - Analysis")

annual_inc_bucket_summary <- create_summary(annual_inc_bucket)
summary_graph10 <-
  create_graph(annual_inc_bucket_summary,
               annual_inc_bucket_summary$annual_inc_bucket) + ggtitle("Annual Income Bucket To Charged Off Percentage - Analysis")

delinq_2yrs_summary <- create_summary(delinq_2yrs)
summary_graph11 <-
  create_graph(delinq_2yrs_summary, delinq_2yrs_summary$delinq_2yrs) + ggtitle("Delinq 2 Years To Charged Off Percentage - Analysis")

dti_bucket_summary <- create_summary(dti_bucket)
summary_graph12 <-
  create_graph(dti_bucket_summary, dti_bucket_summary$dti_bucket) + ggtitle("DTI To Charged Off Percentage - Analysis")

open_acc_bucket_summary <- create_summary(open_acc_bucket)
summary_graph13 <-
  create_graph(open_acc_bucket_summary,
               open_acc_bucket_summary$open_acc_bucket) + ggtitle("open_acc_bucket To Charged Off Percentage - Analysis")

total_acc_bucket_summary <- create_summary(total_acc_bucket)
summary_graph14 <-
  create_graph(total_acc_bucket_summary,
               total_acc_bucket_summary$total_acc_bucket) + ggtitle("total_acc_bucket To Charged Off Percentage - Analysis")

loan_amnt_bucket_summary <- create_summary(loan_amnt_bucket)
summary_graph15 <-
  create_graph(loan_amnt_bucket_summary,
               loan_amnt_bucket_summary$loan_amnt_bucket) + ggtitle("Loan Amount To Charged Off Percentage - Analysis")

earliest_cr_line_year_bucket_summary <-
  create_summary(earliest_cr_line_year_bucket)
summary_graph16 <-
  create_graph(
    earliest_cr_line_year_bucket_summary,
    earliest_cr_line_year_bucket_summary$earliest_cr_line_year_bucket
  ) + ggtitle("earliest_cr_line_year_bucket To Charged Off Percentage - Analysis")

revol_util_bucket_summary <- create_summary(revol_util_bucket)
create_graph(revol_util_bucket_summary,
             revol_util_bucket_summary$revol_util_bucket) + ggtitle("revol_util_bucket To Charged Off Percentage - Analysis")

create_summary(zip_code)[order(-create_summary(zip_code)$Charged_Off_percent),]
loan[is.na(loan$pub_rec_bankruptcies) == F,] %>% group_by(., pub_rec_bankruptcies) %>%
  summarise(
    .,
    Charged_Off_percent = round(sum(loan_status == "Charged Off") / n() * 100, 2),
    Current_percent = round(sum(loan_status == "Current") / n() * 100, 2),
    Fully_Paid_percent = round(sum(loan_status == "Fully Paid") / n() * 100, 2),
    count = n()
  )
plot_grid(summary_graph1,
          summary_graph2,
          summary_graph3,
          summary_graph4)
plot_grid(summary_graph5,
          summary_graph6,
          summary_graph7,
          summary_graph8)
plot_grid(summary_graph9,
          summary_graph10,
          summary_graph11,
          summary_graph12)
plot_grid(summary_graph13,
          summary_graph14,
          summary_graph15,
          summary_graph16)

## Extra anaysis
## Analysis of Fully Paid who paid total_rec_late_fee
loan[loan$total_rec_late_fee > 0,] %>% group_by(., loan_status) %>%
  summarise(
    .,
    term_36_percent = round(sum(term == "36 months") / n() * 100, 2),
    term_60_percent = round(sum(term == "60 months") / n() * 100, 2),
    count = n()
  )

## Analysis of interest_rate with purpose type to analyse how interest_rate changes with purpose
ggplot(loan, aes(x = purpose_bucket, y = int_rate)) +
  geom_boxplot(fill = "#DF7401") +
  theme_solarized_2(light = FALSE) +
  scale_colour_solarized("blue") +
  ggtitle("int_rate Vs purpose_bucket")
