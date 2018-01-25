library(dplyr)
library(stringr)
library(tidyr)

######################### Checkpoint 1 - Data Cleaning

companies <- read.delim("companies.txt", header = TRUE, sep = "\t")
rounds2 <- read.csv("rounds2.csv")

##Table1.1 - Understand the Data Set

rounds2$company_permalink <-
  str_to_lower(rounds2$company_permalink, locale = "en")
companies$permalink <-
  str_to_lower(companies$permalink, locale = "en")

#How many unique companies are present in rounds2?
unique_rounds2 <- length(unique(rounds2$company_permalink))
unique_rounds2

#How many unique companies are present in the companies file?
unique_companies <- length(unique(companies$permalink))
unique_companies

master_frame <-
  merge(rounds2,
        companies,
        by.x = "company_permalink", 
        by.y = "permalink", #In the companies data frame, which column can be used as the  unique key for each company?
        all.x = TRUE)
master_frame

#Are there any companies in the rounds2 file which are not  present in companies ? Answer - N
sum(is.na(master_frame$name))

#How many observations are present in master_frame ?
nrow(master_frame)

######################### Checkpoint 2: Funding Type Analysis

venture_type_group <- group_by(master_frame, funding_round_type)
venture_type_summary <-
  summarise(venture_type_group, mean(raised_amount_usd, na.rm = TRUE))
colnames(venture_type_summary) <-
  c("venture_type", "avg_funding_amount")

# Table-2.1 -  Average Values of Investments for Each of these Funding Types

#Average funding amount of venture type
venture_average_funding <-
  filter(venture_type_summary, venture_type == "venture")
venture_average_funding

#Average funding amount of angel type
angel_average_funding <-
  filter(venture_type_summary, venture_type == "angel")
angel_average_funding

#Average funding amount of seed type
seed_average_funding <-
  filter(venture_type_summary, venture_type == "seed")
seed_average_funding

#Average funding amount of private equity type
private_equity_average_funding <-
  filter(venture_type_summary, venture_type == "private_equity")
private_equity_average_funding

#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
venture_type_spark_fund_suitable <-
  filter(
    venture_type_summary,
    venture_type == "venture" |
      venture_type == "seed" |
      venture_type == "angel" | venture_type == "private_equity"
  )

venture_type_spark_fund_suitable <-
  filter(
    venture_type_spark_fund_suitable,
    avg_funding_amount >= 5000000 & avg_funding_amount <= 15000000
  )

venture_type_spark_fund_suitable

######################### Checkpoint 3: Country Analysis
#For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)
venture_data_frame <-
  filter(master_frame, funding_round_type == "venture")

country_group_by <- group_by(venture_data_frame, country_code)
country_investments <-
  summarise(country_group_by, sum(raised_amount_usd, na.rm = TRUE))
colnames(country_investments) <-
  c("country_code", "total_investment")

country_investments <-
  filter(country_investments, country_code != "")
country_investments <-
  arrange(country_investments, desc(total_investment))

top9 <- head(country_investments, 9)
top9

##Table 3.1 - Analysing the Top 3 English-Speaking Countries - USA, GBR, IND

######################### Checkpoint 4: Sector Analysis 1
#1. Extract the primary sector of each category list from the category_list column
master_frame <-
  separate(
    master_frame,
    category_list,
    c("primary_sector"),
    sep = "\\|",
    remove = FALSE
  )


#2. Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors (Note that 'Others' is also considered one of the main sectors)
##converting mapping.csv file from wide to long format
sector_mapping_file <- read.csv("mapping.csv", check.names = FALSE)

sector_mapping_file <-
  gather(sector_mapping_file, "sector_category", "value", 2:10)
sector_mapping_file <-
  sector_mapping_file[!(sector_mapping_file$value == 0), ]
sector_mapping_file <- sector_mapping_file[, 1:2]

##Merging the long format mapping file with master data frame
master_frame <-
  merge(
    master_frame,
    sector_mapping_file,
    by.x = "primary_sector",
    by.y = "category_list",
    all.x = TRUE
  )
colnames(master_frame)[colnames(master_frame) == 'sector_category'] <-
  'main_sector'

######################### Checkpoint 5: Sector Analysis 2
#Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of 
#funding type FT falling within the 5-15 million USD range.
D1 <-
  filter(
    master_frame,
    funding_round_type == "venture" &
      country_code == "USA" &
      raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000
  )
D1

D2 <-
  filter(
    master_frame,
    funding_round_type == "venture" &
      country_code == "GBR" &
      raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000
  )
D2

D3 <-
  filter(
    master_frame,
    funding_round_type == "venture" &
      country_code == "IND" &
      raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000
  )
D3

D1_MainSector_Group <- group_by(D1, main_sector)
D2_MainSector_Group <- group_by(D2, main_sector)
D3_MainSector_Group <- group_by(D3, main_sector)

D1_Investment_Count <- summarise(D1_MainSector_Group, n())
colnames(D1_Investment_Count) <-
  c("main_sector", "Total_Investment_Count")
D1_Investment_Count

D2_Investment_Count <- summarise(D2_MainSector_Group, n())
colnames(D2_Investment_Count) <-
  c("main_sector", "Total_Investment_Count")
D2_Investment_Count

D3_Investment_Count <- summarise(D3_MainSector_Group, n())
colnames(D3_Investment_Count) <-
  c("main_sector", "Total_Investment_Count")
D3_Investment_Count

D1_Investment_Sum <-
  summarise(D1_MainSector_Group, sum(raised_amount_usd))
colnames(D1_Investment_Sum) <-
  c("main_sector", "Total_Investment_Sum")
D1_Investment_Sum

D2_Investment_Sum <-
  summarise(D2_MainSector_Group, sum(raised_amount_usd))
colnames(D2_Investment_Sum) <-
  c("main_sector", "Total_Investment_Sum")
D2_Investment_Sum

D3_Investment_Sum <-
  summarise(D3_MainSector_Group, sum(raised_amount_usd))
colnames(D3_Investment_Sum) <-
  c("main_sector", "Total_Investment_Sum")
D3_Investment_Sum

# The three data frames should contain: 
# All the columns of the master_frame along with the primary sector and the main sector
# The total number (or count) of investments for each main sector in a separate column
# The total amount invested in each main sector in a separate column

D1 <- merge(D1, D1_Investment_Count)
D1 <- merge(D1, D1_Investment_Sum)
D2 <- merge(D2, D2_Investment_Count)
D2 <- merge(D2, D2_Investment_Sum)
D3 <- merge(D3, D3_Investment_Count)
D3 <- merge(D3, D3_Investment_Sum)

## Table - 5.1 - Sector-wise Investment Analysis
#Total number of Investments (count) - C1, C2, C3
C1_TotalInvestmentCount <-
  sum(D1_Investment_Count$Total_Investment_Count)
C1_TotalInvestmentCount

C2_TotalInvestmentCount <-
  sum(D2_Investment_Count$Total_Investment_Count)
C2_TotalInvestmentCount

C3_TotalInvestmentCount <-
  sum(D3_Investment_Count$Total_Investment_Count)
C3_TotalInvestmentCount

#Total amount of investment (USD) - C1, C2, C3
C1_TotalInvestmentSum <- sum(D1_Investment_Sum$Total_Investment_Sum)
C1_TotalInvestmentSum

C2_TotalInvestmentSum <- sum(D2_Investment_Sum$Total_Investment_Sum)
C2_TotalInvestmentSum

C3_TotalInvestmentSum <- sum(D3_Investment_Sum$Total_Investment_Sum)
C3_TotalInvestmentSum

#Top Sector name (no. of investment-wise) - C1, C2, C3
#Second Sector name (no. of investment-wise) - C1, C2, C3
#Third Sector name (no. of investment-wise) - C1, C2, C3
#Number of investments in top sector (3) - C1, C2, C3
#Number of investments in second sector (4) - C1, C2, C3
#Number of investments in third sector (5) - C1, C2, C3
C1_sector_group <- group_by(D1, country_code, main_sector)
C2_sector_group <- group_by(D2, country_code, main_sector)
C3_sector_group <- group_by(D3, country_code, main_sector)

C1_sector_summary <- summarise(C1_sector_group,n())
C2_sector_summary <- summarise(C2_sector_group,n())
C3_sector_summary <- summarise(C3_sector_group,n())

colnames(C1_sector_summary)[3] <- "no_of_investments"
colnames(C2_sector_summary)[3] <- "no_of_investments"
colnames(C3_sector_summary)[3] <- "no_of_investments"

C1_top3_sectors <- top_n(C1_sector_summary, 3, no_of_investments)
C2_top3_sectors <- top_n(C2_sector_summary, 3, no_of_investments)
C3_top3_sectors <- top_n(C3_sector_summary, 3, no_of_investments)

C1_top3_sectors <- arrange(C1_top3_sectors, desc(no_of_investments))
C2_top3_sectors <- arrange(C2_top3_sectors, desc(no_of_investments))
C3_top3_sectors <- arrange(C3_top3_sectors, desc(no_of_investments))


#For point 3 (top sector count-wise), which company received the highest investment? - C1
C1_name_group <- group_by(D1, country_code, main_sector, name)
C1_name_group <-
  subset(
    C1_name_group,
    main_sector == "Others" # Top 1 sector for country C1 is 'Others'
  )

C1_name_summary <- summarise(C1_name_group, n())
colnames(C1_name_summary)[4] <- "no_of_investments"

highinvest_C1_top_sector <-
  top_n(C1_name_summary, 1, no_of_investments)
highinvest_C1_top_sector <-
  arrange(highinvest_C1_top_sector,
          desc(no_of_investments, main_sector))
highinvest_C1_top_sector


#For point 3 (top sector count-wise), which company received the highest investment? - C2
C2_name_group <- group_by(D2, country_code, main_sector, name)
C2_name_group <-
  subset(
    C2_name_group,
    main_sector == "Others" # Top 1 sector for country C2 is 'Others'
  )

C2_name_summary <- summarise(C2_name_group, n())
colnames(C2_name_summary)[4] <- "no_of_investments"

highinvest_C2_top_sector <-
  top_n(C2_name_summary, 1, no_of_investments)
highinvest_C2_top_sector <-
  arrange(highinvest_C2_top_sector,
          desc(no_of_investments, main_sector))
highinvest_C2_top_sector

#For point 3 (top sector count-wise), which company received the highest investment? - C3
C3_name_group <- group_by(D3, country_code, main_sector, name)
C3_name_group <-
  subset(
    C3_name_group,
    main_sector == "Others" # Top 1 sector for country C3s is 'Others'
  )

C3_name_summary <- summarise(C3_name_group, n())
colnames(C3_name_summary)[4] <- "no_of_investments"

highinvest_C3_top_sector <-
  top_n(C3_name_summary, 1, no_of_investments)
highinvest_C3_top_sector <-
  arrange(highinvest_C3_top_sector,
          desc(no_of_investments, main_sector))
highinvest_C3_top_sector



#For point 4 (second best sector count-wise), which company received the highest investment? - C1
C1_name_group <-
  group_by(D1, country_code, main_sector, name)
C1_name_group <-
  subset(C1_name_group,
         country_code == "USA" &
           main_sector == "Cleantech / Semiconductors") # Top 2 sector for company C1 is 'Cleantech /Semiconductors'

C1_name_summary <- summarise(C1_name_group, n())
colnames(C1_name_summary)[4] <- "no_of_investments"

highinvest_C1_second_sector <-
  top_n(C1_name_summary, 1, no_of_investments)
highinvest_C1_second_sector <-
  arrange(highinvest_C1_second_sector,
          desc(no_of_investments, main_sector))
highinvest_C1_second_sector


#For point 4 (second best sector count-wise), which company received the highest investment? - C2
C2_name_group <-
  group_by(D2, country_code, main_sector, name)
C2_name_group <-
  subset(C2_name_group,
         country_code == "GBR" & 
           main_sector == "Cleantech / Semiconductors" # Top 2 sector for company C2 is 'Cleantech /Semiconductors'
  )
C2_name_summary <- summarise(C2_name_group, n())
colnames(C2_name_summary)[4] <- "no_of_investments"

highinvest_C2_second_sector <-
  top_n(C2_name_summary, 1, no_of_investments)
highinvest_C2_second_sector <-
  arrange(highinvest_C2_second_sector,
          desc(no_of_investments, main_sector))
highinvest_C2_second_sector


#For point 4 (second best sector count-wise), which company received the highest investment? - C3
C3_name_group <-
  group_by(D3, country_code, main_sector, name)
C3_name_group <-
  subset(C3_name_group,
         country_code == "IND" & 
           main_sector == "News, Search and Messaging" # Top 2 sector for company C3 is 'News, Search and Messaging'
  )
C3_name_summary <- summarise(C3_name_group, n())
colnames(C3_name_summary)[4] <- "no_of_investments"

highinvest_C3_second_sector <-
  top_n(C3_name_summary, 1, no_of_investments)
highinvest_C3_second_sector <-
  arrange(highinvest_C3_second_sector,
          desc(no_of_investments, main_sector))
highinvest_C3_second_sector