######### // Importing the Data & necessary libraries into WorkSpace // ##########

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
Uber_Request_Data <- read_csv("Uber Request Data.csv")

######### // Data Exploration, trying to have a feel of the Data // ##########

View(Uber_Request_Data)
str(Uber_Request_Data)
unique(Uber_Request_Data$Status) #"Trip Completed" "Cancelled" "No Cars Available"
unique(Uber_Request_Data$`Pickup point`) #"Airport" "City"
colSums(is.na(Uber_Request_Data)) #Driver id (2650), Drop Timestamp(3914)--No. of NA values in the 2 columns having NA values
sum(Uber_Request_Data$Status=="No Cars Available") #2650 JUstifying the NA values in Driver ID
sum(Uber_Request_Data$Status %in% c("No Cars Available","Cancelled")) #3914 justifying the NA values in Drop Timestamp
sum(duplicated(Uber_Request_Data$`Request id`)) # No Duplicates
sapply(Uber_Request_Data, function(x) length(which(x == ""))) # No blank "" values
length(unique(Uber_Request_Data$`Request id`)) #6745 unique values
nrow(Uber_Request_Data) #6745 row count, hence, no duplicates

######### // Data Preparation // ##########

Uber_Request_Data$Status <- as.factor(Uber_Request_Data$Status)
Uber_Request_Data$`Pickup point` <- as.factor(Uber_Request_Data$`Pickup point`)
Uber_Request_Data$`Request id` <- as.character(Uber_Request_Data$`Request id`)
Uber_Request_Data$`Driver id` <- as.character(Uber_Request_Data$`Driver id`)
summary(Uber_Request_Data) # No uppercase and lowercase problem

######### // Handling the Data Quality Issues // ##########

### Not imputing the NA values as the respective Columns are not used for analysis. If required, will impute the Values depending ###
### upon the kind of Analysis being done ###
Uber_Request_Data$`Request timestamp` <- str_replace_all(Uber_Request_Data$`Request timestamp`, "[-]", "/")
Uber_Request_Data$`Drop timestamp` <- str_replace_all(Uber_Request_Data$`Drop timestamp`, "[-]", "/")

######### // Derived Metrics  // ##########

#Uber_Request_Data$Drop_Point <- as.factor(ifelse(Uber_Request_Data$`Pickup point`=="City","Airport","City"))
Request_DateTime_Temp <-  strptime(Uber_Request_Data$`Request timestamp`, format = "%d/%m/%Y %H:%M")
Drop_DateTime_Temp <-  strptime(Uber_Request_Data$`Drop timestamp`, format = "%d/%m/%Y %H:%M")
Uber_Request_Data$Request_Day <- factor(format(Request_DateTime_Temp, "%d"))
Uber_Request_Data$Request_Time <- format(Request_DateTime_Temp, "%H:%M")
Uber_Request_Data$Drop_Day <- factor(format(Drop_DateTime_Temp, "%d"))
Uber_Request_Data$Drop_Time <- format(Drop_DateTime_Temp, "%H:%M")
#Uber_Request_Data$Travel_Time <- as.numeric(difftime(Drop_DateTime_Temp,Request_DateTime_Temp, units = "mins"))
Uber_Request_Issues <- Uber_Request_Data[which(Uber_Request_Data$Status %in% c("Cancelled", "No Cars Available")), ] #Filtering 
#for "Cancelled", "No Cars Available"
#summary(Uber_Request_Data$Travel_Time)

######### // Data Analysis & Visualisation // ##########

### Creating an Hour-wise(in 24-Hr Format) Frequency  Plot of Cancellation/No Cars Availability in respective PickUp Points
### Frequency for Cancellation/No Cars Available plotted in Stacked Histograms Hour Wise
### Line Graph for Visualising where the most Pickup Requests were made hour wise
### Aesthetics:> x:Hours, y:count GEometrics: fill = Status (for geom_bar) and colour = Pickup Point (for geom_line)
### Included ggtheme(theme_fivethirtyeight) as well to make the plots more visuaaly convinicive
ggplot(Uber_Request_Issues) +
  geom_bar(
    aes(x = factor(format(
      strptime(Request_Time, "%H:%M"), "%H"
    )),
    fill = Status),
    stat = "count",
    position = "stack",
    width = 0.5
  ) +
  geom_line(
    aes(x = as.numeric(format(
      strptime(Request_Time, "%H:%M"), "%H"
    )), colour = `Pickup point`),
    stat = "count",
    size = 1.45,
    alpha = 0.75,
    linetype = 3
  ) +
  scale_color_manual(values = c("blue", "#006400")) +
  labs(title = "Hour-wise(in 24-Hr Format) Frequency of Cancellation/No Cars Availability in respective PickUp Points") +
  theme_fivethirtyeight()
  
#Request_Issues_Time_Hrs <- format( strptime(Uber_Request_Issues$Request_Time, "%H:%M"), "%H")

### Assigment of different TimeSlots based on similar characteristics

Uber_Request_Data$Time_Slot <- ifelse(format(Request_DateTime_Temp, "%H") %in% c("00","01","02","03"), "Pre-Dawn",
                                      ifelse(format(Request_DateTime_Temp, "%H") %in% c("04","05","06","07","08","09","10"),"Morning_Rush",
                                             ifelse(format(Request_DateTime_Temp, "%H") %in% c("11","12","13","14","15","16"),"Day_Time",
                                                    ifelse(format(Request_DateTime_Temp, "%H") %in% c("17","18","19","20","21"),"Evening_Rush",
                                                           ifelse(format(Request_DateTime_Temp, "%H") %in% c("22","23"),"Midnight",NA)))))
Uber_Request_Data$Time_Slot <- factor(Uber_Request_Data$Time_Slot) 

### Plotting of Served/Unserved Requests Graph
### Line Graph depicting time slot wise Served/Unserved Requests
### Stacked Bar Chart depicting the Pickup Requests made time-slot wise, for comparison against the total
### Aesthetics:> x:Time_Slot, y:count Geometrics: color = Supply (for geom_line) and fill = Pickup Point (for geom_bar)
### Included ggtheme(theme_economist) as well to make the plots more visually convinicive
Uber_Request_Data$Request_Type <- ifelse(Uber_Request_Data$Status == "Trip Completed", "Served Request", "Unserved Request")

ggplot(Uber_Request_Data) +
  geom_line(aes(x=Time_Slot, group = Request_Type, color=Request_Type),stat = "count", size = 1.2)+
  scale_color_manual(values = c("blue", "red"))+
  geom_point(aes(x=Time_Slot, group = Request_Type, color=Request_Type),stat = "count", size = 3) +
  geom_bar(aes(x=Time_Slot, fill = Uber_Request_Data$`Pickup point`), stat = "count", alpha = 0.4, width = 0.5) +
  labs(fill = "Pickup Point") +
  ggtitle("Served/Unserved Requests in respective Pickup Points") +
  theme_economist()


### Calculating the Served/Unserved requests for each respective timeslots
request_data <- aggregate(Uber_Request_Data[c("Time_Slot","Request_Type")], by=list(Uber_Request_Data$Time_Slot,Uber_Request_Data$Request_Type), FUN = length)
request_data <- request_data[,-4]
colnames(request_data) <- c("Time_Slot","Request_Type", "Count")
request_data

### Plotting to recognise how requests were treated in different Time Slot Wise
### Line Graph depicting time slot wise how were the requests Handled
### Stacked Bar Chart depicting the Pickup Requests made time-slot wise, for comparison against the total
### Aesthetics:> x:Time_Slot, y:count Geometrics: color = Status (for geom_line) and fill = Pickup Point (for geom_bar)
### Included ggtheme(theme_economist) as well to make the plots more visually convinicive
ggplot(Uber_Request_Data) +
  geom_line(aes(x=Time_Slot, group = Status, color=Status),stat = "count", size = 1.2) +
  scale_color_manual(values = c("blue", "red", "#006400"))+
  geom_point(aes(x=Time_Slot, group = Status, color=Status),stat = "count", size = 3) +
  geom_bar(aes(x=Time_Slot, fill = Uber_Request_Data$`Pickup point`), stat = "count", alpha = 0.4, width = 0.5) +
  labs(fill = "Pickup Point") +
  ggtitle("Requests' Conversion at respective Pickup Points") +
  theme_economist()  

### Calculating the Request Conversions for each respective timeslots
request_conv_data <- aggregate(Uber_Request_Data[c("Time_Slot","Status")], by=list(Uber_Request_Data$Time_Slot,Uber_Request_Data$Status), FUN = length)
request_conv_data <- request_conv_data[,-4]
colnames(request_conv_data) <- c("Time_Slot","Status", "Count")
request_conv_data

    
#### Calculation of Supply & Demand for the respective Timezones
#### Calcualtion of Demand = "Trip Completed" + "Cancelled" + "No Cars Available"
Supply_Demand <- aggregate(Uber_Request_Data[c("Time_Slot")], by=list(Uber_Request_Data$Time_Slot), FUN=length)
colnames(Supply_Demand) <- c("Time_Slot","Demand")
#### Calculation of Supply =  "Trip Completed"
temp_data <- aggregate(subset(Uber_Request_Data, Status == "Trip Completed", select = c(Time_Slot)), 
                       by=list(subset(Uber_Request_Data, Status == "Trip Completed", select = c(Time_Slot))$Time_Slot), FUN=length)
colnames(temp_data) <- c("Time_Slot","Supply")
Supply_Demand <- merge(Supply_Demand,temp_data, by="Time_Slot")
Supply_Demand <- mutate(Supply_Demand, Gap = Demand-Supply)
Supply_Demand$Percent <- (Supply_Demand$Gap/Supply_Demand$Demand)*100
Supply_Demand

#### Calculation of Requests for respective pickup Points
pickup_data <- aggregate(Uber_Request_Data[c("Time_Slot","Pickup point")], 
                         by = list(Uber_Request_Data$Time_Slot, Uber_Request_Data$`Pickup point`), FUN = length)
pickup_data <- pickup_data[,-4]
colnames(pickup_data) <- c("Time_Slot","Pickup_Point","Count")
pickup_data

### Plotting the Supply Demand Graph
## Melting Data First
temp_data <- melt(Supply_Demand[c(1,2,3,4)], id.vars='Time_Slot')
ggplot(temp_data, aes(Time_Slot, value)) + geom_bar(aes(fill = variable), 
                                              width = 0.4, position = "fill", stat="identity")+
  theme_wsj() +
  theme(legend.title = 
          element_blank())+
  ggtitle("Supply Demand Graph")
