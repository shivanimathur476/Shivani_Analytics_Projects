#Packages needed 
library(lpSolveAPI)
library(ISLR)
library(caret)
library(dplyr)
# Reading the Green Taxi data for four months
NYC_data_Jan= read.csv("C:/Users/shiva/Documents/DataMining/Project Materials/Data/green_tripdata_2017-01.csv")
NYC_data_Feb= read.csv("C:/Users/shiva/Documents/DataMining/Project Materials/Data/green_tripdata_2017-02.csv")
NYC_data_May=read.csv("C:/Users/shiva/Documents/DataMining/Project Materials/Data/green_tripdata_2017-05.csv")
NYC_data_Jun=read.csv("C:/Users/shiva/Documents/DataMining/Project Materials/Data/green_tripdata_2017-06.csv")
#Combinig all the four months data
Combined_Data=rbind(NYC_data_Jan,NYC_data_Feb,NYC_data_May,NYC_data_Jun)
#Reading the weather data for four months as demand varies according to Weather
Weather_data=read.csv("C:/Users/shiva/Documents/DataMining/Project Materials/Data/weather_data_nyc.csv")
#Adding an hour column
Combined_Data$Hour_of_the_day = format(as.POSIXct(Combined_Data$lpep_pickup_time,format="%H:%M"),"%H")

#Finding hour of the day with highest frequency of cabs
Combined_Data %>% group_by(Hour_of_the_day) %>% summarise(no=n()) %>% arrange(desc(no))
Filtered_Time_Data <- filter(Combined_Data, (Hour_of_the_day==15) | (Hour_of_the_day==18 ) | (Hour_of_the_day=='09') )
################################ Data Cleaning ###############################
#Checking and removing rows with nearzerovariance
nearZeroVar(Combined_Data)
NZV_Removed_Data<-subset(Combined_Data,,-c(store_and_fwd_flag, RatecodeID,fare_amount,mta_tax,tolls_amount,ehail_fee,improvement_surcharge, trip_type))
Combined_Data<-subset(Combined_Data,,-c(16))

#impute missing values with median
Unnecessary_Attr_Removed$trip_distance[is.na(Unnecessary_Attr_Removed$trip_distance)] <- median(Unnecessary_Attr_Removed$trip_distance, na.rm = TRUE)
library(VIM)
#No missing values 
colMeans(is.na(Combined_Data))

################################Weather Data Cleaning###############################
#Cleaning WeatherData
library(scales)
library(lubridate)
WeatherData$lpep_pickup_date <- dmy(WeatherData$lpep_pickup_date)
WeatherData$lpep_pickup_date <- format(as.Date(WeatherData$lpep_pickup_date),"%m/%d/%Y")
WeatherData$precipitation_NEW <- as.numeric(gsub("T",0.01,WeatherData$precipitation))
WeatherData$snow.fall_NEW <- as.numeric(gsub("T",0.01,WeatherData$snow.fall))
WeatherData$snow.depth <- as.numeric(gsub("T",0.01,WeatherData$snow.depth))
WeatherData$minimum.temperature <- as.numeric(gsub("/",0,WeatherData$minimum.temperature))

Temp=strptime(WeatherData$lpep_pickup_date,format='%m/%d/%Y',tz='America/New_York')
WeatherData$Month_of_year = as.numeric(format(Temp, "%m"))
WeatherData=filter(WeatherData,(Month_of_year==1) |(Month_of_year==2) | (Month_of_year==5)| (Month_of_year==6))
################################Merging Taxi and weather data###############################
```{r}
#Merging weather and taxi data
Combined_Data$lpep_pickup_date <- mdy(Combined_Data$lpep_pickup_date)
Combined_Data$lpep_pickup_date <- format(date(Combined_Data$lpep_pickup_date),"%m/%d/%Y")
merged_data <- merge(x=Combined_Data,y=WeatherData,by="lpep_pickup_date")
```
################################Feature extraction###############################
```{r}
#Combining Hour of the day and Location Id

Combined_Data <-transform(Combined_Data,Location_Hour=paste(PULocationID,Hour_of_the_day,sep="_"))
Combined_Data <-transform(Combined_Data,Location_Day_Hour=paste(PULocationID,DayOfTheWeek,Hour_of_the_day,sep="_"))

#Adding Pickups column
Combined_Data1 <-Combined_Data
Pickups=Combined_Data1 %>% group_by(Hour_of_the_day, lpep_pickup_date) %>% summarize(Pickups=n()) 
Combined_Data1<-merge(Combined_Data1,Pickups, by=c("Hour_of_the_day","lpep_pickup_date"))
#Dividng into test and train
index <- sample(1:nrow(Combined_Data1),round(0.6*nrow(Combined_Data1)))
train_data <- Combined_Data1[index,]
test <- Combined_Data1[-index,]

index1 <- sample(1:nrow(test),round(0.5*nrow(test)))
Validation_data <- test[index1,]
Test_data <- test[-index,]
#Regression Model
train_data$lpep_dropoff_datetime<-NULL
train_data$lpep_pickup_time<-NULL
Test_data$lpep_dropoff_datetime<-NULL
Test_data$lpep_pickup_time<-NULL
Model=lm(Pickups ~ Hour_of_the_day + PULocationID + lpep_pickup_date  + trip_distance + average.temperature +precipitation_NEW,data=train_data)
summary(fit)
anova(fit)
#Running for Test Data
Test_data2 <-transform(Test_data2,Location_Hour=paste(PULocationID,Hour_of_the_day,sep="_"))
Test_data2 <- Test_data2 %>% group_by(Location_Hour,DayOfTheWeek,PULocationID,Hour_of_the_day,lpep_pickup_date,average.temperature,maximum.temperature,minimum.temperature,precipitation_NEW,snow.depth_NEW,snow.fall_NEW,Month_of_year) %>% summarise(Demand=n())
Test_data2 <- Test_data2 %>% group_by(Location_Hour) %>% summarise(Demand=n())
