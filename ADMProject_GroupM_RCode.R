#Set working directory
setwd("C:\\Users\\niraj\\OneDrive\\Desktop\\ADM project")

#Packages used in the analysis
library(caret)      
library(tidyverse)  
library(lubridate)  
library(VIM)        
library(Hmisc)      
library(GGally)     
library(scales)      
library(forecast)   

library(dplyr)
library(tidyr)
library(lubridate)
#install.packages('esquisse')
library(esquisse)
#install.packages('padr')
library(padr)
#install.packages('imputeTS')
library(imputeTS)
library(ggplot2)
library(chron)
#install.packages('plotly')
library(plotly)
library(forecast)
#install.packages('forecastHybrid')
library(forecastHybrid)#can be removed
#install.packages('zoo')
library(zoo)
#install.packages('shiny')
library(shiny)
#install.packages('visdat')
library(visdat)
library(sqldf)

# read raw data from text file
elec_data <- read_delim('ADMProject_GroupM_Dataset_HouseholdPowerConsumption.txt', col_names = TRUE, col_types = cols(Global_active_power='d', Global_reactive_power='d',Voltage='d', Global_intensity='d', Sub_metering_1='d', Sub_metering_2='d', Sub_metering_3='d'), delim=';',  na='?')

#Calculating root mean square voltage
elec_data$Vrms <- 1.11 * elec_data$Voltage
#Calculating Apparant power in watts
elec_data$Apparant_Power <- elec_data$Vrms * elec_data$Global_intensity
#Converting Apparant power in kilowatts
elec_data$Apparant_Power <- elec_data$Apparant_Power / 1000
#Calculating power factor
elec_data$Power_Factor <- elec_data$Global_active_power / elec_data$Apparant_Power

#Reactive energy consumed every minute (in watt hour) in the household, rounded to 4 digits of precision
elec_data$Global_reactive_power <- round(elec_data$Global_reactive_power*1000/60, digits = 4)
#Active energy consumed every minute (in watt hour) in the household, rounded to 4 digits of precision
elec_data$Global_active_power <- round(elec_data$Global_active_power*1000/60, digits = 4)

#Convert date format to date-month-year
elec_data$Date<- dmy(elec_data$Date)
#Combine rows of date and time
elec_data$DateTime<-paste(elec_data$Date,elec_data$Time)
#Convert the newly created date-time column to format: year-month-date hour-minute-second
elec_data$DateTime <- ymd_hms(elec_data$DateTime)
#Gives the total number of null values in all the columns
sum(is.na(elec_data))
#Checking for null values
sapply(elec_data,function(x) sum(is.na(x)))  
#Removing the null values
newtable <- elec_data[!is.na(elec_data$Sub_metering_3), ]
#Re-checking for null values
sapply(newtable,function(x) sum(is.na(x)))  
sum(is.na(newtable))

#Copy data to another data frame for exploratory data analysis
try <- newtable

#Create vector lists for months and days
MonthLst <- c('Jan', 'Feb', 'Mar','Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
              'Oct', 'Nov','Dec', 'Jan')

WkdayLst <- c('Mon', 'Tues', 'Wed', 'Thurs', 'Fri')

WkLst <- c('Sun','Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat', 'Sun' )

WkndList <- c('Sat', 'Sun', 'Mon')

#rename column names
colnames(try)[2] <- 'Glbl_actvPwr'
colnames(try)[3] <- 'Glbl_ractvPwr'
colnames(try)[7] <- 'Sub-Meter-1'
colnames(try)[8] <- 'Sub-Meter-2'
colnames(try)[9] <- 'Sub-Meter-3'

#Create tidy tibble
try_tidy <- try %>%
  gather(Meter, Watt_hr, `Sub-Meter-1`, `Sub-Meter-2`, `Sub-Meter-3`)

try_tidy %>% as_tibble(try_tidy)
is_tibble(try_tidy)

try_tidy$Meter <- factor(try_tidy$Meter)
glimpse(try_tidy)



##-Year Bar Chart
try_tidy %>%
  filter(year(DateTime)>2006) %>%
  
  group_by(year(DateTime), Meter) %>%
  summarise(sum=round(sum(Watt_hr/1000),3)) %>%
  ggplot(aes(x=factor(`year(DateTime)`), y=sum)) +
  labs(x='Year', y='kWh') +
  ggtitle('Total Energy Usage by Year') +
  geom_bar(stat='identity', aes(fill = Meter), colour='black')




###-Month- Line Plot
try_tidy %>%
  filter(year(DateTime)>2006) %>%
  mutate(Month=lubridate::month(DateTime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Meter) %>%
  summarise(sum=sum(Watt_hr/1000)) %>%
  ggplot(aes(x=factor(Month), sum, group=Meter, colour=Meter)) +
  labs(x='Month of the Year', y='KWh') +
  ggtitle('Average Monthly Energy Usage') +
  geom_line(size=1) +
  geom_line()



# Correlation plot
ggcorr(try_tidy) +
  ggtitle('Correlation Plot of Energy Consumption Dataset')
#Part 2
#checking power factor values greater than 1
pf <- sqldf('select count(Power_Factor) from elec_data where Power_Factor >1')
pf
#removing rows with power factor values greater than 1
elec_data <- elec_data[!(elec_data$Power_Factor > 1),]
#re-checking power factor values greater than 1; this should give result as zero
sqldf('select count(Power_Factor) from elec_data where Power_Factor >1')


#Create new columns of year, months, week and day
newtable$year <- year(newtable$DateTime)
newtable$month <- month(newtable$DateTime)
newtable$week <- week(newtable$DateTime)
newtable$day <- day(newtable$DateTime)

#Condition to get the data according to seasons i.e Winter, Spring, Summer and Autumn
newtable$Season <- ifelse(newtable$month == 12|newtable$month == 1|newtable$month == 2,"Winter",
                          ifelse(newtable$month == 3|newtable$month == 4|newtable$month == 5, "Spring",
                                 ifelse(newtable$month == 6|newtable$month == 7|newtable$month == 8, "Summer", "Autumn"))) #season info



# Grouping done for Power Factor
seasonal_pf <- newtable %>% group_by(Season) %>%
  summarize_at(vars(Power_Factor), funs(mean))
colnames(seasonal_pf)[1] <- "Seasons"



#Plot of seasonal trend for power factor
plot_ly(seasonal_pf, x = ~seasonal_pf$Seasons, y = ~seasonal_pf$Power_Factor, 
        name = 'Averaged Power Factor', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Factor averaged by Season",
         xaxis = list(title = "Season"),
         yaxis = list (title = "Power Factor"))


#declaring a variable for aggregated data
aggregate_pf <- c()

#Creating vector for all the levels of granularity
granularity_vector <- c("year", "month", "day", "week")


#Creating aggregated vector for power factor
for(i in granularity_vector){
  aggregate_pf[[i]] <- newtable %>%
    group_by(DateTime=floor_date(DateTime, i)) %>%
    dplyr::summarize_at(vars(
      Power_Factor),
      funs(mean))
}  

#Plot for Monthly Power factor values
plot_ly(aggregate_pf[["month"]], 
        x = ~aggregate_pf[["month"]]$DateTime, 
        y = ~aggregate_pf[["month"]]$Power_Factor, 
        name = 'Power Factor', type = 'scatter', mode = 'lines') %>%
  layout(title = paste("Monthly Power Factor"),
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power Factor"))



#Creating monthly time-series object for entire data 
total_monthly_ts <- ts(aggregate_pf[["month"]], start = c(2006,12), frequency = 12)
total_monthly_ts_df <- as.data.frame(total_monthly_ts)

#Creating monthly time-series object for data in year range 2007-2009
ref_monthly_ts <-  ts(aggregate_pf[["month"]], start = c(2007,1), end = c(2009,12), frequency = 12)  

#Creating weekly time-series object for entire data
total_weekly_ts <- ts(aggregate_pf[["week"]], start = c(2006,51), frequency = 52)
total_weekly_ts_df <- as.data.frame(total_weekly_ts)

#Creating daily time-series object for entire data
total_daily_ts <- ts(aggregate_pf[["day"]], start = c(2006,350), frequency = 365)
total_daily_ts_df <- as.data.frame(total_daily_ts)


#declaring the below variables
total_monthly_ts_var <- list()
total_weekly_ts_var <- list()
total_daily_ts_var <- list()

#Declaring variable to create a list of all the required fields
variables <- list("Power_Factor")

monthly_decom_var <- list()
monthly_remain_var <- c()

weekly_decom_var <- list()
weekly_remain_var <- c()

daily_decom_var <- list()
daily_remain_var <- c()

for(j in variables) {
  total_monthly_ts_var[[j]] <- total_monthly_ts [ ,j]
  total_weekly_ts_var[[j]] <- total_weekly_ts [ ,j]
  total_daily_ts_var[[j]] <- total_daily_ts [ ,j]
  
  # Monthly decomposition
  monthly_decom_var[[j]] <- total_monthly_ts_var[[j]] %>% stl(s.window = 12) 
  monthly_remain_var[[j]] <- total_monthly_ts_var[[j]] %>% stl(s.window = 12) %>% remainder()
  
  
  # Weekly decomposition
  weekly_decom_var[[j]] <- total_weekly_ts_var[[j]] %>% stl(s.window = 52) 
  weekly_remain_var[[j]] <- total_weekly_ts_var[[j]] %>% stl(s.window = 52) %>% remainder()
  
  
  # Daily decomposition
  daily_decom_var[[j]] <- total_daily_ts_var[[j]] %>% stl(s.window = 365) 
  daily_remain_var[[j]] <- total_daily_ts_var[[j]] %>% stl(s.window = 365) %>% remainder()
}

monthly_remain <- as.data.frame(monthly_remain_var$Power_Factor)
monthly_remain <- mean(abs(monthly_remain$x)) / mean(total_monthly_ts_df$Power_Factor)

weekly_remain <- as.data.frame(weekly_remain_var$Power_Factor)
weekly_remain <-mean(abs(weekly_remain$x)) / mean(total_weekly_ts_df$Power_Factor)

daily_remain <- as.data.frame(daily_remain_var$Power_Factor)
daily_remain <- mean(abs(daily_remain$x)) / mean(total_daily_ts_df$Power_Factor)

##Plot for remainders of each granularity
granular_remain <- c(monthly_remain, weekly_remain, daily_remain)
granular_remain <- as.data.frame(granular_remain)
granular_remain$granularity <- c("month", "week","day")
colnames(granular_remain) <- c("remainder", "granularity")


library(ggplot2)
ggplot(data = granular_remain) +
  aes(x = granularity, weight = remainder) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Randomness for each granularity",
       y = "Randomness (Relative)") +
  theme_minimal()



#Division into training and testing
total_train <- window(total_monthly_ts[,"Power_Factor"], start = c(2006,12), end = c(2009,12))  #training upto dec 2009
total_test <- window(total_monthly_ts[,"Power_Factor"], start= c(2010,1)) #testing from jan 2010 to end

#ETS model for forecasting
ETS_model_total = ets(total_train)
ETS_prediction_total = forecast(ETS_model_total, h=12) # forecast 12 months ahead 
ETS_prediction_total
plot(ETS_prediction_total)
accuracy(ETS_prediction_total, total_test) #check metrics comparing prediction in test



#ARIMA model for forecasting
ARIMA_model_total <- auto.arima(total_train)
ARIMA_prediction_total<- forecast(ARIMA_model_total, h= 12)  # forecast 12 months ahead 
ARIMA_prediction_total
plot(ARIMA_prediction_total)
accuracy(ARIMA_prediction_total, total_test)  #check metrics for evaluating model


#Holt-Winters model for forecasting
HoltW_model <- HoltWinters(total_train)
HoltW_prediction <- forecast(HoltW_model, h= 12) # forecast 12 months ahead
HoltW_prediction
plot(HoltW_prediction)   
accuracy(HoltW_prediction, total_test)  #check metrics for evaluating model


