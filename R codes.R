library(ggplot2)
library(Amelia)
library(readr)
library(plyr)
library(readxl)
library(dplyr)
library(forecast)
remove(list = ls())


NASA <- read_excel(file.choose(), sheet=3)
str(NASA)
NASA$Temperature <- (NASA$Temperature+ 57.2)
str(NASA)

#####################
##Part 1: Forecast the global average temperatures through year 2100:
#####################
NASA_ts<- ts(NASA$Temperature, start=c(1880,1,1), frequency=12)
NASA_fit_multiplicative <- decompose(NASA_ts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(NASA_fit_multiplicative)

NASA_fit_additive<- decompose(NASA_ts, type="additive") #decompose using "classical" method, additive form
plot(NASA_fit_additive)

NASA_fit_periodic <- stl(NASA_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(NASA_fit_periodic)

plot(NASA_ts)

################################################################

NASA_temperature_AAA <- ets(NASA_ts, model="AAA", damped=FALSE)
NASA_temperature_AAA_d <- ets(NASA_ts, model="AAA", damped=TRUE)
NASA_temperature_MMM_d <- ets(NASA_ts, model="MMM", damped=TRUE)
NASA_temperature_MMM <- ets(NASA_ts, model="MMM", damped=FALSE)
NASA_temperature_ZMM <- ets(NASA_ts, model="MMM", damped=FALSE)


NASA_temperature_AAA_pred <- forecast(NASA_temperature_AAA, h=969, level=c(0.75, 0.90))
NASA_temperature_AAA_pred_d <- forecast(NASA_temperature_AAA_d, h=969, level=c(0.75, 0.90))
NASA_temperature_MMM_pred_d <- forecast(NASA_temperature_MMM_d, h=969, level=c(0.75, 0.90))
NASA_temperature_MMM_pred <- forecast(NASA_temperature_MMM, h=969, level=c(0.75, 0.90))

NASA_tbats <- tbats(NASA_ts)
NASA_tbats_pred <-forecast(NASA_tbats, h=969, level=c(0.75, 0.90))

# Compare the prediction "cones" visually
par(mfrow=c(1,4)) 
par(mfrow=c(1,1)) 
plot(NASA_temperature_AAA_pred, xlab="Year", ylab="Global Average Temperatures")
plot(NASA_temperature_AAA_pred_d, xlab="Year", ylab="Global Average Temperatures")
plot(NASA_temperature_MMM_pred_d, xlab="Year", ylab="Global Average Temperatures")
plot(NASA_temperature_MMM_pred, xlab="Year", ylab="Global Average Temperatures")

NASA_temperature_AAA
NASA_temperature_MMM

###TBATS
par(mfrow=c(1,1)) 
plot(NASA_tbats_pred, xlab="Year", ylab="Global Average Temperatures")
NASA_tbats


### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)


f_AAA  <- function(y, h) forecast(ets(y, model="AAA", damped=FALSE), h = h)
errors_AAA <- tsCV(NASA_ts, f_AAA, h=1, window=842)

f_AAA_d  <- function(y, h) forecast(ets(y, model="AAA",damped=TRUE), h = h)
errors_AAA_d <- tsCV(NASA_ts, f_AAA_d, h=1, window=842)

f_MMM_d  <- function(y, h) forecast(ets(y, model="MMM", damped=TRUE), h = h)
errors_MMM_d <- tsCV(NASA_ts, f_MMM_d, h=1, window=842)

f_MMM  <- function(y, h) forecast(ets(y, model="MMM", damped=FALSE), h = h)
errors_MMM <- tsCV(NASA_ts, f_MMM, h=1, window=842)



####################################### Plots errors
par(mfrow=c(1,1)) 
plot(errors_AAA, ylab='tsCV errors')
abline(0,0)
lines(errors_AAA_d, col="red")
lines(errors_MMM_d, col="green")
lines(errors_MMM, col="blue")
legend("left", legend=c("CV_error_AAA", "CV_error_AAA_d","CV_error_MMM_d","CV_error_MMM"), col=c("black", "red", "green", "blue"), lty=1:4)
################################## Plots

mean(abs(errors_AAA/NASA_ts), na.rm=TRUE)*100
mean(abs(errors_AAA_d/NASA_ts), na.rm=TRUE)*100
mean(abs(errors_MMM_d/NASA_ts), na.rm=TRUE)*100
mean(abs(errors_MMM/NASA_ts), na.rm=TRUE)*100

f_TBATS  <- function(y, h) forecast(tbats(y), h = h)
errors_TBATS <- tsCV(NASA_ts, f_TBATS, h=1, window=842)
mean(abs(errors_TBATS/NASA_ts), na.rm=TRUE)*100

plot(errors_AAA, ylab='tsCV errors', col="green")
abline(0,0)
lines(errors_MMM, col="blue")
lines(errors_TBATS, col="gray")
legend("left", legend=c("CV_error_AAA", "CV_error_MMM","CV_error_TBATS"), col=c("green", "blue", "gray"), lty=1:4)



# Print the mean and confidence intervals for the MMZ model
NASA_tbats_pred

# Export the results out

file1<-data.frame(Year = NASA$Year,Temparature = NASA_tbats_pred)
write.csv(NASA_tbats_pred, file1 = "Predicted Global Average Nasa.csv") # export the selected model's predictions into a CSV file


##############
##Part 2: point predictions, as well as the 90% confidence intervals for the global average temperatures for January and July 2030, 2050 and 2100?
##############

# Print the mean and confidence intervals for the MMZ model
NASA_tbats_pred

# Export the results out

file1<-data.frame(Year = NASA$Year,Temparature = NASA_tbats_pred)
write.csv(NASA_tbats_pred, file1 = "Predicted Global Average Nasa.csv") # export the selected model's predictions into a CSV file


##########################
####part 3: the quantities from Q2 for the Kingston, ON (postal code K7L3N6)
##########################
## Load Data ##

UK.kingston <- read_excel(choose.files())

UK.kingston1 <- gather(UK.kingston, "Month", "Temp", 2:13)
UK.kingston1$Month <- match(UK.kingston1$Month,month.abb) # converting char month to number
UK.kingston1$time.ID <- as.yearmon(paste(UK.kingston1$Year, UK.kingston1$Month), "%Y %m")
UK.kingston1 <- UK.kingston1[c(4,3)]
UK.kingston1 <- arrange(UK.kingston1, UK.kingston1$time.ID)
UK.kingston1$Temp <- UK.kingston1$Temp + 14
UK.KINGSTON <- UK.kingston1[1:2042,]
remove(UK.kingston1)


Kingston.Temp.ts <- ts(UK.KINGSTON$Temp, start = 1850, frequency = 12)

TBATS.Kingston <- tbats(Kingston.Temp.ts)

predict.Kingston <- forecast(TBATS.Kingston, h=969, level=0.9)
plot(predict.Kingston)

write.csv(predict.Kingston, "C:\\Users\\qianh\\Desktop\\Kingston.Prediction.csv", row.names = TRUE)




##################
###the rest:
##################

NASA.global <- read_excel(choose.files(),sheet=3)
UK.global <- read_excel(choose.files(),sheet=2)

NASA.global$Temperature <- NASA.global$Temperature + 57.2
NASA.global.1951.1980.ts <- ts(NASA.global$Temperature, start = c(1951,1), end=c(1980,12), frequency = 12)

TBATS.NASA.1951.1980<- tbats(NASA.global.1951.1980.ts)
predict.NASA.1981.2020 <- forecast(TBATS.NASA.1951.1980, h=471, level=c(0.7,0.8,0.9,0.95))

plot(predict.NASA.1981.2020)

write.csv(predict.NASA.1981.2020, "C:\\Users\\qianh\\Desktop\\predict.NASA.1981.2020.csv", row.names = TRUE)


UK.global$Temperature <- UK.global$Temperature + 14
UK.global.1961.1990.ts <- ts(UK.global$Temperature, start = c(1961,1), end=c(1990,12), frequency = 12)

TBATS.UK.1961.1990<- tbats(UK.global.1961.1990.ts)
predict.UK.1991.2020 <- forecast(TBATS.UK.1961.1990, h=360, level=c(0.7,0.8,0.9,0.95))

plot(predict.UK.1991.2020)

write.csv(predict.UK.1991.2020, "C:\\Users\\qianh\\Desktop\\predict.UK.1991.2020.csv", row.names = TRUE)



## Question 5

NASA.Predict.2013.Temp <- c(56.66, 56.70, 56.72, 56.72, 56.73, 56.72, 56.77, 56.74, 56.76, 56.76, 56.74, 56.74)
NASA.Actual.2013.Temp <- c(57.91, 57.83, 57.87, 57.76, 57.82, 57.91, 57.81, 57.90, 57.97, 57.89, 58.05, 57.90)
NASA.Predict.2020.Temp <- c(56.72, 56.73, 56.72, 56.77, 56.74, 56.76, 56.76, 56.74, 56.74, 56.66, 56.70, 56.72)
NASA.Actual.2020.Temp <- c(58.22, 58.06, 58.12, 58.14, 58.14, 58.12, 58.22, 58.20, 58.30, 58.37, 58.45, 58.39)

SD.DEV.NASA.Predict.2013 <- sd(NASA.Predict.2013.Temp)
#0.0298481
SD.DEV.NASA.Actual.2013 <- sd(NASA.Actual.2013.Temp)
#0.07681146

SD.DEV.NASA.Predict.2020 <- sd(NASA.Predict.2020.Temp)
#0.0298481
SD.DEV.NASA.Actual.2020 <- sd(NASA.Actual.2020.Temp)
#0.1238859




UK.2013.predict.Temp <- c(13.83, 14.47, 14.15, 13.83, 13.79, 13.69, 14.14, 14.28, 14.11, 13.93, 13.82, 13.66)
UK.2013.Actual.Temp <- c(14.47, 14.50, 14.42, 14.45, 14.53, 14.50, 14.52, 14.54, 14.55, 14.52, 14.66,14.53)
UK.2020.predict.Temp <- c(14.15, 13.83, 13.79, 13.69, 14.14, 14.28, 14.11, 13.93, 13.82, 13.66, 13.83, 14.47)
UK.2020.Actual.Temp <- c(14.874, 14.78, 14.61, 14.708, 14.706, 14.719, 14.713, 14.752, 14.693, 14.88, 14.982, 14.999)


SD.DEV.UK.Predict.2013 <- sd(UK.2013.predict.Temp)
#0.2517033
SD.DEV.UK.Actual.2013 <- sd(UK.2013.Actual.Temp)
#0.05961366

SD.DEV.UK.Predict.2020 <- sd(UK.2020.predict.Temp)
#0.2517033
SD.DEV.UK.Actual.2020 <- sd(UK.2020.Actual.Temp)
#0.1219026



##NASA dataset......................................................................................................................................

NASA_data <- read_excel("C:/Users/sharg/Desktop/Geeta College Assignments/867- Predictive Modelling/Assignment 2/New folder/Combined.xlsx",sheet = 3)

NASA_data$Temperature <- (NASA_data$Temperature+ 57.2)

NASA06_ts <-ts(NASA_data$Temperature, start=c(1907,1),end=c(2006,12),frequency=12)
str(NASA06_ts)

##tbats model for NASA ##

NASA_tbats <- tbats(NASA06_ts)
NASA_tbats_pred <-forecast(NASA_tbats, h=132, level=c(0.75, 0.90))

#export the predicted temp for 2007 to 2017 into a cvs file##

write.csv(NASA_tbats_pred, "Predicted_temp_NASA_07.csv")

### Read_predicted data/compare ##

predicted_07_17 <- read.csv(file.choose())

Actual_NASAdata <- subset(NASA_data,(NASA_data$Year >=2007 & NASA_data$Year <=2017))

NASA_actual <- select(Actual_NASAdata, Temperature)
predicted_NASA.07 <- select(predicted_07_17, Point.Forecast)
predicted_NASA.07

str(NASA_actual)

#calculate MAPE:
percent.errors.nasa <- abs(predicted_NASA.07- NASA_actual)/predicted_NASA.07
mean(percent.errors.nasa$Point.Forecast)*100 #### mape for NASA TABTS value is 1.097825 ############

#calculate RMSE:
residual<-predicted_NASA.07 - NASA_actual
Q7NASA_TBATS_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7NASA_TBATS_RMSE
#### RMSE for NASA TABTS value is 0.6575786 ############


#NASA data .............................................................................................................................
#Constant temperature approach _Naive method

NASA_naive_pred06 <-naive(NASA06_ts,h=132)
write.csv(NASA_naive_pred06, "NASA_07_pred_naive.csv")

#calculate MAPE
NASA_naive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_naive_07 <-select(NASA_naive_07,Point.Forecast)

percent.error.naive <-abs(NASA_naive_07 - NASA_actual)/ NASA_naive_07
percent.error.naive$Point.Forecast

mean(percent.error.naive$Point.Forecast)*100
#### MAPE for NASA naive value is 0.454177 ############

#calculate RMSE:
residual<-NASA_naive_07 - NASA_actual
Q7naive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7naive_RMSE
#### RMSE for NASA naive value is 0.3153762 ############



##Seasonal naive method ####################################################
str(NASA06_ts)

NASA_snaive_pred07 <-snaive(NASA06_ts,h=132)
write.csv(NASA_snaive_pred07, "NASA_07_pred_snaive.csv")

NASA_snaive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_snaive_2007 <-select(NASA_snaive_07,Point.Forecast)

percent.error.snaive <-abs(NASA_snaive_2007 - NASA_actual)/ NASA_snaive_2007
mean(percent.error.snaive$Point.Forecast)*100 

#MAPE is 1.000181 using snaive method

#calculate RMSE:
residual<-NASA_snaive_2007 - NASA_actual
Q7snaive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7snaive_RMSE
#### RMSE for NASA using snaive method is 0.6164137 ############


##ETS (ANN) method (without seasonality and trend)####################################################

NASA_temp_ANN_2006 <- ets(NASA06_ts, model="ANN", damped=FALSE)
NASA_temp_ANN_2007_pred <- forecast(NASA_temp_ANN_2006, h=132, level=c(0.75, 0.90))

write.csv(NASA_temp_ANN_2007_pred,"2007-2017_ETS(ANN)_Predicted NASA.csv")

predicted_07_ETS_ANN <- read.csv(file.choose())

plot(NASA_temp_ANN_2007_pred, xlab="Year", ylab="Global Average Temperatures ANN 2007- 2017")

NASA_pred_07_ETS_ANN <- select (predicted_07_ETS_ANN,Point.Forecast)


percent.error.ets.ann.nasa <- abs(NASA_pred_07_ETS_ANN - NASA_actual)/NASA_pred_07_ETS_ANN
percent.error.ets.ann.nasa$Point.Forecast
mean(percent.error.ets.ann.nasa$Point.Forecast)*100  

## MAPE value is 0.6473988 with ANN_ets model

#calculate RMSE:
residual<-NASA_pred_07_ETS_ANN - NASA_actual
Q7ets_ANN_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7ets_ANN_RMSE
#### RMSE for NASA using ets(ANN) method is 0.4141276 ############




##UK dataset......................................................................................................................................

UK_data <- read_excel("C:/Users/sharg/Desktop/Geeta College Assignments/867- Predictive Modelling/Assignment 2/New folder/met_life data.xlsx")

UK_data$Temperature <- (UK_data$Temperature + 14)

UK_06 <- ts(UK_data$Temperature, start=c(1907,1), end=c(2006,12),frequency=12)

str(UK_06)

#####
## tbats model for uk dataset
######

UK06_tbats <- tbats(UK_06)
UK_tbats_pred <-forecast(UK06_tbats, h=132, level=c(0.75, 0.90))

##export the predicted temperature from 2007 to 2017 values in cvs

write.csv(UK_tbats_pred, "Predicted temp_UK07.csv")

### Read_predicted data/compare ##

predicted.uk.07 <- read.csv(file.choose(), header=TRUE, sep = ",")

str(predicted.uk.07)

actual.uk.data <- subset(UK_data,(UK_data$Year >=2007 & UK_data$Year <=2017))
uk.actual <- select(actual.uk.data, Temperature)
predicted.ukdata <- select(predicted.uk.07, Point.Forecast)
str(predicted.ukdata)
str(uk.actual)
uk.actual <- data.frame(uk.actual)
predicted.ukdata <- as.data.frame(predicted.ukdata)

percent.errors.uk <- abs(predicted.ukdata-uk.actual)/predicted.ukdata
mean(percent.errors.uk$Point.Forecast)*100 

#### mape for UK TBATS value is 4.540998 ############

#calculate RMSE:
residual<-predicted.ukdata - uk.actual
Q7tbats_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7tbats_ukRMSE
#### RMSE for UK using TBATS method is 0.6877331 ############



#UK data .............................................................................................................................
#Constant temperature approach _Naive method

UK_naive_pred07 <-naive(UK_06,h=132)
write.csv(UK_naive_pred07, "UK_07_pred_naive.csv")

#calculate MAPE
UK_naive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_naive_07 <-select(UK_naive_07,Point.Forecast)

percent.error.uk.naive <-abs(UK_naive_07 - uk.actual)/ UK_naive_07
percent.error.uk.naive$Point.Forecast

mean(percent.error.uk.naive$Point.Forecast)*100 
#MAPE value using naive method is 4.260964

#calculate RMSE:
residual<-UK_naive_07 - uk.actual
Q7naive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7naive_ukRMSE
#### RMSE for UK using naive method is 0.6173509 ############


##Seasonal naive method ####################################################
str(UK_06)

UK_snaive_pred07 <-snaive(UK_06,h=132)
write.csv(UK_snaive_pred07, "UK_07_pred_snaive.csv")

UK_snaive_07 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_snaive_2007 <-select(UK_snaive_07,Point.Forecast)

percent.error.uk.snaive <-abs(UK_snaive_2007 - uk.actual)/ UK_snaive_2007
mean(percent.error.uk.snaive$Point.Forecast)*100 

#MAPE value is 4.409947 using snaive method

#calculate RMSE:
residual<-UK_snaive_2007 - uk.actual
Q7snaive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7snaive_ukRMSE
#### RMSE for UK using snaive method is 0.6808314 ############


##ETS (ANN) method (without seasonality and trend)####################################################

UK_temp_ANN_07 <- ets(UK_06, model="ANN", damped=FALSE)
UK_temp_ANN_2007_pred <- forecast(UK_temp_ANN_07, h=132, level=c(0.75, 0.90))

write.csv(UK_temp_ANN_2007_pred,"2007-17_ANN_Predicted UK.csv")

predicted_uk07_ETS_ANN <- read.csv(file.choose())

plot(UK_temp_ANN_2007_pred, xlab="Year", ylab="Global Average Temperatures ANN 2007- 2017")

UK_pred_07_ETS_ANN <- select (predicted_uk07_ETS_ANN,Point.Forecast)


percent.error.uk.ets_ANN<- abs(UK_pred_07_ETS_ANN - uk.actual)/UK_pred_07_ETS_ANN
percent.error.uk.ets_ANN$Point.Forecast
mean(percent.error.uk.ets_ANN$Point.Forecast)*100  

## MAPE value is 4.655204  with ANN_ets model

#calculate RMSE:
residual<-UK_pred_07_ETS_ANN - uk.actual
Q7UK_ets_ANN_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7UK_ets_ANN_RMSE
#### RMSE for UK using ets(ANN) method is 0.6682177 ############


################################## Plots_ Actual temperature vs. Predicted #####################################
nasa.tbats<- read.csv(file.choose())
nasa.naive <- read.csv(file.choose())
nasa.snaive <- read.csv(file.choose())
nasa.ANN <- read.csv(file.choose())

plot(NASA_actual$Temperature,ylab = "Global nasa temperatures", xlab = "Observations")
lines(nasa.tbats$Point.Forecast,col = "yellow")
lines(NASA_actual$Temperature,col="blue")
lines(nasa.ANN$Point.Forecast, col = "purple")
lines(nasa.naive$Point.Forecast, col = "Dark green")
lines(nasa.snaive$Point.Forecast, col = "orange")
legend("top", legend=c("TBATS","Actual","Naive","Snaive","ets(ANN)"), 
       col=c(" yellow", "blue","purple","purple","orange"), lty=1:6)

uk.tbats <-read.csv(file.choose())
uk.naive <- read.csv(file.choose())
uk.snaive <- read.csv(file.choose())
uk.ANN <- read.csv(file.choose())


plot(uk.actual$Temperature,ylab = "Global UK temperatures", xlab = "Observations")
lines(uk.tbats$Point.Forecast, col = "brown")
lines(uk.actual$Temperature,col="steelblue")
lines(uk.naive$Point.Forecast, col = "purple")
lines(uk.ANN$Point.Forecast, col = "Dark green")
lines(uk.snaive$Point.Forecast, col = "orange")
legend("top", legend=c("TBATS","Actual","Naive","Snaive","ets(ANN)"), 
       col=c(" Brown", "steelblue","purple","Dark green","orange"), lty=1:6)


##NASA dataset......................................................................................................................................
NASA_data <- read_excel(file.choose(), sheet=3)
str(NASA_data)

NASA_data$Temperature <- (NASA_data$Temperature+ 57.2)
str(NASA_data)

NASA_ts <- ts(NASA_data$Temperature, start=c(1880,1), end = c(2008,12) ,frequency=12) # Traing data

plot(NASA_ts)

##################
#TBATS

NASA_tbats <- tbats(NASA_ts)

# Using TBATS model from question 1 - predicting 2009 to 2019 temperature,  h =(11 years * 12 months = 132 months)

NASA_tbats_pred <- forecast(NASA_tbats, h= 132, level=c(0.75,0.90))

##### Plot #######
par(mfrow=c(1,1)) 
plot(NASA_tbats_pred, xlab="Year", ylab="Global Average Temperatures 2009-2019")
NASA_tbats

#### Export the predicted values temp (2009-19) in csv #####

write.csv(NASA_tbats_pred, "2009-19_Predicted_temp_NASA.csv")

##Predicted NASA temperatures from 2009 to 2019 tbats model ####

predicted.NASA.09 <-read.csv(file.choose())

NASA.predicted.09 <-select(predicted.NASA.09,Point.Forecast)
actual.NASA.data <- subset(NASA_data,(NASA_data$Year >=2009 & NASA_data$Year <=2019))

NASA.actual <-select(actual.NASA.data, Temperature)

percent.error.nasa <- abs(NASA.predicted.09- NASA.actual)/NASA.predicted.09
percent.error.nasa$Point.Forecast

mean(percent.error.nasa$Point.Forecast)*100 

###Mape for NASA is 0.3731635 using TBATS model

#calculate RMSE:
residual<-NASA.predicted.09 - NASA.actual
Q7NASA_TBATS_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q7NASA_TBATS_RMSE
#### RMSE for NASA TABTS value is 0.2666195 ############



####Constant temperature approach _Naive method#######################

NASA_naive_pred09 <-naive(NASA_ts,h=132)
write.csv(NASA_naive_pred09, "NASA_09_pred_naive.csv")

#calculate MAPE

NASA_naive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_naive_09 <-select(NASA_naive_09,Point.Forecast)

percent.error.naive <- abs(NASA_naive_09 - NASA.actual)/ NASA_naive_09
percent.error.naive$Point.Forecast
mean(percent.error.naive$Point.Forecast)*100 
#MAPE value for NASA using naive method is 0.44872

#calculate RMSE:
residual<-NASA_naive_09 - NASA.actual
Q9_NASA_naive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9_NASA_naive_RMSE
#### RMSE for NASA naive value is 0.3095133 ############







####Seasonal naive method ##################################
str(NASA_ts)

NASA_snaive_pred09 <-snaive(NASA_ts,h=132)
write.csv(NASA_snaive_pred09, "NASA_09_pred_snaive.csv")

NASA_snaive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
NASA_snaive_09 <-select(NASA_snaive_09,Point.Forecast)

percent.error.snaive <-abs(NASA_snaive_09 - NASA.actual)/ NASA_snaive_09
mean(percent.error.snaive$Point.Forecast)*100 
#MAPE value for NASA using snaive method is 0.4612283

#calculate RMSE:
residual<-NSA_snaive_09 - NASA.actual
Q9NASA_snaive_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9NASA_snaive_RMSE
#### RMSE for NASA using snaive method is 0.3244027 ############



###### Using ETS (ANN) Model###################

NASA_temp_ANN_2009 <- ets(NASA_ts, model="ANN", damped=FALSE)
NASA_temp_ANN_2009_pred <- forecast(NASA_temp_ANN_2009, h=132, level=c(0.75, 0.90))

write.csv(NASA_temp_ANN_2009_pred,"2009-19_(ANN)_Predicted NASA.csv")

predicted_09_ETS_ANN <- read.csv(file.choose())

plot(NASA_temp_ANN_2009_pred, xlab="Year", ylab="Global Average Temperatures ANN 2009- 2019")

NASA_pred_09_ETS_ANN <- select (predicted_09_ETS_ANN,Point.Forecast)

percent.error.ets_ann <- abs(NASA_pred_09_ETS_ANN - NASA.actual)/NASA_pred_09_ETS_ANN 
percent.error.ets_ann$Point.Forecast
mean(percent.error.ets_ann$Point.Forecast)*100  
## MAPE for NASA using ETS (ANN) method is 0.3709477

#calculate RMSE:
residual<-NASA_pred_09_ETS_ANN - NASA.actual
Q9NASA_ets_ann_RMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9NASA_ets_ann_RMSE
#### RMSE for NASA using ets(ANN) method is 0.2666888 ############


#################################################################################################################################################
#UK data set 

UK_data <- read_excel(file.choose())

UK_data$Temperature <-(UK_data$Temperature + 14)

UK_09 <- ts(UK_data$Temperature, start=c(1850,1), end=c(2008,12),frequency=12)

str(UK_09)

#####
## tbats model for uk dataset to predict year 2009 to 2019 (h = 11 years* 12 =132 months)
######

UK09_tbats <- tbats(UK_09)
UK_tbats_pred <-forecast(UK09_tbats, h=132, level=c(0.75, 0.90))

##export the predicted temperature from 2007 to 2017 values in cvs

write.csv(UK_tbats_pred, "2009-19_Predicted_temp_UK.csv")

### Read_predicted data/compare ##

predicted.uk.09 <- read.csv(file.choose(), header=TRUE, sep = ",")

str(predicted.uk.09)

actual.uk.data <- subset(UK_data,(UK_data$Year >=2009 & UK_data$Year <=2019))


uk.actual <- select(actual.uk.data, Temperature)
str(uk.actual)
predicted.ukdata <- select(predicted.uk.09, Point.Forecast)
str(predicted.ukdata)
str(uk.actual)
uk.actual <- data.frame(uk.actual)
predicted.ukdata <- as.data.frame(predicted.ukdata)

percent.errors.uk <- abs(predicted.ukdata-uk.actual)/predicted.ukdata
mean(percent.errors.uk$Point.Forecast)*100 
# MAPE is 0.7040578 for UK 2009 - 2019 predicted value using TBATS


#calculate RMSE:
residual<-predicted.ukdata - uk.actual
Q9tbats_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9tbats_ukRMSE
#### RMSE for UK using tbats method is 0.1414572 ############


####.....................................................................................................................................................
# constant approach with Naive method - using UK dataset

UK_naive_pred09 <-naive(UK_09,h=132)
write.csv(UK_naive_pred09, "UK_09_pred_naive.csv")

#calculate MAPE

UK_naive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_naive_09 <-select(UK_naive_09,Point.Forecast)

percent.error.uk.naive <- abs(UK_naive_09 - uk.actual)/ UK_naive_09
percent.error.uk.naive$Point.Forecast
mean(percent.error.uk.naive$Point.Forecast)*100 
#MAPE value is 1.512889 for UK dataset using naive method

#calculate RMSE:
residual<-UK_naive_09 - uk.actual
Q9naive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9naive_ukRMSE
#### RMSE for UK using naive method is 0.2577166 ############



##Seasonal naive method ####################################################
str(UK_09)

UK_snaive_pred09 <-snaive(UK_09,h=132)
write.csv(UK_snaive_pred09, "UK_09_pred_snaive.csv")

UK_snaive_09 <-read.csv(file.choose(), header=TRUE, sep = ",")
UK_snaive_09 <-select(UK_snaive_09,Point.Forecast)

percent.error.uk.snaive <-abs(UK_snaive_09 - uk.actual)/ UK_snaive_09
mean(percent.error.uk.snaive$Point.Forecast)*100 
##MAPE value is 1.57471 for UK using snaive method

#calculate RMSE:
residual<-UK_snaive_09 - uk.actual
Q9snaive_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9snaive_ukRMSE
#### RMSE for UK using snaive method is 0.2786768 ############



### Using ETS (ANN) Model##########################################
UK_temp_ANN_2009 <- ets(UK_09, model="ANN", damped=FALSE)
UK_temp_ANN_2009_pred <- forecast(UK_temp_ANN_2009, h=132, level=c(0.75, 0.90))

write.csv(UK_temp_ANN_2009_pred,"2009-2019_(ANN)_Predicted UK.csv")

predicted_uk09_ETS_ANN <- read.csv(file.choose())

plot(UK_temp_ANN_2009_pred, xlab="Year", ylab="Global Average Temperatures ANN 2009- 2019")

UK_pred_09_ETS_ANN <- select (predicted_uk09_ETS_ANN,Point.Forecast)

percent.error.uk.ets_ann <- abs(UK_pred_09_ETS_ANN - uk.actual)/UK_pred_09_ETS_ANN
percent.error.uk.ets_ann$Point.Forecast
mean(percent.error.uk.ets_ann$Point.Forecast)*100  
###  mape value is 1.170722 for UK using ETS (ANN) model

#calculate RMSE:
residual<-UK_pred_09_ETS_ANN - uk.actual
Q9ets_ann_ukRMSE<-sqrt(mean((residual$Point.Forecast)^2))
Q9ets_ann_ukRMSE
#### RMSE for UK using ets(ANN) method is 0.1952772  ############






######## Plots  Actual Temperature vs. Predicted temperature overview #############################
plot(NASA.actual$Temperature,ylab = "Global nasa temperatures", xlab = "Observations")
lines(NASA.predicted.09$Point.Forecast,col="Dark green") # tbats 2009 - 2019
lines(NASA.actual$Temperature,col="blue")
lines(NASA_naive_09$Point.Forecast,col="purple")
lines(NASA_snaive_09$Point.Forecast,col="orange")
lines(NASA_pred_09_ETS_ANN$Point.Forecast,col="pink")
legend("top", legend=c("TBATS","Actual","Naive","Snaive","ets(ANN)"), 
       col=c(" Dark green", "blue", "red","purple","orange","pink"), lty=1:6)


plot(actual.uk.data$Temperature, ylab = "Global UK temperature", xlab = "observations")
lines(predicted.ukdata$Point.Forecast,col="green") 
lines(actual.uk.data$Temperature,col="blue")
lines(UK_naive_09$Point.Forecast,col="orange")
lines(UK_snaive_09$Point.Forecast,col="purple")
lines(UK_pred_09_ETS_ANN$Point.Forecast,col="pink")

legend("top", legend=c("TBATS","Actual"," Naive","Snaive","ETS(ANN)"), 
       col=c("green", "blue", "red","orange","purple","pink"), lty=1:6)










