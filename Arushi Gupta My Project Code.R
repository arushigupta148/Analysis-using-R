# Hotel Room Pricing In The Indian Market
# NAME: ARUSHI GUPTA
# EMAIL: arushigupta148@gmail.com
# COLLEGE: Manipal Institute of Technology


#1) Read the data
All<-read.csv(paste("All.csv",sep=""))

All$Year <- substr(c(All$Date),nchar(c(All$Date))-2,nchar(c(All$Date)))
mylist <- list()
for(date in All$Date)
{
  mylist[length(mylist)+1] <- substr(date,nchar(date)-1,nchar(date))
}

All$Year <- as.numeric(mylist)
mylist <- NULL

#2) View the data frame
View(All)

#3) Summarize the data
library(psych)
View(describe(All))


#Find top 3 independent variables for Agra
#Perform chi-square test for that
agra <- All[All$CityName=="Agra",]


#4) Contingency tables

#Hypothesis 1: The roomrent is independent of the weekend
mytable <- xtabs(~IsWeekend+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value= 0.9998, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 2: The roomrent is independent of new years eve
mytable <- xtabs(~IsNewYearEve+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.8276, Do not reject null hypothesis
chisq.test(mytable)

#Hypothesis 3: The roomrent is independent of the star rating
mytable <- xtabs(~StarRating+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=2.2e-16, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 4: The roomrent is independent of the distance from the airport
mytable <- xtabs(~Airport+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=2.2e-16, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 5: The roomrent is independent of the service free wifi
mytable <- xtabs(~FreeWifi+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=7.42e-13, Reject null hypothesis
chisq.test(mytable)


#Hypothesis 6: The roomrent is independent of the service free breakfast
mytable <- xtabs(~FreeBreakfast+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=2.2e-16, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 7: The roomrent is independent of the hotel capacity
mytable <- xtabs(~HotelCapacity+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=2.2e-16, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 8: The roomrent is independent of the swimming pool facility
mytable <- xtabs(~HasSwimmingPool+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=2.2e-16, Reject null hypothesis
chisq.test(mytable)

#Hypothesis 9: The roomrent is independent of the date
mytable <- xtabs(~Year+RoomRent,data=agra)
addmargins(mytable)

#Chi square test for the above table
#p-value=0.03979, Do not reject null hypothesis
chisq.test(mytable)


#From above results, the top 3 independent factors are IsWeekend, IsNewYear and Year
#Visualize these 3 with respect to RoomRent
table(All$IsWeekend)
table(All$IsNewYearEve)
table(All$Year)

library(car)
scatterplot(jitter(All$IsWeekend), jitter(All$RoomRent))
scatterplot(jitter(All$IsNewYearEve), jitter(All$RoomRent))
scatterplot(jitter(All$Year), jitter(All$RoomRent))


#Create a dataframe with only the 4 required variables
extra <- data.frame(All)
extra <- extra[,c(6,7,10,20)]
#extra <- extra[,c(1,2,4,3)]
View(extra)

#Corrgram
library(corrgram)
corrgram(extra,upper.panel=panel.cor)

#Compute Variance and Covariance matrices
var(extra)
cov(extra)

#Hypothesis 1: Room Rents are equal on weekends and weekdays
t.test(All$RoomRent~All$IsWeekend)
#p-value = 0.6041, do not reject null hypothesis

#Hypothesis 2: Room Rents are same on new years and other days of the year
t.test(All$RoomRent~All$IsNewYearEve)
#p-value = 3.046e-05, Reject null hypothesis

#Hypothesis 2: Room Rents are same in 2016 and 2017
t.test(All$RoomRent~All$Year)
#p-value = 0.6302, do not reject null hypothesis

price_predict <- lm(RoomRent~Population+CityRank+IsMetroCity+IsTouristDestination+IsWeekend
                    +IsNewYearEve+StarRating+Airport+FreeWifi+FreeBreakfast+HotelCapacity
                    +HasSwimmingPool+Year, data=All)

coefficients(price_predict)
summary(price_predict)