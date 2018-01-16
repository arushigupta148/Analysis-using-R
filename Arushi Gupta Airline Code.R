# Analysis of Airline Ticket Pricing
# NAME: ARUSHI GUPTA
# EMAIL: arushigupta148@gmail.com
# COLLEGE: Manipal Institute of Technology


#1) Read the data
airlines <- read.csv(paste("SixAirlines.csv", sep=""))

#2) View the data frame
View(airlines)

#3) Merging datasets of week 4 day 1 and day 2
airlines$N <- NULL
airlines$LAMBDA <- NULL
airlines$QUALITY <- NULL
airlines$SEATS_TOTAL <- airlines$SEATS_ECONOMY+airlines$SEATS_PREMIUM
airlines$FractionPremiumSeats <-airlines$SEATS_PREMIUM/airlines$SEATS_TOTAL
airlines$PitchDifference<-airlines$PITCH_PREMIUM-airlines$PITCH_ECONOMY
airlines$WidthDifference <- airlines$WIDTH_PREMIUM - airlines$WIDTH_ECONOMY

#3) Summarize the data
library(psych)
View(describe(airlines))

#4) Boxplots to show the distribution of all variables independently
library(car)
Boxplot(airlines$AIRCRAFT)
Boxplot(airlines$MONTH)
Boxplot(airlines$INTERNATIONAL)
Boxplot(airlines$FLIGHT_DURATION)
Boxplot(airlines$SEATS_ECONOMY)
Boxplot(airlines$SEATS_PREMIUM)
Boxplot(airlines$PITCH_ECONOMY)
Boxplot(airlines$PITCH_PREMIUM)
Boxplot(airlines$WIDTH_ECONOMY)
Boxplot(airlines$WIDTH_PREMIUM)
Boxplot(airlines$PRICE_ECONOMY)
Boxplot(airlines$PRICE_PREMIUM)
Boxplot(airlines$PRICE_RELATIVE)
Boxplot(airlines$SEATS_TOTAL)
Boxplot(airlines$FractionPremiumSeats)
Boxplot(airlines$PitchDifference)
Boxplot(airlines$WidthDifference)

#5) Correlation between each value using corrgram
library(corrgram)
air1 <- data.frame(airlines)
air1$AIRLINE <- NULL
air1$PRICE_RELATIVE <- NULL
air1$WidthDifference <- NULL
air1$FractionPremiumSeats <- NULL
air1$SEATS_TOTAL <- NULL
air1$PitchDifference <- NULL

correl <- cor(air1)
corrgram(air1, upper.panel = panel.pie)

#6)	Scatter Plots to understand how the are variables correlated pair-wise

scatterplot(air1$FLIGHT_DURATION,air1$WIDTH_ECONOMY) 
#Most flights have medium seats (18), large seats in longer flights
#Small seats in mostly short flights

scatterplot(air1$FLIGHT_DURATION,air1$WIDTH_PREMIUM) 
#Most flights have the same seat size(19) and does not depend much on flight duration

scatterplot(air1$FLIGHT_DURATION,air1$PRICE_ECONOMY)
#Shorter flights have cheaper economy tickets
#Long flights have cheap and expensive tickets

scatterplot(air1$FLIGHT_DURATION,air1$PRICE_PREMIUM)
#price of premium tickets increases gradually as flight time increases

scatterplot(jitter(air1$PITCH_PREMIUM)~air1$INTERNATIONAL)
#Higher pitch seen in international flights and not in Domestic

scatterplot(jitter(air1$WIDTH_PREMIUM)~air1$INTERNATIONAL)
#Higher Width seen in international flights and not in Domestic

scatterplot(jitter(air1$PRICE_PREMIUM)~air1$INTERNATIONAL)
#Higher premium economy price in international flights

scatterplot(jitter(air1$PRICE_ECONOMY)~air1$INTERNATIONAL)
#Higher economy price in international flights

scatterplot(jitter(air1$SEATS_ECONOMY)~jitter(air1$SEATS_PREMIUM))
#An almost linear relation betwen the number of seats

scatterplot(jitter(air1$SEATS_ECONOMY)~jitter(air1$WIDTH_ECONOMY))
#Higher the width, number of seat are still relatively high

scatterplot(jitter(air1$SEATS_PREMIUM)~jitter(air1$WIDTH_ECONOMY))
#Number of seats in premium are the highest when the economy seats width is 18

scatterplot(jitter(air1$PITCH_PREMIUM)~jitter(air1$PITCH_ECONOMY))
#Pitch of economy and premium are highly negatively correlated

scatterplot(jitter(air1$WIDTH_ECONOMY)~jitter(air1$PITCH_ECONOMY))
#When Pitch of Economy is high, Width of Economy is high too

scatterplot(jitter(air1$PITCH_ECONOMY)~jitter(air1$WIDTH_PREMIUM))
#WHen width of premium is the highest, width of economy is the least

scatterplot(jitter(air1$PITCH_ECONOMY)~jitter(air1$PRICE_ECONOMY))
#As pitch of economy increases, the price of economy seats also increase

scatterplot(jitter(air1$PITCH_PREMIUM)~jitter(air1$WIDTH_PREMIUM))
#Very correlated, pitch of premium increases with width of premium

scatterplot(jitter(air1$PRICE_ECONOMY)~jitter(air1$PRICE_PREMIUM))
#Very highly correlated, price of premium increases with price of economy

#7) Create Variance and Covariance matrices of the dataset
var(air1)
cov(air1)


#8) Hypothesis to be tested using the regression model

#Hypothesis 1: Domestic flights have the same prices for seats as International flights
t.test(air1$PRICE_ECONOMY~air1$INTERNATIONAL)
t.test(air1$PRICE_PREMIUM~air1$INTERNATIONAL)
#Both have very low p-values (2.2e-16)

#Hypothesis 2: Price of economy influences price of premium
t.test(air1$PRICE_PREMIUM, air1$PRICE_ECONOMY)
#False, low p-value

#Hypothesis 3: Pitch of the Seat determines the price of the seat
t.test(air1$PITCH_ECONOMY,air1$PRICE_ECONOMY)
t.test(air1$PITCH_PREMIUM,air1$PRICE_PREMIUM)
#False

#Hypothesis 4: Width of the seat determines price of the seat
t.test(air1$WIDTH_ECONOMY,air1$PRICE_ECONOMY)
t.test(air1$WIDTH_PREMIUM,air1$PRICE_PREMIUM)
#False

#Hypothesis 5: Number of seats in the class determines the price of the class
t.test(air1$SEATS_ECONOMY,air1$PRICE_ECONOMY)
t.test(air1$SEATS_PREMIUM,air1$PRICE_PREMIUM)
#False

#Hypothesis 6: Flight Duration determines the price of the seat
t.test(air1$FLIGHT_DURATION,air1$PRICE_ECONOMY)
t.test(air1$FLIGHT_DURATION,air1$PRICE_PREMIUM)
#False

#9) Regression model

lin_model_eco <- lm(PRICE_ECONOMY~AIRCRAFT+FLIGHT_DURATION+INTERNATIONAL+MONTH
+                     +SEATS_ECONOMY+SEATS_PREMIUM+PITCH_ECONOMY+PITCH_PREMIUM
+                     +WIDTH_ECONOMY+WIDTH_PREMIUM, data=airlines)
> coefficients(lin_model_eco)

lin_model_pre <- lm(PRICE_PREMIUM~AIRCRAFT+FLIGHT_DURATION+INTERNATIONAL+MONTH
+                     +SEATS_ECONOMY+SEATS_PREMIUM+PITCH_ECONOMY+PITCH_PREMIUM
+                     +WIDTH_ECONOMY+WIDTH_PREMIUM, data=airlines)
> coefficients(lin_model_pre)







