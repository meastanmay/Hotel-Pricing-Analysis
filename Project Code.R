# CAPSTONE PROJECT ANALYSIS
# NAME: TANMAY AGARWAL  
# COLLEGE: R.V. COLLEGE OF ENGINEERING
# EMAIL: tanmay151997@gmail.com 

# Setting working Directory
setwd("C:/Users/Tanmay/Desktop/Data Science and Analytics and Programming in R/Major Project")

# Reading Data set
caps<-read.csv(paste("Cities42.csv", sep = ""))

# Attching the data set to use the columns easily
attach(caps)

# Summary
summary(caps)

# Loading packages
library(psych)
library(car)
library(gplots)
library(gmodels)
library(lattice)
library(vcd)
library(Matrix)
library(corrgram)
library(corrplot)

# Desribing Data
describe(caps)

# Varible types
str(caps)

# View data set
View(caps)

# To organise the data having similar dates
caps$Date<-gsub("18-Dec-16","Dec 18 2016",caps$Date)
caps$Date<-gsub("21-Dec-16","Dec 21 2016",caps$Date)
caps$Date<-gsub("24-Dec-16","Dec 24 2016",caps$Date)
caps$Date<-gsub("25-Dec-16","Dec 25 2016",caps$Date)
caps$Date<-gsub("29-Dec-16","Dec 28 2016",caps$Date)
caps$Date<-gsub("31-Dec-16","Dec 31 2016",caps$Date)
caps$Date<-gsub("4-Jan-17","Jan 4 2017",caps$Date)
caps$Date<-gsub("8-Jan-17","Jan 8 2017",caps$Date)

# Checking the changes
table(caps$Date)

# Changing the dates to factors
caps$Date<-factor(caps$Date)
is.factor(caps$Date)

# Checking the labelling 
levels(caps$Date)

# To check the correlation between different variables of data set
corrgram(caps,order = TRUE,upper.panel = panel.pie,lower.panel = panel.shade,
         text.panel = panel.txt, main="Corrgram of the data of hotels in different cities")

# To check the correlation between the variables of the data set through corrplot
corrplot(corr = cor(caps[,c(1,(3:8),(11:13),15,17,(18:20))],use = "complete.obs"),method = "ellipse")


# ROOM RENT ANALYSIS
# We have found that Room rent of the hotels depend on city rank, tourist destination, Star rating, Hotel Capacity and swimming pool

# Corrgram for the variables correlated to room rent in the hotels

# Data frame of the required variables to analyze
rr<-data.frame(caps$RoomRent,caps$IsTouristDestination,caps$StarRating,caps$HotelCapacity,caps$HasSwimmingPool)
corrgram(rr,order = TRUE,upper.panel = panel.pie,lower.panel = panel.shade,text.panel = panel.txt,
         main="Corrgram for the variables correlated to Room Rent")

# We will analyze the distribution of data in Different variables

# Bar Plots
# Changing the labels 
tourist1<-factor(caps$IsTouristDestination, levels = c(0,1),labels = c("No","Yes"))

# Hotels in different cities
table1<-table(caps$CityName)
table1
barplot(table1,main =  " Number of Hotels in a city",xlab = "City",
        ylab = "Number Of hotels",
        col = "Turquoise2",ylim = c(0,2500))

# Visualtion of the number of tourist Destinations
table2<-table(tourist1)
table2
barplot(table2,main = "Tourist Destination Distribution",
        xlab = "Tourist Destination(Yes/No)",
        ylab = "Count",col = "Yellow4")

# Star Rating Distribution of the Hotels
table3<-table(caps$StarRating)
table3
barplot(table3,main = "Star Rating Distribution",xlab = "Star Ratings",ylab = "Count",
        col = "thistle4",ylim = c(0,6000))

# Visualizing Distribution of hotels having swimmimg pool
swim1<-factor(caps$HasSwimmingPool,levels = c(0,1),labels = c("No","Yes"))
table4<-table(swim1)
table4
barchart(table4,main="Distribution of Hotels On the basis of Swimming Pool Facility",
         ylab= "Swimming Pool (Yes/No)",
         xlab= "Count",
         col= "Slateblue")

# BOXPLOTS

# Star Rating
boxplot(caps$StarRating,main="Star Rating Analysis",horizontal = TRUE,xlab="Ratings",
        col = "wheat4")

boxplot(caps$StarRating~caps$CityName,main="Star Ratings Of Hotels at Different Places",
        xlab="Ratings",ylab="City",col= c("magenta","Yellow2","springgreen","slateblue4"),
        horizontal= TRUE)

# Hotel Capacity
boxplot(caps$HotelCapacity,main="Hotel Capacity Analysis",horizontal = TRUE,
        xlab="Capacity",col = "Orange")


boxplot(caps$HotelCapacity~caps$StarRating,horizontal= TRUE,
        main="Hotel Capacity Analysis based on Star Rating",
        xlab="Capacity",ylab="Star Ratings",
        col= c("wheat2","slateblue2","yellow2","tomato2","yellowgreen","thistle2"))
## The above boxplot shows that there are many outliers in the hotels having star rating 3 and 3.5

# Airport
boxplot(caps$Airport,main="Airport Analysis",horizontal = TRUE,
        xlab="Distance",col = "Saddlebrown")


# RoomRent Vs HasSwimmingPool
plot(jitter(caps$HasSwimmingPool),jitter(caps$RoomRent),main = "RoomRent Vs HasSwimmingPool",xlab = "Swimming Pool",
     ylab = "RoomRent",cex= 1.1)

# RoomRent VS HotelCapacity
scatterplot(caps$HotelCapacity,caps$RoomRent,main="Room Rent Vs Hotel Capacity",
            xlab="Hotel Capacity",
            ylab="Room Rent",cex=1.1,pch = 19)
# There are many outliers in the Hotel capacity

# RoomRent Vs StarRating
scatterplot(caps$StarRating,caps$RoomRent,main="Room Rent Vs Star Rating",
            xlab = "Star Rating",
            ylab = "Room Rent",
            cex = 1.1,pch = 19)

# RoomRent Vs X
scatterplot(caps$X,caps$RoomRent,main="Room Rent Vs X",
            xlab = "X",
            ylab = "Room Rent",
            cex = 1.1,pch = 19)

# RoomRent Vs City Rank
scatterplot(caps$CityRank,caps$RoomRent,main="Room Rent Vs City Rank",
            xlab = "City Rank",
            ylab = "Room Rent",
            cex = 1.1,pch = 19)

# RoomRent Vs Tourist Destination
plot(jitter(caps$IsTouristDestination),jitter(caps$RoomRent),
     main ="Room Rent Vs Tourist Destination ",
     xlab = "Tourist Destination",
     ylab = "Room Rent",
     cex=1.1,pch= 19,col="Blue")

# Scatterplot Matrix
scatterplotMatrix(~RoomRent+CityName+HasSwimmingPool,data = caps,diagonal= "histogram")

# Density Plots
densityplot(caps$Population,main="Population Density Analysis",xlab="Population")

# Variance-covariance matrix

# Correlation Matrix
x<-caps[,c("HasSwimmingPool","HotelCapacity","StarRating","IsTouristDestination")]
y<-caps[,c("RoomRent")]

cor(x,y)

cov(x,y)

var(x,y)


# Multivariable Regression Model 

# Regression Model for Room Rent

#Model1
model1<-RoomRent~Population+CityRank+IsMetroCity+IsWeekend+IsNewYearEve+Date+FreeBreakfast+FreeWifi+HasSwimmingPool+StarRating+IsTouristDestination+Airport+HotelCapacity
fit1<-lm(model1,data=caps)

#Summary
summary(fit1)

# Checking the variables to determine the best fit model for determining the room Rent in hotels

# Loadind Leaps package
library(leaps)
leap1<-regsubsets(model1,data = caps,nbest = 1,really.big = T)
# Visualizing it through graph
plot(leap1,scale = "adjr2")

## From the plot it is clear that the Room Rent is depended upon Star Ratings, Availabilty of Swimming Pool,if the Destination is a Tourist Place or not, Hotel Capacity,and some famous places like Delhi,Jodhpur,Udaipur and Varanasi

# REVISED MODEL TAKING THE BEST VARIABLES
model2<-RoomRent~Population+CityRank+IsNewYearEve+HasSwimmingPool+StarRating+IsTouristDestination+Airport+HotelCapacity
fit2<-lm(model2,data = caps)
summary(fit2)

# MODEL WITH THE THREE VARIABLES WHICH AFFECTS THE ROOM RENT THE MOST
model3<-RoomRent~StarRating+HasSwimmingPool+HotelCapacity
fit3<-lm(model3,data = caps)
summary(fit3)

# Coeffecient Plot
# Loading Package ggplot2 and coefplot
library(ggplot2)
library(coefplot)

coefplot(fit3,intercept= FALSE,outerCI=1.96,
         coefficients= c("StarRating","HasSwimmingPool","HotelCapacity"))

# Model:b0+b1*StarRating+b2*HasSwimmingPool+b3*HotelCapacity
# b0 = -6896.154, b1 = 3597.322, b2 = 2528.885,b3 = -15.558
# Final Expression:-6896.154+3597.322*StarRating+2528.885*HasSwimmingPool+-15.558*HotelCapacity


# ANALYSIS OF STAR RATING OF HOTELS

# Density Plots
densityplot(caps$Population,main="Population Density Analysis",xlab="Population")

# Changing the labels 
Metro1<-factor(caps$IsMetroCity,levels = c(0,1),labels = c("Not Metro City","Metro City"))
# BOXPLOTS

# Population of Tourist and Non Tourist Places and  Segregation if it is Metro City
bwplot(tourist1~Population|Metro1,data = caps,xlab = "Population",
       ylab = "Tourist Destination",horizontal = TRUE,main="Population Analysis")

# Room Rent Of Hotels dividing on the basis of Tourist Destination and Metro Cities
bwplot(tourist1~RoomRent|Metro1,data = caps,xlab = "Room Rent",ylab = "Tourist Destination",
       main="Room Rent Analysis",horizontal = TRUE)

# Hotel Capacity dividing on the basis of Tourist Destination and Metro Cities
bwplot(tourist1~HotelCapacity|Metro1,xlab = "Hotel Capacity",ylab = "Tourist Destination",
       main="Hotel Capacity Analysis",horizontal = TRUE)

# ScatterPlots

# Star Rating Vs Population
scatterplot(caps$Population,caps$StarRating,main="Star Rating VS Population",
            xlab = "Population",ylab = "Star Rating",cex = 1.1,pch = 18)

# Star Rating Vs IsMetroCity
plot(jitter(caps$IsMetroCity),jitter(caps$StarRating),
     main = "Star Rating in Metro and Non Metro Cities",xlab = "Metro city(Yes/No)",
     ylab = "Star Rating",cex= 0.5,pch= 19)

# Star Rating Vs Room Rent
scatterplot(caps$RoomRent,caps$StarRating,main="Star Rating Vs Room Rent",
            xlab = "Room Rent",ylab = "Star Rating",cex = 1.0,pch = 19)

# Star Rating Vs Hotel Capacity
scatterplot(caps$HotelCapacity,caps$StarRating,main="Star Rating Vs Hotel Capacity",
            xlab = "Hotel Capacity",ylab = "Star Rating",cex = 1.0,pch = 19)

# Star Rating Vs Availability Of Swimming Pool
plot(jitter(caps$HasSwimmingPool),jitter(caps$StarRating),
     main = "Star Rating Vs Swimming Pool Availability",xlab = "Swimming Pool(Yes/No)",
     ylab = "Star Rating",cex= 0.5,pch= 19)

# Star Rating Vs City Rank
scatterplot(caps$CityRank,caps$StarRating,main="Star Rating Vs City Rank",xlab= "City Rank",
            ylab = "Star Rating",cex = 1.0,pch = 19)

# T-Test

# Test 1
table6<-table(caps$IsMetroCity,caps$StarRating)
table6
t.test(caps$StarRating,caps$IsMetroCity,var.equal = TRUE)
# The above Test Shows that p-value<0.05.Thus it fails the null hypothesis and the two variables are dependent on each other

# Test 2: Population and Metro city 
t.test(Population~IsMetroCity,var.equal= TRUE)
# p-value<0.05
# Fails Null Hypothesis.Thus,the two variables are related to each other

# Test 3: Star Rating and Hotel Capacity
t.test(caps$StarRating,caps$HotelCapacity,var.equal = TRUE)
# p-value<0.05
# Fails Null Hypothesis. Thus, the two variables are dependent on each other

# Test 4: Star Rating and Room Rent
t.test(caps$StarRating,caps$RoomRent,var.equal = TRUE)
# p-value<0.05
# Fails Null Hypothesis.Thus,the two variables are dependent on each other

# Test 5: Star Rating and Availability of Swimming Pool
t.test(caps$StarRating,caps$HasSwimmingPool,var.equal = TRUE)
# p-value<0.05
# Fails Null Hypothesis.Thus,the two variables are dependent on each other

# Multi variable Regression model for determining Star Rating of a Hotel
model3<-StarRating~Population+CityRank+IsMetroCity+IsTouristDestination+IsWeekend+IsNewYearEve+Date+
RoomRent+Airport+HotelPincode+FreeWifi+FreeBreakfast+HotelCapacity+HasSwimmingPool
fit3<-lm(model3,data = caps)
summary(fit3)

# Analyzing the best fit variables to determine the star Rating of a Hotel
leap2<-regsubsets(model3,data=caps,nbest = 1,really.big = T)
plot(leap2,scale = "adjr2")

# Best Fit model for determining Star Rating of hotel
model4<-StarRating~RoomRent+Airport+FreeWifi+HotelCapacity+HasSwimmingPool
fit4<-lm(model4,data = caps)
summary(fit4)
# Model:b0+b1*RoomRent+b2*Airport+b3*FreeWifi+b4*HotelCapacity+b5*HasSwimmingPool
# b0 = 2.780e+00, b1 = 1.983e-05, b2 = 1.076e-03, b3 = 9.348e-02, b4 = 4.317e-03, b5 = 5.384e-01

# Coeffecient plot of the Regression Model of Star Rating Of the Hotels
coefplot(fit4,intercept=FALSE,OuterCI=1.96,
         coefficients= c("CityName","RoomRent","HotelCapacity","HasSwimmingPool","FreeWifi","FreeBreakfast"))

# Analysis Of City Rank

# Scatterplots

# City Rank Vs Tourist Destination
plot(jitter(caps$IsTouristDestination),jitter(caps$CityRank),main = "City Rank Vs Tourist Destination",
     xlab = "Tourist Destination",ylab = "City Rank",cex= 0.5)

# City Rank Vs Airport
scatterplot(caps$Airport,caps$CityRank,main="City Rank Vs Airport Distance",
            xlab = "Airport Distance",ylab = "City Rank",cex = 1.1,pch = 19)

# City Rank Vs Population
scatterplot(caps$Population,caps$CityRank,main="City Rank Vs Population of the City",
            xlab = "Population",ylab = "City Rank",cex = 1.1,pch = 19)

# City Rank Vs Hotel Capacity
scatterplot(caps$HotelCapacity,caps$CityRank,main="City Rank Vs Hotel Capacity",
            xlab="Hotel Capacity",ylab = "City Rank",cex = 1.1,pch = 19)

# City Rank Vs Room Rent
scatterplot(caps$RoomRent,caps$CityRank,main="City Rank Vs Room Rent",
            xlab="Room Rent",ylab="City Rank",cex = 1.1,pch = 19)

# T-Test

# Test1: City Rank Vs Free breakfast
t.test(CityRank~FreeBreakfast,var.equal= TRUE)
# p>0.05
# The test doesn't fails Null Hypothesis. Thus,the two variables are independent of each other.

# Test2: City Rank Vs Free Wifi
t.test(CityRank~IsTouristDestination,var.equal= TRUE)
# p-value<0.05
# Fails Null Hypothesis.Thus,the two variables are dependent on each other

# Test3: City Rank Vs IsWeekend
t.test(CityRank~IsWeekend,var.equal=TRUE)
# p-value>0.05
# Test doesn't fails Null Hypothesis.Thus,the two variables are independent of each other.

# Multi variable regression model for determining City Rank
model5<-CityRank~Population+IsMetroCity+IsTouristDestination+IsWeekend+IsNewYearEve+Date+
  RoomRent+Airport+HotelPincode+FreeWifi+FreeBreakfast+HotelCapacity+HasSwimmingPool+StarRating
fit5<-lm(model5,data = caps)
summary(fit5)

# Determining Best Fit Regression Model for determing City Rank
leap3<-regsubsets(model5,data = caps,nbest = 1,really.big = T)
plot(leap3,scale = "adjr2")

# Fit Regression Model for determing City Rank
model6<-CityRank~Population+Airport+IsMetroCity+IsTouristDestination
fit6<-lm(model6,data = caps)
summary(fit6)
# Model:b0+b1*Population+b2*Airport+b3*IsMetroCity+b4*IsTouristDestination
# b0 = 1.878e+01, b1 =-2676e-06 , b2 = 1.653e-01, b3 = 3.431e+00,b4 = 4.874e+00
# Final Expression:1.878e+01+-2676e-06*Population+1.653e-01*Airport+3.431e+00*IsMetroCity+4.874e+00*IsTouristDestination

# Coefficient plot
coefplot(fit6,outerCI = 1.6,
         coefficients = c("Population","Airport","IsMetroCity","IsTouristDestination"))


# Analysis Of Hotel Capacity

# Scatterplots

# Hotel Capacity Vs Population
scatterplot(caps$Population,caps$HotelCapacity,main="Hotel Capacity Vs Population",
            xlab="Population",ylab = "Hotel Capacity",cex = 1.0,pch = 19)

# Scatterplot Matrix
scatterplotMatrix(~HotelCapacity+StarRating+RoomRent+HasSwimmingPool,data = caps,diagonal="histogram")

# Multi Variable model for Determining the Hotel Capacity of the Hotels in a city

model7<-HotelCapacity~CityRank+IsMetroCity+IsTouristDestination+IsWeekend+IsNewYearEve+Date+
  RoomRent+Airport+HotelPincode+FreeWifi+FreeBreakfast+Population+HasSwimmingPool+StarRating
fit7<-lm(model7,data = caps)
summary(fit7)

# Determining the best fit model for estimating Hotel Capacity
leap4<-regsubsets(model7,data = caps,nbest = 1,really.big = T)
plot(leap4,scale = "adjr2")

# Best fit model to determine the Hotel Capacity of the Hotel in a city
model8<-HotelCapacity~Population+IsMetroCity+IsTouristDestination+RoomRent*StarRating+HasSwimmingPool*StarRating+StarRating
fit8<-lm(model8,data = caps)
summary(fit8)

# Model:Hotel Capacity=b0+b1*Population+b2*IsMetroCity+b3*IsTouristDestination+b4*RoomRent*StarRating+b5*HasSwimmingPool*StarRating+b6*StarRating

# Coeffecient Plot
coefplot(fit8,intercept= FALSE,OuterCI=1.6,
         coeffecients= c("Population","IsMetroCity","IsTouristDestination","RoomRent*StarRating","HasSwimmingPool*StarRating","StarRating"))
