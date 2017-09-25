# Analysis of Hotel Pricing
# NAME: Purbasha Chatterjee
# EMAIL: pchatterjee10@gmail.com
# COLLEGE : Oregon State University, Corvallis

setwd('C:/Users/purba/Desktop/R')
hotels.df <- read.csv(paste("Cities42.csv", sep=""))
summary(hotels.df)

library(psych)
describe(hotels.df)

hotels.df$Date <- gsub("18-Dec- 16", "Dec 18 2016", hotels.df$Date)
hotels.df$Date <- gsub("21-Dec- 16", "Dec 21 2016", hotels.df$Date)
hotels.df$Date <- gsub("24-Dec- 16", "Dec 24 2016", hotels.df$Date)
hotels.df$Date <- gsub("25-Dec- 16", "Dec 25 2016", hotels.df$Date)
hotels.df$Date <- gsub("28-Dec- 16", "Dec 28 2016", hotels.df$Date)
hotels.df$Date <- gsub("31-Dec- 16", "Dec 31 2016", hotels.df$Date)
hotels.df$Date <- gsub("4-Jan- 16", "Jan 04 2016", hotels.df$Date)
hotels.df$Date <- gsub("4-Jan- 17", "Jan 04 2017", hotels.df$Date)
hotels.df$Date <- gsub("8-Jan- 16", "Jan 08 2016", hotels.df$Date)
hotels.df$Date <- gsub("8-Jan- 17", "Jan 08 2017", hotels.df$Date)
hotels.df$Date <- gsub("Jan 4 17", "Jan 04 2017", hotels.df$Date)
hotels.df$Date <- gsub("Jan 8 17", "Jan 08 2017", hotels.df$Date)

library(corrgram)
corrgram(hotels.df, main="Corrgram of Hotel Variables", lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel=panel.txt)

### As per the corrgram, I listed out room rent, star rating, Hotel Capacity 
### and Has Swimmming Pool shares stron correlation among each other.
### Hence, we can consider star rating has the dependent variable
### and room rent, Hotel Capacity 
### and Has Swimmming Pool as independent variables
### Following analysis has been done for the same.

new_table <- data.frame(hotels.df$RoomRent,hotels.df$HotelCapacity,hotels.df$HasSwimmingPool, hotels.df$StarRating)
View(new_table)
roomrent_table <- table(hotels.df$RoomRent)
View(roomrent_table)

library(car)
scatterplot(hotels.df$StarRating, hotels.df$RoomRent)
scatterplot(hotels.df$HotelCapacity, hotels.df$RoomRent)
scatterplot(hotels.df$HasSwimmingPool, hotels.df$RoomRent)

library(corrgram)
corrgram(new_table, main="Corrgram of Hotel Variables", lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel=panel.txt)

var(hotels.df$StarRating, hotels.df$RoomRent)
var(hotels.df$HotelCapacity, hotels.df$RoomRent)
var(hotels.df$HasSwimmingPool, hotels.df$RoomRent)

##Hypothesis is average room rent is with higher star 
##rating hotels is more than the ones with low star rating
t.test(hotels.df$RoomRent, hotels.df$StarRating)

##Hypothesis is tourist destination has more population 
t.test(hotels.df$IsTouristDestination, hotels.df$Population)

##Hypothesis is higher city rank cities have more number of star rating hotels
t.test(hotels.df$StarRating, hotels.df$CityRank)

##Hypothesis average hotel capacity is always more for star rating hotels
t.test(hotels.df$HotelCapacity, hotels.df$StarRating)

##Hotels closer to airport has higher star ratings
t.test(hotels.df$Airport, hotels.df$StarRating)

##Hotels closer to airport has higher room rent
t.test(hotels.df$Airport, hotels.df$RoomRent)

###Comparing with other variables
counts <- table(hotels.df$StarRating)
barplot(counts,hotels.df$StarRating, data = hotels.df )

## This gives the number of hotels in each city
hist(hotels.df$CityRank, main="Histogram for City Rank",xlab="City Rank", 
     border="blue",col="light blue",
     breaks=5)

boxplot(hotels.df$RoomRent~hotels.df$StarRating,data=hotels.df, 
             main="Room Rent Vs Star Rating", 
              xlab="Start Rating", ylab="Room Rent")
boxplot(hotels.df$HasSwimmingPool~hotels.df$StarRating,data=hotels.df, 
             main="Has Swimming Pool Vs Star Rating", 
             xlab="Star Rating", ylab="Has Swimming Pool")
boxplot(hotels.df$Airport~hotels.df$StarRating,data=hotels.df, 
             main="Airport Vs StarRating", 
             xlab="StarRating", ylab="Airport")
boxplot(hotels.df$HotelCapacity~hotels.df$StarRating,data=hotels.df, 
             main="Hotel Capacity Vs Star Rating", 
             xlab="Star Rating", ylab="Hotel Capacity")
boxplot(hotels.df$RoomRent~hotels.df$IsWeekend,data=hotels.df, 
             main="IsWeekend Vs Room Rent", 
             xlab="IsWeekend", ylab="Room Rent")
boxplot(hotels.df$RoomRent~hotels.df$IsTouristDestination,data=hotels.df, 
        main="IsTouristDestination Vs Room Rent", 
        xlab="IsTouristDestination", ylab="Room Rent")
## Regression Model
model1 <- lm(hotels.df$RoomRent~hotels.df$StarRating+hotels.df$HotelCapacity+hotels.df$HasSwimmingPool, data = hotels.df)
summary(model1)

coefficients(model1)
residuals(model1)

model2 <- lm(hotels.df$RoomRent~hotels.df$StarRating+hotels.df$HotelCapacity
             +hotels.df$HasSwimmingPool+hotels.df$Airport, data = hotels.df)
summary(model2)
coefficients(model2)
residuals(model2)

model3 <- lm(hotels.df$RoomRent~hotels.df$StarRating+hotels.df$HotelCapacity
             +hotels.df$HasSwimmingPool+hotels.df$Airport
             +hotels.df$FreeWifi+hotels.df$FreeBreakfast, data = hotels.df)
summary(model3)
coefficients(model3)
residuals(model3)
RoomRent <- str(hotels.df$RoomRent)
StarRating <- str(hotels.df$StarRating)
HotelCapacity <- str(hotels.df$HotelCapacity)
HasSwimmingPool <- str(hotels.df$HasSwimmingPool)

## Applying Random Forest Regression
library(randomForest)  
rf1 = randomForest(hotels.df$RoomRent~hotels.df$StarRating+hotels.df$HotelCapacity+hotels.df$HasSwimmingPool, data = hotels.df)
summary(rf1)

rf2 = randomForest(hotels.df$RoomRent~hotels.df$StarRating+
                     hotels.df$HotelCapacity+hotels.df$HasSwimmingPool
                   +hotels.df$Airport, data = hotels.df)
summary(rf2)

rf3 = randomForest(hotels.df$RoomRent~hotels.df$StarRating+
                     hotels.df$HotelCapacity+hotels.df$HasSwimmingPool
                   +hotels.df$Airport
                   +hotels.df$FreeWifi+hotels.df$FreeBreakfast, data = hotels.df)
summary(rf3)
coefficients(rf3)