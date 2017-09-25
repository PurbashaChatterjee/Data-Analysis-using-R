# Analysis of Airline Ticket Pricing
# NAME: Purbasha Chatterjee
# EMAIL: pchatterjee10@gmail.com
# COLLEGE : Oregon State University, Corvallis


setwd('C:/Users/purba/Desktop/R')

airlines.df <- read.csv(paste("SixAirlines.csv", sep=""))
summary(airlines.df)

model <- lm(airlines.df$PRICE_PREMIUM~., data = airlines.df)
summary(model)

str(airlines.df)

#Creating bar graphs with respect to different variables to analyze
counts <- table(airlines.df$PRICE_ECONOMY)
barplot(counts, main = "PRICE ECONOMY Distribution")
counts <- table(airlines.df$PRICE_PREMIUM)
barplot(counts, main = "PRICE PREMIUM Distribution")
counts <- table(airlines.df$PRICE_RELATIVE)
barplot(counts, main = "PRICE RELATIVE Distribution")
counts <- table(airlines.df$AIRLINE)
barplot(counts, main = "AIRLINE Distribution")
counts <- table(airlines.df$AIRCRAFT)
barplot(counts, main = "AIRCRAFT Distribution")
counts <- table(airlines.df$FLIGHT_DURATION)
barplot(counts, main = "FLIGHT DURATION Distribution")
counts <- table(airlines.df$MONTH)
barplot(counts, main = "MONTH Distribution")
counts <- table(airlines.df$INTERNATIONAL)
barplot(counts, main = "INTERNATIONAL Distribution")
counts <- table(airlines.df$SEATS_ECONOMY)
barplot(counts, main = "SEATS ECONOMY Distribution")

#Boxplot to analyze further
library(car)
par(mfrow=c(2, 2))
with(airlines.df, scatterplot(airlines.df$AIRLINE , airlines.df$FLIGHT_DURATION))
with(airlines.df, scatterplot(airlines.df$AIRLINE , airlines.df$PRICE_RELATIVE))
with(airlines.df, scatterplot(airlines.df$AIRLINE , airlines.df$SEATS_PREMIUM))
with(airlines.df, scatterplot(airlines.df$AIRLINE , airlines.df$SEATS_ECONOMY))

#Renaming airlines
airlines.df$airname <- ifelse(airlines.df$AIRLINE=="AirFrance",1,
                             (ifelse(airlines.df$AIRLINE=="British",2,
                                     (ifelse(airlines.df$AIRLINE=="Delta",3,
                                             (ifelse(airlines.df$AIRLINE=="Jet",4,
                                                     (ifelse(airlines.df$AIRLINE=="Singapore",5,
                                                             (ifelse(airlines.df$AIRLINE=="Virgin",6, 0
                                                             ))
                                                     ))))
                                             )))))

#To analyze the airline with maximum number of seats
library(ggplot2)
ggplot(airlines.df, aes(x = AIRLINE, fill = AIRLINE)) + geom_bar()

library(car)
scatterplotMatrix(formula=~airlines.df$PRICE_ECONOMY+airlines.df$SEATS_ECONOMY,cex=0.6,diagonal="histogram")
scatterplotMatrix(formula=~airlines.df$PRICE_PREMIUM+airlines.df$SEATS_PREMIUM,cex=0.6,diagonal="histogram")
scatterplotMatrix(formula=~airlines.df$SEATS_PREMIUM+airlines.df$SEATS_ECONOMY,cex=0.6,diagonal="histogram")

#To analyze the correlation
library(corrgram)
corrgram(airlines.df, main="Corrgram of Airline Variables", lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel=panel.txt)

cor.test(airlines.df$PITCH_ECONOMY,airlines.df$PRICE_ECONOMY)
cor.test(airlines.df$PITCH_PREMIUM,airlines.df$PRICE_PREMIUM)

#T-test to analyze the statistical significance of the variables with each other
t.test(airlines.df$airname, airlines.df$QUALITY)
t.test(airlines.df$airname, airlines.df$PRICE_ECONOMY)
t.test(airlines.df$airname, airlines.df$PRICE_PREMIUM)
t.test(airlines.df$SEATS_ECONOMY, airlines.df$PRICE_ECONOMY)
t.test(airlines.df$SEATS_PREMIUM, airlines.df$PRICE_PREMIUM)

#Applying linear regression to classify the dataset
model1 <- lm(airlines.df$SEATS_ECONOMY~airlines.df$WIDTH_ECONOMY+airlines.df$PRICE_ECONOMY+airlines.df$PITCH_ECONOMY, data = airlines.df)
summary(model1)

model2 <- lm(airlines.df$SEATS_PREMIUM~airlines.df$WIDTH_PREMIUM+airlines.df$PRICE_PREMIUM+airlines.df$PITCH_PREMIUM, data = airlines.df)
summary(model2)
