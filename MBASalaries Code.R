# Analysis of Airline Ticket Pricing
# NAME: Purbasha Chatterjee
# EMAIL: pchatterjee10@gmail.com
# COLLEGE : Oregon State University, Corvallis

setwd('C:/Users/purba/Desktop/R')

mba_Sal.df <- read.csv(paste("MBA Starting Salaries Data.csv", sep=""))
summary(mba_Sal.df)

#Analyzing the distribution of different variables by barplot
counts <- table(mba_Sal.df$gmat_tot)
barplot(counts, main = "GMAT TOTAL Distribution")
counts <- table(mba_Sal.df$salary)
barplot(counts, main = "SALARY Distribution")
counts <- table(mba_Sal.df$work_yrs)
barplot(counts, main = "WORK EXPERIENCE Distribution")
counts <- table(mba_Sal.df$age)
barplot(counts, main = "AGE Distribution")

#Analyzing the scatterplot
library(car)
par(mfrow=c(3, 3))
with(mba_Sal.df, scatterplot(mba_Sal.df$sex , mba_Sal.df$satis))
with(mba_Sal.df, scatterplot(mba_Sal.df$frstlang , mba_Sal.df$gmat_vpc))
with(mba_Sal.df, scatterplot(mba_Sal.df$f_avg , mba_Sal.df$s_avg))
with(mba_Sal.df, scatterplot(mba_Sal.df$quarter , mba_Sal.df$salary))

#Segragating placed and non-placed candidates
#Applying chi-square test
placed <- mba_Sal.df[ which(mba_Sal.df$salary>0 & mba_Sal.df$salary!=998 & mba_Sal.df$salary!=999) , ] 

placedtable <- xtabs(~ placed$gmat_tot+placed$f_avg, data=placed)
View(placedtable)
prop.table(placedtable)
chisq.test(placedtable)

placedtable <- xtabs(~ placed$s_avg+placed$f_avg, data=placed)
View(placedtable)
prop.table(placedtable)
chisq.test(placedtable)

placedtable <- xtabs(~ placed$work_yrs+placed$f_avg, data=placed)
View(placedtable)
prop.table(placedtable)
chisq.test(placedtable)

placedtable <- xtabs(~ placed$frstlang+placed$gmat_vpc, data=placed)
View(placedtable)
prop.table(placedtable)
chisq.test(placedtable)

#Applying t-test
t.test(placed$gmat_vpc, placed$frstlang)
t.test(placed$f_avg, placed$salary)
t.test(placed$s_avg, placed$salary)
t.test(placed$age, placed$work_yrs)
t.test(placed$work_yrs, placed$salary)

#Analyzing the best fit model in pairs
model <- lm(placed$salary ~ placed$age)
summary(model)
model <- lm(placed$salary ~ placed$sex)
summary(model)
model <- lm(placed$salary ~ placed$gmat_tot)
summary(model)
model <- lm(placed$salary ~ placed$work_yrs)
summary(model)

#Analyzing the correlation between the pairs
library(corrgram)
corrgram(placed, main="Corrgram of Placed Variables", lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel=panel.txt)

non_placed <- mba_Sal.df[ which(mba_Sal.df$salary==0) , ] 
nonplaced <- xtabs(~ non_placed$gmat_tot+non_placed$f_avg, data=non_placed)
View(nonplaced)
prop.table(nonplaced)
chisq.test(nonplaced)

nonplaced <- xtabs(~ non_placed$s_avg+non_placed$f_avg, data=non_placed)
View(nonplaced)
prop.table(nonplaced)
chisq.test(nonplaced)

nonplaced <- xtabs(~ non_placed$work_yrs+non_placed$f_avg, data=non_placed)
View(nonplaced)
prop.table(nonplaced)
chisq.test(nonplaced)

library(ggplot2)
ggplot(placed, aes(x = salary, fill = salary)) + geom_bar()
ggplot(placed, aes(x = gmat_tot, fill = gmat_tot)) + geom_bar()


library(ggplot2)
ggplot(non_placed, aes(x = gmat_tot, fill = gmat_tot)) + geom_bar()

mba_Sal.df$sal <- ifelse(mba_Sal.df$salary==998,0,
                         (ifelse(mba_Sal.df$salary==999,0,
                                 (ifelse(mba_Sal.df$salary==0,0, 1
                                 )))))
                              
mba_Sal.data = glm(formula = mba_Sal.df$sal ~ mba_Sal.df$gmat_tot+mba_Sal.df$f_avg+mba_Sal.df$s_avg+mba_Sal.df$work_yrs+mba_Sal.df$age, data = placed, family = binomial)
summary(mba_Sal.data)
