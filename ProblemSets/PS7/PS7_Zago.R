#!/bin/sh.


#QUESTION 4
# importing file
library(RCurl)
data <- getURL("https://raw.githubusercontent.com/rafaelalfenazago/DScourseS19/master/ModelingOptimization/wages.csv")
wage <- read.csv(text = data)

#QUESTION 5
# dropping missing on hgc and tenure
data_subset <- wage[, c("hgc", "tenure")]  
wage_nomissing <- wage[complete.cases(data_subset), ]

#QUESTION 6                        
# summary table
library(stargazer)
stargazer(wage_nomissing)

#counting missing values in logwage
summary(wage_nomissing)

#QUESTION 7
#a)
question7a<- na.omit(wage_nomissing, cols="logwage")
results_q7a <-lm(logwage ~ hgc + college + poly(tenure, 2) + age + married, data=question7a)
results_q7a
stargazer(results_q7a, title="Results", align=TRUE)

#b)
#replacing logwage missing values with the mean
question7b <- transform(wage_nomissing, logwage = ifelse(is.na(logwage), mean(logwage, na.rm=TRUE), logwage))
results_q7b <-lm(logwage ~ hgc + college + poly(tenure, 2) + age + married, data=question7a)
results_q7b
stargazer(results_q7a, title="Results", align=TRUE)

#c)

#column of dummys to see NAs

missDummy <- function(t)
{
  x <- dim(length(t)) 
  x[which(!is.na(t))] = 1
  x[which(is.na(t))] = 0
  return(x)
}

wage_nomissing$dummy <- missDummy(wage_nomissing$logwage)

#replacing logwages using eq in "a
wage_nomissing$logwage[is.na(wage_nomissing$logwage)] <- predict(results_q7a, newdata=wage[is.na(wage_nomissing$logwage),])
results_q7c <-lm(logwage ~ hgc + college + poly(tenure, 2) + age + married, data=wage_nomissing)
results_q7c

stargazer(results_q7a,results_q7b, results_q7c, title="Results", align=TRUE)


#d)
#using mice package
library(mice)
data(wage)
head(wage)


wage_nonmissing.imp = mice(wage_nomissing, seed = 12345)
summary(wage_nomissing.is)
fit = with(wage_nonmissing.imp, lm(logwage ~ hgc + college + tenure + poly(tenure, 2) + age + married))
round(summary(pool(fit)),2)

