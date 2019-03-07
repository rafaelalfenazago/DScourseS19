#!/bin/sh.




##importing files
setwd("C:/Users/rafae/OneDrive - University of Oklahoma/Effect of Immigration on Native Wages/Documents")
immigration <- read.csv(file="Solicitação de Refúgio Todas as Nacionalidades - 1994-2018.csv", header = TRUE)

##changing variable name and format
names(immigration)
names(immigration)[3] <- 'date1'
format(immigration$date1, format="%m/%Y")

##creating year and mo nth variables
library(dplyr)
refugee <- separate(immigration, "date1", c("Month", "Year" ), sep = "/")

##filtering datasets
library("tidyverse")
ven <- filter(refugee, Nacionalidade =="VENEZUELA")
view(ven)

##(1)
quan <- ven %>% group_by(UF) %>% summarise(count = sum(Quantidade))
view(quan)

##(2)
time <- ven %>% group_by(UF, Year) %>% summarise(count = sum(Quantidade))

##(3)
Ror <- filter(refugee, Nacionalidade =="VENEZUELA", UF=="RR")
time_Ror <- ven %>% group_by(UF, Year) %>% summarise(count = sum(Quantidade))

##graphing

##(1)
quan %>% ggplot(aes(y=count, x = UF)) + geom_col() +
labs(
x = "States",
y = "Number of Refugee Seekers" 
)
##title = "Venezuelans Refugee Seekers by State in Brazil, 2007-2018")

##(2)
time %>% ggplot(aes(y=count, x = Year)) + geom_col() +
labs(
x = "States",
y = "Number of Refugee Seekers")

##title = "Venezuelans Refugee Seekers by Year in Brazil, 2007-2018")


##(3)
time_Ror %>% ggplot(aes(y=count, x = Year)) + geom_col() +
labs(
x = "States",
y = "Number of Refugee Seekers")
##title = "Venezuelans Refugee Seekers by Year in Roraima, 2007-2018")



