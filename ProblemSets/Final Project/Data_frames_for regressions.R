

##setting the home directory
setwd("C:/Users/rafae/OneDrive - University of Oklahoma/PhD/Spring 2019/Data Science for Economists/Final Project")

library(haven)
library(car)
library(dplyr)
library(plyr)
library(ggplot2)
library(gghighlight)
library(stargazer)

##importing wage file
sample <- read_dta("C:/Users/rafae/OneDrive - University of Oklahoma/Effect of Immigration on Native Wages/Stata/sample.dta")

## importing refugee file
solic_refugee <- read_dta("C:/Users/rafae/OneDrive - University of Oklahoma/Effect of Immigration on Native Wages/Stata/solic_refugee.dta")

## importing pop file
library(readxl)
proj_pop <- read_excel("C:/Users/rafae/OneDrive - University of Oklahoma/Effect of Immigration on Native Wages/Documents/proj_pop.xlsx")


##Amount of Venezuelans Refugees by UF
venezuelans <- subset(solic_refugee, Nacionalidade=="VENEZUELA" & year>2009)
View(venezuelans)  
amount_ven_refugees <- venezuelans %>%
  group_by(UF, year) %>%
  summarise_at(vars(Quantidade), funs(sum(Quantidade)))
View(amount_ven_refugees)



##changing the column name from UF to state
names(amount_ven_refugees)[names(amount_ven_refugees) == 'UF'] <- 'state'

##replacing name of observation from abreviation to actual name of state
library(plyr)
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("AC"="Acre"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("AL"="Alagoas"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("AM"="Amazonas"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("AP"="Amapá"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("BA"="Bahia"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("CE"="Ceará"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("DF"="Distrito Federal"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("ES"="Espírito Santo"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("GO"="Goiânia"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("MA"="Maranhão"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("MG"="Minas Gerais"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("MS"="Mato Grosso do Sul"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("MT"="Mato Grosso"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("PA"="Pará"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("PB"="Paraíba"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("PE"="Pernambuco"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("PI"="Piauí"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("PR"="Paraná"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("RJ"="Rio de Janeiro"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("RN"="Rio Grande do Norte"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("RO"="Rondonia"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("RR"="Roraima"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("RS"="Rio Grande do Sul"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("SC"="Santa Catarina"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("SE"="Sergipe"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("SP"="São Paulo"))
amount_ven_refugees$state <- revalue(amount_ven_refugees$state, c("TO"="Tocantins"))


## generating dummies for formal job/no paym/wkd for money
sample$formal_mainjob_dummy<- ifelse(sample$formal_mainjob=="yes", 1, 0)
sample$wkd_nopaym_dummy<- ifelse(sample$wkd_nopaym=="yes", 1, 0)
sample$wkd_wkdfor_money_dummy<- ifelse(sample$wkdfor_money=="yes", 1, 0)


## generating log of hours and wages
#normal worked hs main/all jobs
sample$ln_wkd_hs_norm_mainjob <- log(sample$wkd_hs_norm_mainjob)
sample$ln_wk_wkd_hrs_norm_alljobs <- log(sample$wk_wkd_hrs_norm_alljobs)

#effective wage main/all jobs
sample$log_effect_month_wage_mainjob <- log(sample$effect_month_wage_mainjob)
sample$log_effect_month_wage_mainjob[which(sample$log_effect_month_wage_mainjob==-Inf)] = NA


sample$log_effect_month_wage_alljobs <- log(sample$effect_month_wage_alljobs)
sample$log_effect_month_wage_alljobs[which(sample$log_effect_month_wage_alljobs==-Inf)] = NA

#normal wage main/all jobs
sample$log_normal_month_wage_mainjob <- log(sample$normal_month_wage_mainjob)
sample$log_normal_month_wage_mainjob[which(sample$log_normal_month_wage_mainjob==-Inf)] = NA


sample$log_effect_month_wage_alljobs <- log(sample$effect_month_wage_alljobs)
sample$log_effect_month_wage_alljobs[which(sample$log_effect_month_wage_alljobs==-Inf)] = NA




##merging both datasets
pop.sample <- merge(sample, proj_pop, by=c("state", "year"))
merged.sample <- merge(pop.sample, amount_ven_refugees, by=c("state", "year"))
names(merged.sample)[names(merged.sample) == 'Quantidade'] <- 'Ven.Immigrants'



## generating perc of population (trimester and year)
merged.sample$imm_pop <- (merged.sample$Ven.Immigrants/merged.sample$pop_projection)*100
merged.sample$imm_pop_year <- (merged.sample$Ven.Immigrants/merged.sample$pop)*100


Roraima <- subset(merged.sample, state=="Roraima")
North <- subset(merged.sample, region=="North" & state!="Amazonas")
