####################################################################################
########### WAGE GRAPHS ############################################################
####################################################################################


##setting the home directory
setwd("C:/Users/rafae/OneDrive - University of Oklahoma/PhD/Spring 2019/Data Science for Economists/Final Project")

library(haven)
library(car)
library(dplyr)
library(plyr)
library(ggplot2)
library(gghighlight)

##importing wage file
sample <- read_dta("C:/Users/rafae/OneDrive - University of Oklahoma/Effect of Immigration on Native Wages/Stata/sample.dta")
View(sample)


##making a separate dataset with year, state and wage
rest_sample <- select(sample, year, state, region, effect_month_wage_mainjob, normal_month_wage_alljobs)

#making mean of wage over time for Brazil
means_Brazil <- rest_sample %>%
  group_by(year, state) %>%
  summarise_at(vars(effect_month_wage_mainjob), funs(mean(., na.rm=TRUE)))
View(means_Brazil)

means_Brazil_alljobs <- rest_sample %>%
  group_by(year, state) %>%
  summarise_at(vars(normal_month_wage_alljobs), funs(mean(., na.rm=TRUE)))
View(means_Brazil_alljobs)

##making a data set of means for North in each year   
north <-  subset(means_Brazil,region=="North")
means_north <- north %>%
  group_by(year, state) %>%
  summarise_at(vars(effect_month_wage_mainjob), funs(mean(., na.rm=TRUE)))
View(means_north)


##graphing North
graph_wage_North <- ggplot(means_north) +
geom_line(data = means_north, aes(x=year,
                              y=effect_month_wage_mainjob, 
                              group = state, 
                              color = state), size = 2) +
gghighlight(state=="Roraima" | state=="Amazonas" | state=="Amapá", label_key = state) +
scale_shape_manual(values=c(1, 2, 3, 4, 5, 6, 7)) +
scale_x_discrete(name ="Year", limits=c(2012,2013,2014,2015,2016,2017,2018)) +
theme(legend.position="bottom") +
labs(x="Year",
     y="Effective Monthly Wage in the Main Job")
ggsave("graph_wage_North.png") 


##graphing Brazil
graph_wage_Brazil <- ggplot(means_Brazil) +
  geom_line(data = means_Brazil, aes(x=year,
                                    y=effect_month_wage_mainjob, 
                                    group = state, 
                                    color = state), size = 2) +
  gghighlight(state=="Roraima" | state=="Amazonas"| state=="São Paulo"| state=="Distrito Federal"| state=="Maranhão", 
              label_key = state) +
  scale_x_discrete(name ="Year", limits=c(2012,2013,2014,2015,2016,2017,2018)) +
  labs(x="Year",
       y="Effective Monthly Wage in the Main Job")
ggsave("graph_wage_Brazil.png")


##graphing Brazil Normal Month Wage All Jobs
graph_wage_Brazil_alljobs <- ggplot(means_Brazil_alljobs) +
  geom_line(data = means_Brazil_alljobs, aes(x=year,
                                     y=normal_month_wage_alljobs, 
                                     group = state, 
                                     color = state), size = 2) +
  gghighlight(state=="Roraima" | state=="Amazonas"| state=="São Paulo"| state=="Distrito Federal"| state=="Maranhão", 
              label_key = state) +
  scale_x_discrete(name ="Year", limits=c(2012,2013,2014,2015,2016,2017,2018)) +
  labs(x="Year",
       y="Normal Monthly Wage in All Jobs")
ggsave("graph_wage_Brazil_alljobs.png")

####################################################################################
########### REFUGEE GRAPHS #########################################################
####################################################################################

## importing refugee file
solic_refugee <- read_dta("C:/Users/rafae/OneDrive - University of Oklahoma/Effect of Immigration on Native Wages/Stata/solic_refugee.dta")
View(solic_refugee)

## Amount of All Refugees
all_refugees <-  subset(solic_refugee, year>2009)
amount_all_refugees <- all_refugees %>%
  group_by(UF, year) %>%
  summarise_at(vars(Quantidade), funs(sum(Quantidade)))
View(amount_all_refugees)

##Amount of Venezuelans Refugees by UF
venezuelans <- subset(solic_refugee, Nacionalidade=="VENEZUELA" & year>2009)
View(venezuelans)  
amount_ven_refugees <- venezuelans %>%
  group_by(UF, year) %>%
  summarise_at(vars(Quantidade), funs(sum(Quantidade)))
View(amount_ven_refugees)

##Amount of Venezuelans Refugees 
venezuelans_Brazil <- venezuelans %>%
  group_by(year) %>%
  summarise_at(vars(Quantidade), funs(sum(Quantidade)))
View(venezuelans_Brazil)


##graphing All Refugees
all_ref <- ggplot(amount_all_refugees) +
  geom_line(data = amount_all_refugees, aes(x=year,
                                             y=Quantidade, 
                                             group = UF, 
                                             color = UF), size = 2) +
  gghighlight(UF=="RR" | UF=="AM"| UF=="SP"| UF=="AC", label_key = UF) +
  scale_x_discrete(name ="Year", limits=c(2010,2011,2012,2013,2014,2015,2016,2017,2018)) +
  theme(axis.text = element_text(size=20),
        axis.title=element_text(size=20,face="bold"))  +
  labs(x="Year",
       y="Number of Refugee Seekers")
ggsave("all_ref.png", width = 30, height = 20, units = "cm")



##graphing Venezuelans Refugees
ven_ref <- ggplot(amount_ven_refugees) +
  geom_line(data = amount_ven_refugees, aes(x=year,
                                            y=Quantidade, 
                                            group = UF, 
                                            color = UF), size = 2) +
  gghighlight(UF=="RR" | UF=="AM", label_key = UF) +
  scale_x_discrete(name ="Year", limits=c(2010,2011,2012,2013,2014,2015,2016,2017,2018)) +
  theme(axis.text = element_text(size=20),
        axis.title=element_text(size=20,face="bold")) +
  labs(x="Year",
       y="Number of Venezuelans Refugee Seekers")
ggsave("ven_ref.png", width = 30, height = 20, units = "cm")


##graphing Venezuelans Refugees in Brazil
ven_ref_Brazil <- ggplot(venezuelans_Brazil) +
  geom_line(data = venezuelans_Brazil, aes(x=year,
                                            y=Quantidade), 
            size = 2) +
  geom_point(data = venezuelans_Brazil, aes(x=year,
                                            y=Quantidade))+
  scale_x_discrete(name ="Year", limits=c(2010,2011,2012,2013,2014,2015,2016,2017,2018)) +
  theme(axis.text = element_text(size=20),
        axis.title=element_text(size=20,face="bold")) +
  labs(x="Year",
       y="Number of Venezuelans Refugee Seekers")
ggsave("ven_ref_Brazil.png", width = 30, height = 20, units = "cm")

#########################################################################################
################### SUMMARY STATS ######################################################
#######################################################################################

##Roraima
stargazer(as.data.frame(subset(sample[c("age", "peop_living_hous", "years_schooling", 
                                        "n_jobs", "normal_month_wage_mainjob", 
                                        "effect_month_wage_mainjob", "normal_month_wage_alljobs",
                                        "effect_month_wage_alljobs", "wk_wkd_hrs_norm_alljobs",
                                        "wk_wkd_hrs_eff_alljobs")], 
                               sample$state=="Roraima" & sample$year<2016)),
                               summary.stat = c("n", "mean", "sd", "min", "max"),
          digits=2, 
          covariate.labels = c("Age", "People Living in the Household", "Years of Schooling",
                               "Number of Jobs", "Normal Month. Wage Main Job", "Eff. Month. Wage Main Job",
                               "Normal Month. Wage All Jobs", "Eff. Month. Wage All Jobs", "Weekly Wkd. Normally Hours All Jobs", 
                               "Weekly Wkd. Eff. Hours All Jobs"),
          out="summary_stats_Roraima.tex")


##North
stargazer(as.data.frame(subset(sample[c("age", "peop_living_hous", "years_schooling", 
                                        "n_jobs", "normal_month_wage_mainjob", 
                                        "effect_month_wage_mainjob", "normal_month_wage_alljobs",
                                        "effect_month_wage_alljobs", "wk_wkd_hrs_norm_alljobs",
                                        "wk_wkd_hrs_eff_alljobs")],
                               sample$region=="North" & sample$year<2016)),
          summary.stat = c("n", "mean", "sd", "min", "max"), 
          digits=2, 
          covariate.labels = c("Age", "People Living in the Household", "Years of Schooling",
                               "Number of Jobs", "Normal Month. Wage Main Job", "Eff. Month. Wage Main Job",
                               "Normal Month. Wage All Jobs", "Eff. Month. Wage All Jobs", "Weekly Wkd. Normally Hours All Jobs", 
                               "Weekly Wkd. Eff. Hours All Jobs"),
          out="summary_stats_North.tex")

## Brazil
stargazer(as.data.frame(subset(sample[c("age", "peop_living_hous", "years_schooling", 
                                        "n_jobs", "normal_month_wage_mainjob", 
                                        "effect_month_wage_mainjob", "normal_month_wage_alljobs",
                                        "effect_month_wage_alljobs", "wk_wkd_hrs_norm_alljobs",
                                        "wk_wkd_hrs_eff_alljobs")],
                               sample$year<2016)),
          summary.stat = c("n", "mean", "sd", "min", "max"), 
          digits=2, 
          covariate.labels = c("Age", "People Living in the Household", "Years of Schooling",
                               "Number of Jobs", "Normal Month. Wage Main Job", "Eff. Month. Wage Main Job",
                               "Normal Month. Wage All Jobs", "Eff. Month. Wage All Jobs", "Weekly Wkd. Normally Hours All Jobs", 
                               "Weekly Wkd. Eff. Hours All Jobs"),
          out="summary_stats_Brazil.tex")


#######################################################################################
###### FREQUENCY TABLES ###############################################################
#######################################################################################

##generating dummy variables
sample$North <- ifelse(sample$region=="North", 1, 0)
sample$Roraima <- ifelse(sample$state=="Roraima", 1, 0)


##Frequency for Brazil
Brazil <- subset(sample, Roraima==0, year<2016)
sex_Brazil <- table(Brazil$sex)
race_Brazil <- table(Brazil$race)
educ_Brazil <- table(Brazil$educ_level)
lvl_wkd_hs_Brazil <- table(Brazil$lvl_wk_wkd_hrs_eff_mainjob)
type_activity_mainjob_Brazil <- table(Brazil$type_activity_mainjob)

sex_freq_Brazil <- prop.table(sex_Brazil)
race_freq_Brazil <- prop.table(race_Brazil)
educ_freq_Brazil <- prop.table(educ_Brazil)
lvl_wkd_hs_freq_Brazil <- prop.table(lvl_wkd_hs_Brazil)
type_activity_mainjob_freq_Brazil <- prop.table(type_activity_mainjob_Brazil)

write.csv(sex_freq_Brazil,"sex_freq_Brazil.csv")
write.csv(race_freq_Brazil,"race_freq_Brazil.csv")
write.csv(educ_freq_Brazil,"educ_freq_Brazil.csv")
write.csv(lvl_wkd_hs_freq_Brazil,"lvl_wkd_hs_freq_Brazil.csv")
write.csv(type_activity_mainjob_freq_Brazil,"type_activity_mainjob_freq_Brazil.csv")



##Frequency for North
North <- subset(sample, North==1 & Roraima==0 & year<2016)

sex_North <- table(North$sex)
race_North <- table(North$race)
educ_North <- table(North$educ_level)
lvl_wkd_hs_North <- table(North$lvl_wk_wkd_hrs_eff_mainjob)
type_activity_mainjob_North <- table(North$type_activity_mainjob)

sex_freq_North <- prop.table(sex_North)
race_freq_North <- prop.table(race_North)
educ_freq_North <- prop.table(educ_North)
lvl_wkd_hs_freq_North <- prop.table(lvl_wkd_hs_North)
type_activity_mainjob_freq_North <- prop.table(type_activity_mainjob_North)

write.csv(sex_freq_North,"sex_freq_North.csv")
write.csv(race_freq_North,"race_freq_North.csv")
write.csv(educ_freq_North,"educ_freq_North.csv")
write.csv(lvl_wkd_hs_freq_North,"lvl_wkd_hs_freq_North.csv")
write.csv(type_activity_mainjob_freq_North,"type_activity_mainjob_freq_North.csv")


##Frequency for Roraima
Roraima <- subset(sample, Roraima==1 & year<2016)

sex_Roraima <- table(Roraima$sex)
race_Roraima <- table(Roraima$race)
educ_Roraima <- table(Roraima$educ_level)
lvl_wkd_hs_Roraima <- table(Roraima$lvl_wk_wkd_hrs_eff_mainjob)
type_activity_mainjob_Roraima <- table(Roraima$type_activity_mainjob)

sex_freq_Roraima <- prop.table(sex_Roraima)
race_freq_Roraima <- prop.table(race_Roraima)
educ_freq_Roraima <- prop.table(educ_Roraima)
lvl_wkd_hs_freq_Roraima <- prop.table(lvl_wkd_hs_Roraima)
type_activity_mainjob_freq_Roraima <- prop.table(type_activity_mainjob_Roraima)

write.csv(sex_freq_Roraima,"sex_freq_Roraima.csv")
write.csv(race_freq_Roraima,"race_freq_Roraima.csv")
write.csv(educ_freq_Roraima,"educ_freq_Roraima.csv")
write.csv(lvl_wkd_hs_freq_Roraima,"lvl_wkd_hs_freq_Roraima.csv")
write.csv(type_activity_mainjob_freq_Roraima,"type_activity_mainjob_freq_Roraima.csv")

#######################################################################################
###### T-tests ########################################################################
#######################################################################################

##t-test Brazil vs Roraima
t_wkd_hs_norm_mainjob_Brazil <- t.test(wkd_hs_norm_mainjob ~ Roraima, sample, year<2016)
t_age_Brazil<- t.test(age ~ Roraima, sample, year<2016)
t_peop_living_hous_Brazil <- t.test(peop_living_hous ~ Roraima, sample, year<2016)
t_years_schooling_Brazil <- t.test(years_schooling ~ Roraima, sample, year<2016) 
t_n_jobs_Brazil <- t.test(n_jobs ~Roraima, sample, year<2016)
t_normal_month_wage_mainjob_Brazil <- t.test(normal_month_wage_mainjob ~ Roraima, sample, year<2016) 
t_effect_month_wage_mainjob_Brazil <- t.test(effect_month_wage_mainjob ~ Roraima, sample, year<2016)
t_normal_month_wage_alljobs_Brazil <- t.test(normal_month_wage_alljobs ~ Roraima, sample, year<2016)
t_effect_month_wage_alljobs_Brazil <- t.test(effect_month_wage_alljobs ~ Roraima, sample, year<2016) 
t_wk_wkd_hrs_norm_alljobs_Brazil <- t.test(wk_wkd_hrs_norm_alljobs ~ Roraima, sample, year<2016)
t_wk_wkd_hrs_eff_alljobs_Brazil <- t.test(wk_wkd_hrs_eff_alljobs ~ Roraima, sample, year<2016)


library(broom)
library(purrr)

t.test_Brazil <- map_df(list(t_age_Brazil, t_peop_living_hous_Brazil, t_years_schooling_Brazil,
                   t_n_jobs_Brazil, t_normal_month_wage_mainjob_Brazil, t_effect_month_wage_mainjob_Brazil,
                   t_normal_month_wage_alljobs_Brazil, t_effect_month_wage_alljobs_Brazil,
                   t_wkd_hs_norm_mainjob_Brazil,
                   t_wk_wkd_hrs_norm_alljobs_Brazil, t_wk_wkd_hrs_eff_alljobs_Brazil), tidy)

install.packages("xtable")
library(xtable)
t.test_Brazil <- xtable(t.test_Brazil[c("estimate1", "estimate2", 
                                        "estimate", "statistic", "p.value")])
colnames(t.test_Brazil) <- c("Brazil", "Roraima", "Difference", "t-statistics", "p-value") 
rownames(t.test_Brazil) <- c("Age", "People Living in the Household", "Years of Schooling",
                             "Number of Jobs", "Normal Month. Wage Main Job", "Eff. Month. Wage Main Job",
                             "Normal Month. Wage All Jobs", "Eff. Month. Wage All Jobs", "Normal Weekly Hours Wkd. Main Job",
                             "Normal Weekly Wkd. Hours All Jobs", 
                             "Eff. Weekly Wkd.  Hours All Jobs")                        
print(t.test_Brazil, caption.placement = "top", include.colnames = TRUE, 
       hline.after = c( NULL, 0, nrow( t.test_Brazil )))

##t-test North vs Roraima
North <- subset(sample, region=="North")

t_wkd_hs_norm_mainjob_North <- t.test(wkd_hs_norm_mainjob ~ Roraima, North, year<2016)
t_age_North<- t.test(age ~ Roraima, North, year<2016)
t_peop_living_hous_North <- t.test(peop_living_hous ~ Roraima, North, year<2016)
t_years_schooling_North <- t.test(years_schooling ~ Roraima, North, year<2016) 
t_n_jobs_North <- t.test(n_jobs ~Roraima, North, year<2016)
t_normal_month_wage_mainjob_North <- t.test(normal_month_wage_mainjob ~ Roraima, North, year<2016) 
t_effect_month_wage_mainjob_North <- t.test(effect_month_wage_mainjob ~ Roraima, North, year<2016)
t_normal_month_wage_alljobs_North <- t.test(normal_month_wage_alljobs ~ Roraima, North, year<2016)
t_effect_month_wage_alljobs_North <- t.test(effect_month_wage_alljobs ~ Roraima, North, year<2016) 
t_wk_wkd_hrs_norm_alljobs_North <- t.test(wk_wkd_hrs_norm_alljobs ~ Roraima, North, year<2016)
t_wk_wkd_hrs_eff_alljobs_North <- t.test(wk_wkd_hrs_eff_alljobs ~ Roraima, North, year<2016)


library(broom)
library(purrr)

t.test_North <- map_df(list(t_age_North, t_peop_living_hous_North, t_years_schooling_North, t_n_jobs_North,
                             t_normal_month_wage_mainjob_North, t_effect_month_wage_mainjob_North,
                             t_normal_month_wage_alljobs_North, t_effect_month_wage_alljobs_North,
                             t_wkd_hs_norm_mainjob_North, 
                             t_wk_wkd_hrs_norm_alljobs_North, t_wk_wkd_hrs_eff_alljobs_North), tidy)

t.test_North <- xtable(t.test_North[c("estimate1", "estimate2", 
                                        "estimate", "statistic", "p.value")])
colnames(t.test_North) <- c("North", "Roraima", "Difference", "t-statistics", "p-value") 
rownames(t.test_North) <- c("Age", "People Living in the Household", "Years of Schooling",
                             "Number of Jobs", "Normal Month. Wage Main Job", "Eff. Month. Wage Main Job",
                             "Normal Month. Wage All Jobs", "Eff. Month. Wage All Jobs", "Normal Weekly Wkd Hours Main Job",
                            "Normal Weekly Wkd. Hours All Jobs", 
                             "Eff. Weekly Wkd. Hours All Jobs")                        
print( t.test_North, caption.placement = "top", include.colnames = TRUE, 
       hline.after = c( NULL, 0, nrow( t.test_North)))


#######################################################################################
###### Regressions ########################################################################
#######################################################################################

## Diff-in-Diff (year==2016) - Brazil vs Roraima
sapply(sample, function(x) is.factor(x))
lapply(sample, unique)

sample$log_effect_month_wage_mainjob <- log(sample$effect_month_wage_mainjob)
sample$log_effect_month_wage_mainjob[which(sample$log_effect_month_wage_mainjob==-Inf)] = NA

sample$treatment <- ifelse(sample$year>2015, 1, 0)
sample$Roraima_2016 <- sample$treatment*sample$Roraima



results_no_controls_nofe <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016,
                          data = sample, weights=weight)

results_no_controls_fe <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016 + 
                               factor(year) + factor(trimester) + factor(state) + factor(region),
                               data = sample, weights=weight)


results <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016 +
     year + sex + hous_situation + educ_level + pop_projection + peop_living_hous +
     age + reading_writing + attends_school + years_schooling + n_jobs +
     formal_mainjob + social_sec_mainjob + type_occ_mainjob + type_activity_mainjob +
     wkd_hs_norm_mainjob + wkdfor_money +
     factor(year) + factor(trimester) + factor(state) + factor(region), data = sample, 
   weights=weight)


stargazer(results_no_controls_nofe, results_no_controls_fe, results, 
          keep=c("treatment","Roraima","Roraima_2016","sex", "years_schooling", "educ_level"),
          order=c("treatment","Roraima", "Roraima_2016","sex", "years_schooling", "educ_level"),
          dep.var.labels   = "Ln of Monthly Wage in Main Job",
          add.lines = list(c("Fixed effects?", "No", "Yes", "Yes")))



## Diff-in-Diff (year==2016) - North vs Roraima
North <- subset(sample, region=="North")
North_noAM <- subset(North, state!="Amazonas")

results_no_controls_nofe_North <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016,
                               data = North, weights=weight)


results_no_controls_fe_North <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016 + 
                               factor(year) + factor(trimester) + factor(state),
                             data = North, weights=weight)


results_North <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016 +
                year + sex + hous_situation + educ_level + pop_projection + peop_living_hous +
                age + reading_writing + attends_school + years_schooling + n_jobs +
                formal_mainjob + social_sec_mainjob + type_occ_mainjob + type_activity_mainjob +
                wkd_hs_norm_mainjob + wkdfor_money +
                factor(year) + factor(trimester) + factor(state), data = North, 
              weights=weight)

results_no_controls_nofe_North_noAM <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016,
                                     data = North_noAM, weights=weight)

results_no_controls_fe_North_noAM <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016 + 
                                     factor(year) + factor(trimester) + factor(state),
                                   data = North_noAM, weights=weight)


results_NorthnoAM <- lm(log_effect_month_wage_mainjob ~ treatment + Roraima + Roraima_2016 +
                      year + sex + hous_situation + educ_level + pop_projection + peop_living_hous +
                      age + reading_writing + attends_school + years_schooling + n_jobs +
                      formal_mainjob + social_sec_mainjob + type_occ_mainjob + type_activity_mainjob +
                      wkd_hs_norm_mainjob + wkdfor_money +
                      factor(year) + factor(trimester) + factor(state), data = North_noAM, 
                    weights=weight)

stargazer(results_no_controls_nofe_North, results_no_controls_fe_North, results_North,
          results_no_controls_fe_North_noAM, results_no_controls_fe_North_noAM, results_NorthnoAM,
          keep=c("treatment","Roraima","Roraima_2016","sex", "years_schooling", "educ_level"),
          order=c("treatment","Roraima", "Roraima_2016","sex", "years_schooling", "educ_level"),
          dep.var.labels   = "Ln of Monthly Wage in Main Job",
          column.labels   = c("With Amazonas", "Without Amazonas"),
          column.separate = c(3, 3),
          add.lines = list(c("Fixed effects", "No", "Yes", "Yes")))

##############################################################################
### Different Estimation Strategy ############################################
##############################################################################


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

##merging both datasets
merged.sample <- merge(sample, amount_ven_refugees, by=c("state", "year"))
names(merged.sample)[names(merged.sample) == 'Quantidade'] <- 'Ven.Immigrants'


## generating year*immigrants
merged.sample$year_imm <- merged.sample$year*merged.sample$Ven.Immigrants
merged.sample$RR_imm <- merged.sample$Roraima*merged.sample$Ven.Immigrants
merged.sample$imm_pop <- (merged.sample$Ven.Immigrants/merged.sample$pop_projection)*100
merged.sample$ln_wkd_hs_norm_mainjob <- log(merged.sample$wkd_hs_norm_mainjob)
merged.sample$ln_wk_wkd_hrs_norm_alljobs <- log(merged.sample$wk_wkd_hrs_norm_alljobs)

Roraima <- subset(merged.sample, state=="Roraima")
North <- subset(merged.sample, region=="North" & state!="Amazonas")
