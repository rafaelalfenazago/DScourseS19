
#######################################################
### EFF MONTH WAGE MAIN JOB ############################
########################################################
OLS_Roraima <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                    sex + race + hous_situation + educ_level + reading_writing + 
                    attends_school +  formal_mainjob + social_sec_mainjob + 
                    type_occ_mainjob + type_activity_mainjob +
                    wkd_hs_norm_mainjob + wkdfor_money +
                    years_schooling + n_jobs, 
                  data = Roraima, weights=weight)

OLS_Roraima_fe <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                       sex + race + hous_situation + educ_level + reading_writing + 
                       attends_school +  formal_mainjob + social_sec_mainjob + 
                       type_occ_mainjob + type_activity_mainjob +
                       wkd_hs_norm_mainjob + wkdfor_money +
                       years_schooling + n_jobs + factor(year) + factor(trimester), 
                     data = Roraima, weights=weight)

OLS_Brazil <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                   sex + race + hous_situation + educ_level + reading_writing + 
                   attends_school +  formal_mainjob + social_sec_mainjob + 
                   type_occ_mainjob + type_activity_mainjob +
                   wkd_hs_norm_mainjob + wkdfor_money +
                   years_schooling + n_jobs,
                 data = merged.sample, weights=weight)

OLS_Brazil_fe <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                      sex + race + hous_situation + educ_level + reading_writing + 
                      attends_school +  formal_mainjob + social_sec_mainjob + 
                      type_occ_mainjob + type_activity_mainjob +
                      wkd_hs_norm_mainjob + wkdfor_money +
                      years_schooling + n_jobs + 
                      factor(region) + factor(state) + factor(year) + factor(trimester), 
                    data = merged.sample, weights=weight)


OLS_North <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                  sex + race + hous_situation + educ_level + reading_writing + 
                  attends_school +  formal_mainjob + social_sec_mainjob + 
                  type_occ_mainjob + type_activity_mainjob +
                  wkd_hs_norm_mainjob + wkdfor_money +
                  years_schooling + n_jobs,
                data = North, weights=weight)

OLS_North_fe <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                     sex + race + hous_situation + educ_level + reading_writing + 
                     attends_school +  formal_mainjob + social_sec_mainjob + 
                     type_occ_mainjob + type_activity_mainjob +
                     wkd_hs_norm_mainjob + wkdfor_money +
                     years_schooling + n_jobs + 
                     factor(state) + factor(year) + factor(trimester), 
                   data = North, weights=weight)

stargazer(OLS_Brazil, OLS_Brazil_fe, OLS_North, OLS_North_fe, OLS_Roraima, OLS_Roraima_fe,
          keep=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          order=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          dep.var.labels   = "Ln of Monthly Wage in Main Job",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(2, 2, 2),
          add.lines = list(c("Fixed effects", "No", "Yes","No", "Yes","No", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes", "Yes","Yes", "Yes")))

#############################################################################
##hours worked`MAIN JOB ####################################################
##############################################################################`
hs_OLS_Roraima <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                       sex + race + hous_situation + educ_level + reading_writing + 
                       attends_school +  formal_mainjob + social_sec_mainjob + 
                       type_occ_mainjob + type_activity_mainjob +
                       wkdfor_money + years_schooling + n_jobs,
                     data = Roraima, weights=weight)

hs_OLS_Roraima_fe <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                          sex + race + hous_situation + educ_level + reading_writing + 
                          attends_school +  formal_mainjob + social_sec_mainjob + 
                          type_occ_mainjob + type_activity_mainjob +
                          wkdfor_money + years_schooling + n_jobs +
                          years_schooling + n_jobs + factor(year) + factor(trimester), 
                        data = Roraima, weights=weight)

hs_OLS_Brazil <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                      sex + race + hous_situation + educ_level + reading_writing + 
                      attends_school +  formal_mainjob + social_sec_mainjob + 
                      type_occ_mainjob + type_activity_mainjob +
                      wkdfor_money + years_schooling + n_jobs,
                    data = merged.sample, weights=weight)

hs_OLS_Brazil_fe <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                         sex + race + hous_situation + educ_level + reading_writing + 
                         attends_school +  formal_mainjob + social_sec_mainjob + 
                         type_occ_mainjob + type_activity_mainjob +
                         wkdfor_money + years_schooling +
                         years_schooling + n_jobs + 
                         factor(region) + factor(state) + factor(year) + factor(trimester), 
                       data = merged.sample, weights=weight)


hs_OLS_North <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                     sex + race + hous_situation + educ_level + reading_writing + 
                     attends_school +  formal_mainjob + social_sec_mainjob + 
                     type_occ_mainjob + type_activity_mainjob +
                     wkdfor_money + years_schooling + n_jobs,
                   data = North, weights=weight)

hs_OLS_North_fe <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                        sex + race + hous_situation + educ_level + reading_writing + 
                        attends_school +  formal_mainjob + social_sec_mainjob + 
                        type_occ_mainjob + type_activity_mainjob +
                        wkdfor_money + years_schooling + n_jobs +
                        factor(state) + factor(year) + factor(trimester), 
                      data = North, weights=weight)

stargazer(hs_OLS_Brazil, hs_OLS_Brazil_fe, hs_OLS_North, hs_OLS_North_fe, 
          hs_OLS_Roraima, hs_OLS_Roraima_fe,
          keep=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          order=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          dep.var.labels   = "Ln of Monthly Hours Worked Normally in Main Job",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(2, 2, 2),
          add.lines = list(c("Fixed effects", "No", "Yes","No", "Yes","No", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes", "Yes","Yes", "Yes")))

#############################################################################
##hours worked`ALL JOBS ####################################################
#############################################################################
alljobs_OLS_Roraima <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                            sex + race + hous_situation + educ_level + reading_writing + 
                            attends_school +  formal_mainjob + social_sec_mainjob + 
                            type_occ_mainjob + type_activity_mainjob +
                            wkdfor_money + years_schooling + n_jobs,
                          data = Roraima, weights=weight)

alljobs_OLS_Roraima_fe <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                               sex + race + hous_situation + educ_level + reading_writing + 
                               attends_school +  formal_mainjob + social_sec_mainjob + 
                               type_occ_mainjob + type_activity_mainjob +
                               wkdfor_money + years_schooling + n_jobs +
                               years_schooling + n_jobs + factor(year) + factor(trimester), 
                             data = Roraima, weights=weight)

alljobs_OLS_Brazil <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                           sex + race + hous_situation + educ_level + reading_writing + 
                           attends_school +  formal_mainjob + social_sec_mainjob + 
                           type_occ_mainjob + type_activity_mainjob +
                           wkdfor_money + years_schooling + n_jobs,
                         data = merged.sample, weights=weight)

alljobs_OLS_Brazil_fe <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                              sex + race + hous_situation + educ_level + reading_writing + 
                              attends_school +  formal_mainjob + social_sec_mainjob + 
                              type_occ_mainjob + type_activity_mainjob +
                              wkdfor_money + years_schooling +
                              years_schooling + n_jobs + 
                              factor(region) + factor(state) + factor(year) + factor(trimester), 
                            data = merged.sample, weights=weight)


alljobs_OLS_North <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                          sex + race + hous_situation + educ_level + reading_writing + 
                          attends_school +  formal_mainjob + social_sec_mainjob + 
                          type_occ_mainjob + type_activity_mainjob +
                          wkdfor_money + years_schooling + n_jobs,
                        data = North, weights=weight)

alljobs_OLS_North_fe <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                             sex + race + hous_situation + educ_level + reading_writing + 
                             attends_school +  formal_mainjob + social_sec_mainjob + 
                             type_occ_mainjob + type_activity_mainjob +
                             wkdfor_money + years_schooling + n_jobs +
                             factor(state) + factor(year) + factor(trimester), 
                           data = North, weights=weight)

stargazer(alljobs_OLS_Brazil, alljobs_OLS_Brazil_fe, alljobs_OLS_North, alljobs_OLS_North_fe, 
          alljobs_OLS_Roraima, alljobs_OLS_Roraima_fe,
          keep=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          order=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          dep.var.labels   = "Ln of Monthly Hours Worked Normally in All Jobs",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(2, 2, 2),
          add.lines = list(c("Fixed effects", "No", "Yes","No", "Yes","No", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes", "Yes","Yes", "Yes")))




##################################################################
#########EFF. WAGE MAIN JOB BY LEVEL OF EDUCATION ################
##################################################################
interaction_Roraima_educ <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                 sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                 attends_school +  formal_mainjob + social_sec_mainjob + 
                                 type_occ_mainjob + type_activity_mainjob +
                                 wkdfor_money + years_schooling + n_jobs +
                                 factor(year) + factor(trimester), 
                               data = Roraima, weights=weight)

interaction_North_educ <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                               sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                               attends_school +  formal_mainjob + social_sec_mainjob + 
                               type_occ_mainjob + type_activity_mainjob +
                               wkdfor_money + years_schooling + n_jobs +
                               factor(state) + factor(year) + factor(trimester), 
                             data = North, weights=weight)

interaction_Brazil_educ <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                attends_school +  formal_mainjob + social_sec_mainjob + 
                                type_occ_mainjob + type_activity_mainjob +
                                wkdfor_money + years_schooling + n_jobs +
                                factor(region) + factor(state) + factor(year) + factor(trimester), 
                              data = merged.sample, weights=weight)

stargazer(interaction_Brazil_educ, interaction_North_educ, interaction_Roraima_educ, 
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Effective Monthly Wage in Main Job",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1, 1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes")))



#############################################################################
#########NORMAL HS WKD WEEKLY MAIN JOB BY LEVEL OF EDUCATION ################
#############################################################################
norm_hs_wkd_mainjob_educ_Roraima <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                         sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                         attends_school +  formal_mainjob + social_sec_mainjob + 
                                         type_occ_mainjob + type_activity_mainjob +
                                         wkdfor_money + years_schooling + n_jobs + 
                                         factor(year) + factor(trimester), 
                                       data = Roraima, weights=weight)

norm_hs_wkd_mainjob_educ_North <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                       sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                       attends_school +  formal_mainjob + social_sec_mainjob + 
                                       type_occ_mainjob + type_activity_mainjob +
                                       wkdfor_money + years_schooling + n_jobs +
                                       factor(state) + factor(year) + factor(trimester), 
                                     data = North, weights=weight)

norm_hs_wkd_mainjob_educ_Brazil <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                        sex + race + hous_situation + + educ_level + educ_level*imm_pop_year + reading_writing + 
                                        attends_school +  formal_mainjob + social_sec_mainjob + 
                                        type_occ_mainjob + type_activity_mainjob +
                                        wkdfor_money + years_schooling + n_jobs + 
                                        factor(region) + factor(state) + factor(year) + factor(trimester), 
                                      data = merged.sample, weights=weight)

stargazer(norm_hs_wkd_mainjob_educ_Brazil, norm_hs_wkd_mainjob_educ_North, norm_hs_wkd_mainjob_educ_Roraima,
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Normal Weekly Worked Hours in Main Job",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1, 1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes")))



#############################################################################
#########NORM HS WKD WEEKLY ALL JOBS BY LEVEL OF EDUCATION ################
#############################################################################
norm_hs_wkd_alljobs_educ_Roraima <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                         sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                         attends_school +  formal_mainjob + social_sec_mainjob + 
                                         type_occ_mainjob + type_activity_mainjob +
                                         wkdfor_money + years_schooling + n_jobs +
                                         factor(year) + factor(trimester), 
                                       data = Roraima, weights=weight)

norm_hs_wkd_alljobs_educ_North <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                       sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                       attends_school +  formal_mainjob + social_sec_mainjob + 
                                       type_occ_mainjob + type_activity_mainjob +
                                       wkdfor_money + years_schooling + n_jobs +
                                       factor(state) + factor(year) + factor(trimester), 
                                     data = North, weights=weight)

norm_hs_wkd_alljobs_educ_Brazil <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                        sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                        attends_school +  formal_mainjob + social_sec_mainjob + 
                                        type_occ_mainjob + type_activity_mainjob +
                                        wkdfor_money + years_schooling + n_jobs +
                                        factor(region) + factor(state) + factor(year) + factor(trimester), 
                                      data = merged.sample, weights=weight)

stargazer(norm_hs_wkd_alljobs_educ_Brazil, norm_hs_wkd_alljobs_educ_North, norm_hs_wkd_alljobs_educ_Roraima,
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Normal Weekly Worked Hours in All Jobs",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1, 1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes")))




##################################################################
#########EFF. WAGE MAIN JOB BY SECTOR OF ECONOMY ################
##################################################################
interaction_Roraima_activity <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                     sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                     attends_school +  formal_mainjob + social_sec_mainjob + 
                                     type_occ_mainjob + type_activity_mainjob +
                                     wkdfor_money + years_schooling + n_jobs +
                                     factor(year) + factor(trimester), 
                                   data = Roraima, weights=weight)

interaction_North_activity <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                   sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                   attends_school +  formal_mainjob + social_sec_mainjob + 
                                   type_occ_mainjob + type_activity_mainjob +
                                   wkdfor_money + years_schooling + n_jobs +
                                   factor(state) + factor(year) + factor(trimester), 
                                 data = North, weights=weight)

interaction_Brazil_activity <- lm(log_effect_month_wage_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                    sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                    attends_school +  formal_mainjob + social_sec_mainjob + 
                                    type_occ_mainjob + type_activity_mainjob +
                                    wkdfor_money + years_schooling + n_jobs +
                                    factor(region) + factor(state) + factor(year) + factor(trimester), 
                                  data = merged.sample, weights=weight)

stargazer(interaction_Brazil_activity, interaction_North_activity, interaction_Roraima_activity, 
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Effective Monthly Wage in Main Job",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1,1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes", "Yes")))


##################################################################################
######### Normal Weekly Worked Hours in Main Job BY SECTOR OF ECONOMY ################
##################################################################################
norm_hs_mainjob_Roraima_activity <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                         sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                         attends_school +  formal_mainjob + social_sec_mainjob + 
                                         type_occ_mainjob + type_activity_mainjob +
                                         wkdfor_money + years_schooling + n_jobs +
                                         factor(year) + factor(trimester), 
                                       data = Roraima, weights=weight)

norm_hs_mainjob_North_activity <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                       sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                       attends_school +  formal_mainjob + social_sec_mainjob + 
                                       type_occ_mainjob + type_activity_mainjob +
                                       wkdfor_money + years_schooling + n_jobs +
                                       factor(state) + factor(year) + factor(trimester), 
                                     data = North, weights=weight)

norm_hs_mainjob_Brazil_activity <- lm(ln_wkd_hs_norm_mainjob ~ year + imm_pop_year + age + peop_living_hous +
                                        sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                        attends_school +  formal_mainjob + social_sec_mainjob + 
                                        type_occ_mainjob + type_activity_mainjob +
                                        wkdfor_money + years_schooling + n_jobs +
                                        factor(region) + factor(state) + factor(year) + factor(trimester), 
                                      data = merged.sample, weights=weight)

stargazer(norm_hs_mainjob_Brazil_activity, norm_hs_mainjob_North_activity, norm_hs_mainjob_Roraima_activity, 
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Normal Hours Worked weekly in Main Job",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1,1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes", "Yes")))



##################################################################################
######### Normal Weekly Worked Hours in All Jobs BY SECTOR OF ECONOMY ################
##################################################################################
norm_hs_alljobs_Roraima_activity <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                         sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                         attends_school +  formal_mainjob + social_sec_mainjob + 
                                         type_occ_mainjob + type_activity_mainjob +
                                         wkdfor_money + years_schooling + n_jobs +
                                         factor(year) + factor(trimester), 
                                       data = Roraima, weights=weight)

norm_hs_alljobs_North_activity <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                       sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                       attends_school +  formal_mainjob + social_sec_mainjob + 
                                       type_occ_mainjob + type_activity_mainjob +
                                       wkdfor_money + years_schooling + n_jobs +
                                       factor(state) + factor(year) + factor(trimester), 
                                     data = North, weights=weight)

norm_hs_alljobs_Brazil_activity <- lm(ln_wk_wkd_hrs_norm_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                        sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                        attends_school +  formal_mainjob + social_sec_mainjob + 
                                        type_occ_mainjob + type_activity_mainjob +
                                        wkdfor_money + years_schooling + n_jobs +
                                        factor(region) + factor(state) + factor(year) + factor(trimester), 
                                      data = merged.sample, weights=weight)

stargazer(norm_hs_alljobs_Brazil_activity, norm_hs_alljobs_North_activity, norm_hs_alljobs_Roraima_activity, 
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Normal Hours Worked weekly in All Jobs",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1,1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes", "Yes")))

##################################################################
#########EFF. WAGE ALL JOBS JOB BY SECTOR OF ECONOMY ################
##################################################################
interaction_Roraima_activity_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                             sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                             attends_school +  formal_mainjob + social_sec_mainjob + 
                                             type_occ_mainjob + type_activity_mainjob +
                                             wkdfor_money + years_schooling + n_jobs +
                                             factor(year) + factor(trimester), 
                                           data = Roraima, weights=weight)

interaction_North_activity_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                           sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                           attends_school +  formal_mainjob + social_sec_mainjob + 
                                           type_occ_mainjob + type_activity_mainjob +
                                           wkdfor_money + years_schooling + n_jobs +
                                           factor(state) + factor(year) + factor(trimester), 
                                         data = North, weights=weight)

interaction_Brazil_activity_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                            sex + race + hous_situation + + educ_level + type_activity_mainjob*imm_pop_year  + reading_writing + 
                                            attends_school +  formal_mainjob + social_sec_mainjob + 
                                            type_occ_mainjob + type_activity_mainjob +
                                            wkdfor_money + years_schooling + n_jobs +
                                            factor(region) + factor(state) + factor(year) + factor(trimester), 
                                          data = merged.sample, weights=weight)

stargazer(interaction_Brazil_activity_alljobs, interaction_North_activity_alljobs, interaction_Roraima_activity_alljobs, 
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Effective Monthly Wage in All Jobs",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1,1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes", "Yes")))


##################################################################
#########EFF. WAGE ALL JOBS BY LEVEL OF EDUCATION ################
##################################################################
interaction_Roraima_educ_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                         sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                         attends_school +  formal_mainjob + social_sec_mainjob + 
                                         type_occ_mainjob + type_activity_mainjob +
                                         wkdfor_money + years_schooling + n_jobs +
                                         factor(year) + factor(trimester), 
                                       data = Roraima, weights=weight)

interaction_North_educ_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                       sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                       attends_school +  formal_mainjob + social_sec_mainjob + 
                                       type_occ_mainjob + type_activity_mainjob +
                                       wkdfor_money + years_schooling + n_jobs +
                                       factor(state) + factor(year) + factor(trimester), 
                                     data = North, weights=weight)

interaction_Brazil_educ_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                                        sex + race + hous_situation + + educ_level + educ_level*imm_pop_year  + reading_writing + 
                                        attends_school +  formal_mainjob + social_sec_mainjob + 
                                        type_occ_mainjob + type_activity_mainjob +
                                        wkdfor_money + years_schooling + n_jobs +
                                        factor(region) + factor(state) + factor(year) + factor(trimester), 
                                      data = merged.sample, weights=weight)

stargazer(interaction_Brazil_educ_alljobs, interaction_North_educ_alljobs, interaction_Roraima_educ_alljobs, 
          keep=c("imm_pop"),
          order=c( "imm_pop"),
          dep.var.labels   = "Ln of Effective Monthly Wage in All Jobs",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(1, 1, 1),
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes")))

#######################################################
### EFF MONTH WAGE ALL JOBS ############################
########################################################
OLS_Roraima_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                            sex + race + hous_situation + educ_level + reading_writing + 
                            attends_school +  formal_mainjob + social_sec_mainjob + 
                            type_occ_mainjob + type_activity_mainjob +
                            wkd_hs_norm_mainjob + wkdfor_money +
                            years_schooling + n_jobs, 
                          data = Roraima, weights=weight)

OLS_Roraima_fe_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                               sex + race + hous_situation + educ_level + reading_writing + 
                               attends_school +  formal_mainjob + social_sec_mainjob + 
                               type_occ_mainjob + type_activity_mainjob +
                               wkd_hs_norm_mainjob + wkdfor_money +
                               years_schooling + n_jobs + factor(year) + factor(trimester), 
                             data = Roraima, weights=weight)

OLS_Brazil_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                           sex + race + hous_situation + educ_level + reading_writing + 
                           attends_school +  formal_mainjob + social_sec_mainjob + 
                           type_occ_mainjob + type_activity_mainjob +
                           wkd_hs_norm_mainjob + wkdfor_money +
                           years_schooling + n_jobs,
                         data = merged.sample, weights=weight)

OLS_Brazil_fe_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                              sex + race + hous_situation + educ_level + reading_writing + 
                              attends_school +  formal_mainjob + social_sec_mainjob + 
                              type_occ_mainjob + type_activity_mainjob +
                              wkd_hs_norm_mainjob + wkdfor_money +
                              years_schooling + n_jobs + 
                              factor(region) + factor(state) + factor(year) + factor(trimester), 
                            data = merged.sample, weights=weight)


OLS_North_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                          sex + race + hous_situation + educ_level + reading_writing + 
                          attends_school +  formal_mainjob + social_sec_mainjob + 
                          type_occ_mainjob + type_activity_mainjob +
                          wkd_hs_norm_mainjob + wkdfor_money +
                          years_schooling + n_jobs,
                        data = North, weights=weight)

OLS_North_fe_alljobs <- lm(log_effect_month_wage_alljobs ~ year + imm_pop_year + age + peop_living_hous +
                             sex + race + hous_situation + educ_level + reading_writing + 
                             attends_school +  formal_mainjob + social_sec_mainjob + 
                             type_occ_mainjob + type_activity_mainjob +
                             wkd_hs_norm_mainjob + wkdfor_money +
                             years_schooling + n_jobs + 
                             factor(state) + factor(year) + factor(trimester), 
                           data = North, weights=weight)

stargazer(OLS_Brazil_alljobs, OLS_Brazil_fe_alljobs, OLS_North_alljobs, OLS_North_fe_alljobs, 
          OLS_Roraima_alljobs, OLS_Roraima_fe_alljobs,
          keep=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          order=c("imm_pop", "year", "sex", "race", "years of schooling","n_jobs"),
          dep.var.labels   = "Ln of Monthly Wage in All Jobs",
          column.labels   = c("Brazil", "North Region", "Roraima"),
          column.separate = c(2, 2, 2),
          add.lines = list(c("Fixed effects", "No", "Yes","No", "Yes","No", "Yes"),
                           c("Set of Controls", "Yes", "Yes","Yes", "Yes","Yes", "Yes")))



