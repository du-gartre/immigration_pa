#* Title: 03_get_tables
#* 
#* Code function: This script uses the data generated in `02_data_wrangling.R`
#*                And performs multiple evaluations to the data.
#*                
#*                At the end of the code, logistic and a log-binomial 
#*                regressions are implemented considering the survey weights.
#*                
#* Creation date: March 22th 2025
#* Author: David U. Garibay Treviño, M.Sc.

# 01 Initial Setup --------------------------------------------------------

## 01.01 Clean environment ------------------------------------------------
remove(list = ls())

#* Refresh environment memory
gc()

## 01.02 Load libraries ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(gmodels)
library(questionr)
library(forcats)
library(interactionR)

# 02 Load data ------------------------------------------------------------

df_cchs_1718_prep <- readRDS(file = "data/df_cchs_1718_prepared.rds")

df_cchs_1718_prep$sense_belong


#* *Note* the `immigrant_new` variable disaggregates by the length of stay in 
#* Canada, however, it also presents more NA's than the `immigrant` variable


# Add NA as a category (use to perform Sensitivity analysis on missing data)
df_cchs_1718_prep$PA                <- forcats::fct_na_value_to_level(df_cchs_1718_prep$PA)
df_cchs_1718_prep$immigrant_new     <- forcats::fct_na_value_to_level(df_cchs_1718_prep$immigrant_new)
df_cchs_1718_prep$immigrant_10y     <- forcats::fct_na_value_to_level(df_cchs_1718_prep$immigrant_new)
df_cchs_1718_prep$sex_cat           <- forcats::fct_na_value_to_level(df_cchs_1718_prep$sex_cat)
df_cchs_1718_prep$age_cat           <- forcats::fct_na_value_to_level(df_cchs_1718_prep$age_cat)
df_cchs_1718_prep$sense_belong      <- forcats::fct_na_value_to_level(df_cchs_1718_prep$sense_belong)
df_cchs_1718_prep$household_inc_cat <- forcats::fct_na_value_to_level(df_cchs_1718_prep$household_inc_cat)


# 03 Table time!! ---------------------------------------------------------



## 03.01 Crude prevalence of the disorder -------------------------------------

df_cchs_1718_prep %>% 
  count(disorder) %>% 
  mutate(pop = sum(n)) %>% 
  mutate(prev = round(n/pop, 4)*100)


v_crude_prev_values <- table(df_cchs_1718_prep$disorder, useNA = "ifany")
v_crude_prev_prop   <- prop.table(table(df_cchs_1718_prep$disorder, useNA = "ifany"))



data.frame(condition = c("None", "Mood or anxiety disorder"),
           count      = as.numeric(v_crude_prev_values),
           proportion = round(as.numeric(v_crude_prev_prop), 4)*100)



## 03.02 Disorder prevalence by immigration status --------------------------


df_cchs_1718_prep %>% 
  count(immigrant)

df_cchs_1718_prep %>% 
  count(immigrant, disorder) %>% 
  group_by(immigrant) %>% 
  mutate(pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/pop, 4)*100) %>% 
  tidyr::drop_na()



## 03.03 Table 1 --------------------------------------------------------------



# > sum(table(df_cchs_1718_prep$sex_cat))
# [1] 12920

# Physical Activity
df_t1_PA <- df_cchs_1718_prep %>% 
  count(PA, disorder) %>% 
  group_by(PA) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)


# Time since migration
df_t1_immigrant <- df_cchs_1718_prep %>% 
  count(immigrant_new, disorder) %>% 
  group_by(immigrant_new) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)

# Sex
df_t1_sex <- df_cchs_1718_prep %>% 
  count(sex_cat, disorder) %>% 
  group_by(sex_cat) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)

# Age
df_t1_age <- df_cchs_1718_prep %>% 
  count(age_cat, disorder) %>% 
  group_by(age_cat) %>% 
  mutate(age_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/age_pop, 4)*100)
# filter(disorder == 1)

# Sense of belonging
df_t1_belonging <- df_cchs_1718_prep %>% 
  count(sense_belong, disorder) %>% 
  group_by(sense_belong) %>% 
  mutate(sense_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sense_pop, 4)*100)
# filter(disorder == 1)

# Income
df_t1_income <- df_cchs_1718_prep %>% 
  count(household_inc_cat, disorder) %>% 
  group_by(household_inc_cat) %>% 
  mutate(income_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/income_pop, 4)*100)
# filter(disorder == 1)



## 03.04 Table 2 -----------------------------------------------------------

df_cchs_1718_prep %>% 
  group_by(immigrant_new, PA) %>% 
  count()

# PA
df_t2 <- df_cchs_1718_prep %>% 
  count(immigrant_new, PA, disorder) %>% 
  group_by(immigrant_new, PA) %>% 
  mutate(pop = sum(n)) %>% 
  mutate(prev = round(n/pop, 4)*100) %>% 
  ungroup() %>% 
  filter(disorder == 1) %>%
  group_by(immigrant_new) %>% 
  # mutate(rate_rate = rate/lead(rate))
  mutate(prev_ratio = prev/lag(prev))




# 04 Stratified analysis --------------------------------------------------

# General risk of developing Mood or anxiety disorder
model_1 <- glm(formula = disorder ~ 1,
               data = df_cchs_1718_prep,
               family = binomial())

round(exp(coef(model_1)), 2)
round(exp(confint(model_1)), 2)



# Crude risk of developing Mood or anxiety disorder by Physical activity
model_2 <- glm(formula = disorder ~ PA,
               data = df_cchs_1718_prep,
               family = binomial())

round(odds.ratio(model_2), 2)


# Crude risk of developing Mood or anxiety disorder by Time since migration
model_3 <- glm(formula = disorder ~ immigrant_new,
               data = df_cchs_1718_prep,
               family = binomial())

round(odds.ratio(model_3), 2)


# Crude risk of developing mood or anxiety disorder by sex
model_3 <- glm(formula = disorder ~ sex_cat,
               data = df_cchs_1718_prep,
               family = binomial())

round(odds.ratio(model_3), 2)

# Crude risk of developing mood or anxiety disorder by age
model_4 <- glm(formula = disorder ~ age_cat,
               data = df_cchs_1718_prep,
               family = binomial())

round(odds.ratio(model_4), 2)

# Crude risk of developing mood or anxiety disorder by sense of belonging
model_5 <- glm(formula = disorder ~ sense_belong,
               data = df_cchs_1718_prep,
               family = binomial())

round(odds.ratio(model_5), 2)

# Crude risk of developing mood or anxiety disorder by total household income
model_6 <- glm(formula = disorder ~ household_inc_cat,
               data = df_cchs_1718_prep,
               family = binomial())

round(odds.ratio(model_6), 2)




# 04 Crude estimate for an association ------------------------------------

#* Here I have to develop a Chi-squared test
CrossTable(x = df_cchs_1718_prep$PA,
           y = df_cchs_1718_prep$disorder,
           chisq = T)


## 04.01 Stratified test ---------------------------------------------------

# # Test for non-immigrants
# df_cchs_1718_nm <- df_cchs_1718_prep %>% 
#   filter(immigrant == "non-immigrant")
# 
# CrossTable(x = df_cchs_1718_nm$PA,
#            y = df_cchs_1718_nm$disorder,
#            chisq = T)

# Test for immigrants
df_cchs_1718_im <- df_cchs_1718_prep %>% 
  filter(immigrant == "immigrant")

CrossTable(x = df_cchs_1718_im$PA,
           y = df_cchs_1718_im$disorder,
           chisq = T)


# # Test for non-immigrants
# df_cchs_1718_nm <- df_cchs_1718_prep %>% 
#   filter(immigrant == "non-immigrant")
# 
# CrossTable(x = df_cchs_1718_nm$PA,
#            y = df_cchs_1718_nm$disorder,
#            chisq = T)

# Test for immigrants
df_cchs_1718_im <- df_cchs_1718_prep %>% 
  filter(immigrant == "immigrant")

CrossTable(x = df_cchs_1718_im$PA,
           y = df_cchs_1718_im$disorder,
           chisq = T)





# 05 Test for interaction -------------------------------------------------


# Adjust model
fit_interaction <- glm(formula = disorder ~ immigrant_10y*PA,
                       data = df_cchs_1718_prep,
                       family = binomial(link = "logit"))

summary(fit_interaction)

l_interaction_summary <- interactionR::interactionR(
  fit_interaction,
  exposure_names = c("immigrant_10y", "PA"),
  ci.type = "delta", 
  ci.level = 0.95,
  em = FALSE, 
  recode = FALSE)

round(l_interaction_summary$dframe, 2)


# 06 Run regression ----------------------------------------------------------


#* *To do*
#* - implement survey weights

## 06.01 Logistic ----------------------------------------------------------

fit_logistic <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
               data = df_cchs_1718_prep,
               family = binomial())

summary(fit_logistic)
questionr::odds.ratio(fit_logistic)

# AIC(model_1, model_2)
# 
# 
# questionr::odds.ratio(model_1)
# 
# names(exp(coef(model_1)))
# round(exp(coef(model_1)), 2)
# 
# round(coef(model_1),digits =  2)
# exp(coef(model_1))
# 
# round(x = exp(confint(model_1)), digits = 2)
# 
# pander(model_1)
# 
# 
# ??odds.ratio()




## 06.02 Log-binomial ------------------------------------------------------

fit_logbinom <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                    data = df_cchs_1718_prep,
                    family = binomial(link = "log"))

summary(fit_logbinom)


v_lbinom_coef <- coef(fit_logbinom)
df_lbinom_CI  <- confint(fit_logbinom)

class(df_lbinom_CI)


df_lbinom_res <- tibble(variable = names(v_lbinom_coef),
                        coef     = exp(v_lbinom_coef),
                        lb_95    = exp(df_lbinom_CI[, 1]),
                        ub_95    = exp(df_lbinom_CI[, 2]))
