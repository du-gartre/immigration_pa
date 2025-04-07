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
library(weights)
library(Publish)

# 02 Load data ------------------------------------------------------------

df_cchs_1718_prep <- readRDS(file = "data/df_cchs_1718_prepared.rds")


#* *Note* the `immigrant_new` variable disaggregates by the length of stay in 
#* Canada, however, it also presents more NA's than the `immigrant` variable


# include NAs as a category?
include_NA <- FALSE


if (include_NA == TRUE) {
  
  # Add NA as a category (use to perform Sensitivity analysis on missing data)
  df_cchs_1718_prep$PA                <- forcats::fct_na_value_to_level(df_cchs_1718_prep$PA)
  df_cchs_1718_prep$immigrant_new     <- forcats::fct_na_value_to_level(df_cchs_1718_prep$immigrant_new)
  df_cchs_1718_prep$immigrant_10y     <- forcats::fct_na_value_to_level(df_cchs_1718_prep$immigrant_new)
  df_cchs_1718_prep$sex_cat           <- forcats::fct_na_value_to_level(df_cchs_1718_prep$sex_cat)
  df_cchs_1718_prep$age_cat           <- forcats::fct_na_value_to_level(df_cchs_1718_prep$age_cat)
  df_cchs_1718_prep$sense_belong      <- forcats::fct_na_value_to_level(df_cchs_1718_prep$sense_belong)
  df_cchs_1718_prep$household_inc_cat <- forcats::fct_na_value_to_level(df_cchs_1718_prep$household_inc_cat)
}



# 03 Table time!! ---------------------------------------------------------



## 03.01 Crude prevalence of the disorder -------------------------------------

# Unweighted
df_cchs_1718_prep %>% 
  count(disorder) %>% 
  mutate(pop = sum(n)) %>% 
  mutate(prev = round(n/pop, 4)*100)


# Weighted
df_cchs_1718_prep %>% 
  count(disorder, wt = WTS_M) %>% 
  mutate(pop = sum(n)) %>% 
  mutate(prev = round(n/pop, 4)*100)

## 03.02 Disorder prevalence by immigration status --------------------------

# Unweighted
df_cchs_1718_prep %>% 
  count(immigrant, disorder) %>% 
  group_by(immigrant) %>% 
  mutate(pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/pop, 4)*100) %>% 
  tidyr::drop_na()

# Weighted
df_cchs_1718_prep %>% 
  count(immigrant, disorder, wt = WTS_M) %>% 
  group_by(immigrant) %>% 
  mutate(pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/pop, 4)*100) %>% 
  tidyr::drop_na()



## 03.03 Table 1 --------------------------------------------------------------



### 03.03.01 Unweighted -----------------------------------------------------

#* Physical Activity - *main exposure*
df_t1_PA <- df_cchs_1718_prep %>% 
  count(PA, disorder) %>% 
  group_by(PA) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)

#* Physical Activity - *alternate exposure* - recreative PA
df_t1_PA_rec <- df_cchs_1718_prep %>% 
  count(PA_rec, disorder) %>% 
  group_by(PA_rec) %>% 
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

### 03.03.01 Weighted -------------------------------------------------------

#* Physical Activity - *main exposure*
df_t1_PA_w <- df_cchs_1718_prep %>% 
  count(PA, disorder, wt = WTS_M) %>% 
  group_by(PA) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)

#* Physical Activity - *alternate exposure* - recreative PA
df_t1_PA_rec_w <- df_cchs_1718_prep %>% 
  count(PA_rec, disorder, wt = WTS_M) %>% 
  group_by(PA_rec) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)


# Time since migration
df_t1_immigrant_w <- df_cchs_1718_prep %>% 
  count(immigrant_new, disorder, wt = WTS_M) %>% 
  group_by(immigrant_new) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)

# Sex
df_t1_sex_w <- df_cchs_1718_prep %>% 
  count(sex_cat, disorder, wt = WTS_M) %>% 
  group_by(sex_cat) %>% 
  mutate(sex_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sex_pop, 4)*100)
# filter(disorder == 1)

# Age
df_t1_age_w <- df_cchs_1718_prep %>% 
  count(age_cat, disorder, wt = WTS_M) %>% 
  group_by(age_cat) %>% 
  mutate(age_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/age_pop, 4)*100)
# filter(disorder == 1)

# Sense of belonging
df_t1_belonging_w <- df_cchs_1718_prep %>% 
  count(sense_belong, disorder, wt = WTS_M) %>% 
  group_by(sense_belong) %>% 
  mutate(sense_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/sense_pop, 4)*100)
# filter(disorder == 1)

# Income
df_t1_income_w <- df_cchs_1718_prep %>% 
  count(household_inc_cat, disorder, wt = WTS_M) %>% 
  group_by(household_inc_cat) %>% 
  mutate(income_pop = sum(n)) %>% 
  ungroup() %>% 
  mutate(prev = round(n/income_pop, 4)*100)
# filter(disorder == 1)



## 03.04 Table 2 -----------------------------------------------------------

#* *Unweighted*

# Calculate population by time since migration and PA
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


#* *Weighted*

# Calculate population by time since migration and PA
df_cchs_1718_prep %>% 
  group_by(immigrant_new, PA) %>% 
  count()

# PA
df_t2_w <- df_cchs_1718_prep %>% 
  count(immigrant_new, PA, disorder, wt = WTS_M) %>% 
  group_by(immigrant_new, PA) %>% 
  mutate(pop = sum(n)) %>% 
  mutate(prev = round(n/pop, 4)*100) %>% 
  ungroup() %>% 
  filter(disorder == 1) %>%
  group_by(immigrant_new) %>% 
  # mutate(rate_rate = rate/lead(rate))
  mutate(prev_ratio = prev/lag(prev))




# 04 Stratified analysis --------------------------------------------------



## 04.01 unweighted logistic -----------------------------------------------

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



## 04.02 Weighted poisson --------------------------------------------------

# General risk of developing Mood or anxiety disorder
model_1_w_pois <- glm(formula = disorder ~ 1,
                      data = df_cchs_1718_prep,
                      family = poisson(),
                      weights = WTS_M)

exp(coef(model_1_w_pois))
round(exp(confint(model_1_w_pois)), 2)


# Crude risk of developing Mood or anxiety disorder by Physical activity
model_2_w_pois <- glm(formula = disorder ~ PA,
                      data = df_cchs_1718_prep,
                      family = poisson(),
                      weights = WTS_M)

Publish::publish(model_2_w_pois)

# Crude risk of developing Mood or anxiety disorder by Time since migration
model_3_w_pois <- glm(formula = disorder ~ immigrant_new,
                      data = df_cchs_1718_prep,
                      family = poisson(),
                      weights = WTS_M)

Publish::publish(model_3_w_pois)

# Crude risk of developing mood or anxiety disorder by sex
model_4_w_pois <- glm(formula = disorder ~ sex_cat,
               data = df_cchs_1718_prep,
               family = poisson(),
               weights = WTS_M)

Publish::publish(model_4_w_pois)

# Crude risk of developing mood or anxiety disorder by age
model_5_w_pois <- glm(formula = disorder ~ age_cat,
               data = df_cchs_1718_prep,
               family = poisson(),
               weights = WTS_M)

Publish::publish(model_5_w_pois)

# Crude risk of developing mood or anxiety disorder by sense of belonging
model_6_w_pois <- glm(formula = disorder ~ sense_belong,
               data = df_cchs_1718_prep,
               family = poisson(),
               weights = WTS_M)

Publish::publish(model_6_w_pois)

# Crude risk of developing mood or anxiety disorder by total household income
model_7_w_pois <- glm(formula = disorder ~ household_inc_cat,
               data = df_cchs_1718_prep,
               family = poisson(),
               weights = WTS_M)

Publish::publish(model_7_w_pois)


# 04 Test for an association ----------------------------------------------



## 04.01 Unweighted --------------------------------------------------------


#* Here I have to develop a Chi-squared test
CrossTable(x = df_cchs_1718_prep$PA,
           y = df_cchs_1718_prep$disorder,
           chisq = T)

#* Here I have to develop a Chi-squared test
#* *alternate* exposure - recreative PA
CrossTable(x = df_cchs_1718_prep$PA_rec,
           y = df_cchs_1718_prep$disorder,
           chisq = T)



## 04.02 Weighted ----------------------------------------------------------

weights::wtd.chi.sq(var1 = df_cchs_1718_prep$PA,
                    var2 = df_cchs_1718_prep$disorder,
                    weight = df_cchs_1718_prep$WTS_M)



# 05 Test for interaction -------------------------------------------------


#* Adjust model - *main exposure*
fit_interaction <- glm(formula = disorder ~ immigrant_10y*PA,
                       data = df_cchs_1718_prep,
                       family = binomial(link = "logit"))

summary(fit_interaction)
coefficients(fit_interaction)[4]


l_interaction_summary <- interactionR::interactionR(
  fit_interaction,
  exposure_names = c("immigrant_10y", "PA"),
  ci.type = "delta", 
  ci.level = 0.95,
  em = FALSE,
  recode = TRUE)

round(l_interaction_summary$dframe, 2)



#* Adjust model - *alternate exposure*
fit_interaction_alt <- glm(formula = disorder ~ immigrant_10y*PA_rec,
                           data = df_cchs_1718_prep,
                           family = binomial(link = "logit"))

summary(fit_interaction_alt)

l_interaction_summary <- interactionR::interactionR(
  model = fit_interaction_alt,
  exposure_names = c("immigrant_10y", "PA_rec"),
  ci.type = "delta", 
  ci.level = 0.95,
  em = FALSE,
  recode = TRUE)

round(l_interaction_summary$dframe, 2)



## 05.02 Weights -----------------------------------------------------------


#* Adjust model - *main exposure*
#* - log binomial
fit_interaction <- glm(formula = disorder ~ immigrant_10y*PA,
                       data = df_cchs_1718_prep,
                       family = quasibinomial(link = "log"),
                       # family = quasibinomial(link = "logit"),
                       weights = WTS_M)

summary(fit_interaction)

l_interaction_summary <- interactionR::interactionR(
  fit_interaction,
  exposure_names = c("immigrant_10y", "PA"),
  ci.type = "delta", 
  ci.level = 0.95,
  em = FALSE,
  recode = TRUE)

l_interaction_summary$dframe


# #* Adjust model - *alternate exposure*
# fit_interaction_alt <- glm(formula = disorder ~ immigrant_10y*PA_rec,
#                            data = df_cchs_1718_prep,
#                            family = binomial(link = "logit"))
# 
# summary(fit_interaction_alt)
# 
# l_interaction_summary <- interactionR::interactionR(
#   model = fit_interaction_alt,
#   exposure_names = c("immigrant_10y", "PA_rec"),
#   ci.type = "delta", 
#   ci.level = 0.95,
#   em = FALSE,
#   recode = TRUE)
# 
# round(l_interaction_summary$dframe, 2)



# 06 Run regression ----------------------------------------------------------


#* *To do*
#* - implement survey weights



## 06.01 Main exposure - PA ------------------------------------------------

### 06.01.02 Logistic ----------------------------------------------------------

#* Without weights
fit_logistic <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                    data = df_cchs_1718_prep,
                    family = binomial(link = "logit"))

summary(fit_logistic)
questionr::odds.ratio(fit_logistic)
Publish::publish(fit_logistic)

#* With *weights*
fit_logistic_w <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                      data = df_cchs_1718_prep,
                      # family = binomial(link = "logit"),
                      family = quasibinomial(link = "logit"),
                      weights = WTS_M)


summary(fit_logistic_w)
questionr::odds.ratio(fit_logistic_w)


### 06.01.02 Log-binomial ------------------------------------------------------

#* Without weights
fit_logbinom <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                    data = df_cchs_1718_prep,
                    family = binomial(link = "log"))

summary(fit_logbinom)
publish(fit_logbinom)
odds.ratio(fit_logbinom)

#* With *weights*
fit_logbinom_w <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                      data = df_cchs_1718_prep,
                      family = quasibinomial(link = "log"),
                      weights = WTS_M)

summary(fit_logbinom_w)
odds.ratio(fit_logbinom_w)


class(df_cchs_1718_prep$WTS_M )
hist(df_cchs_1718_prep$WTS_M, breaks = 50)


### 06.01.02 Poisson ------------------------------------------------------

#* Without weights
fit_pois <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                data = df_cchs_1718_prep,
                family = poisson)

summary(fit_pois)
exp(coef(fit_pois))

warning("the single PA1 and immigrant variables are missing in publish()")
Publish::publish(object = fit_pois)


#* With *weights*
fit_pois_w <- glm(formula = disorder ~ immigrant_10y + PA + immigrant_10y:PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                  data = df_cchs_1718_prep,
                  family = poisson(),
                  weights = WTS_M)


summary(fit_pois_w)
exp(coef(fit_pois_w))

warning("the single PA1 and immigrant variables are missing in publish()")
Publish::publish(fit_pois_w)


n_coef       <- length(coef(fit_pois_w))
names_coef   <- names(coef(fit_pois_w))
confint_coef <- exp(confint(fit_pois_w))
se_coef      <- summary(fit_pois_w)$coefficients[,2]


# Save data for plotting
df_fit_pois_w <- tibble(index = seq(n_coef),
                        name  = names_coef,
                        label = NA,
                        HR    = exp(coef(fit_pois_w)),
                        se    = se_coef,
                        lb_95 = confint_coef[, 1],
                        ub_95 = confint_coef[, 2])




## 06.02 Alternative exposure - PA_rec -------------------------------------


### 06.01.02 Logistic ----------------------------------------------------------

fit_logistic <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                    data = df_cchs_1718_prep,
                    family = binomial())

df_cchs_1718_prep$wt

summary(fit_logistic)
questionr::odds.ratio(fit_logistic)


### 06.01.02 Log-binomial ------------------------------------------------------

fit_logbinom <- glm(formula = disorder ~ immigrant_10y*PA + sex_cat + age_cat + sense_belong + household_inc_cat,
                    data = df_cchs_1718_prep,
                    family = binomial(link = "log"))

summary(fit_logbinom)
odds.ratio(fit_logbinom)




# 07 Plot results ---------------------------------------------------------




# 07.03 Weighted poisson --------------------------------------------------


df_plot_pois_w <- df_fit_pois_w %>% 
  filter(name != "(Intercept)") %>% 
  mutate(index = seq(nrow(.))) %>% 
  mutate(label2 = c("Time since migration ( vs ≤10 years)",
                    "Physical Activity (≥ CPAG level vs < CPAG level)",
                    "Female vs male",
                    "Age (25-64 years vs 18-24 years)",
                    "Age (≥65 years vs 18-24 years)",
                    "Sense of belonging (Strong vs Weak)",
                    "Household income (60,000-80,000 vs <60,000)",
                    "Household income (≥80,000 vs <60,000)",
                    "Time since migration >10 years & PA ≥ CPAG level"))


plt_plot_pois_w <- ggplot(data = df_plot_pois_w,
       mapping = aes(y = index,
                     x = HR,
                     xmin = lb_95,
                     xmax = ub_95)) + 
  theme_bw(base_size = 16) + 
  geom_point(shape = 18, size = 2) +
  geom_errorbarh(height = 0.55) +
  geom_vline(xintercept = 1, 
             linetype = "dashed") +
  scale_x_continuous(breaks = seq(-10, 10, 0.25)) +
  scale_y_continuous(name = NULL, 
                     breaks = 1:nrow(df_plot_pois_w), 
                     labels = df_plot_pois_w$label2, 
                     trans = "reverse") +
  labs(x = "Hazard Ratio (95% CI)") + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.text.x.bottom = element_text(colour = "black"))

plt_plot_pois_w


ggsave(plot = plt_plot_pois_w,
       filename = "figs/plt_plot_pois_w.png",
       width = 12,
       height = 6)


