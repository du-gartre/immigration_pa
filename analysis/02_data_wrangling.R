#* Title: 02_data_wrangling.R
#* 
#* Code function: This script filters the `df_cchs_1718` data set to keep only
#*                the required variables to evaluate the association between
#*                physical activity and prevalence of mental health disorders
#*                in immigrants. 
#*                
#*                The final data set will only keep individuals older than 18 
#*                years of age. The data set will be filtered to  remove NA's in 
#*                the outcome and the exposure variables. 
#*                
#*                The outcome variable indicates if an individual has
#*                mood disorder *or* anxiety disorder. If the individual has any
#*                of both disorders, its value will be 1, otherwise 0.
#*                
#*                The exposure variable is physical activity.
#*                - Define the threshold of the physical activity to use
#* 
#* Creation date: March 22 2025
#* Author: David U. Garibay Treviño, M.Sc.

# 01 Initial Setup --------------------------------------------------------

## 01.01 Clean environment ------------------------------------------------
remove(list = ls())

#* Refresh environment memory
gc()

## 01.02 Load libraries ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(haven)



# 02 Read raw data --------------------------------------------------------
df_cchs_1718_raw <- readRDS(file = "data/df_cchs_1718.rds")


# Remove labels coming from SAS data
df_cchs_1718_0 <- df_cchs_1718_raw %>% 
  haven::zap_labels()


# 03 Data overview --------------------------------------------------------

# Total population in survey: 113,290 individuals
nrow(df_cchs_1718_0)

#* Immigrant population 
#* Values of Immigrant flag *SDCDVIMM*
#* - 1: landed immigrant (18,683 - 16.5%)
#* - 2: non-immigrant    (92,879 - 82%)
#* - NA                  (1,728  - 1.5%)
df_cchs_1718_0 %>% 
  count(SDCDVIMM) %>% 
  mutate(prop = n/sum(n))

#* Immigrant population by time in Canada
#* Values of length of time since migration *SDCDGRES*
#* - 1: 0 to 9 years    (3,877  - 20.8%)
#* - 2: 10 to 121 years (12,421 - 66.5%)
#* - NA                 (2,385  - 12.8%)
df_cchs_1718_0 %>% 
  filter(SDCDVIMM == 1) %>% 
  count(SDCDGRES) %>% 
  mutate(prop = n/sum(n))


#* Immigrant population by age in Canada
#* Values of *DHHGAGE*
#*  1: Age between 12 and 14 (  390 - 2.09%)
#*  2: Age between 15 and 17 (  413 - 2.21%)
#*  3: Age between 18 and 19 (  262 - 1.40%)
#*  4: Age between 20 and 24 (  768 - 4.11%)
#*  5: Age between 25 and 29 (1,137 - 6.09%)
#*  6: Age between 30 and 34 (1,575 - 8.43%)
#*  7: Age between 35 and 39 (1,641 - 8.78%)
#*  8: Age between 40 and 44 (1,521 - 8.14%)
#*  9: Age between 45 and 49 (1,480 - 7.92%)
#* 10: Age between 50 and 54 (1,287 - 6.89%)
#* 11: Age between 55 and 59 (1,234 - 6.60%)
#* 12: Age between 60 and 64 (1,269 - 6.79%)
#* 13: Age between 65 and 69 (1,651 - 8.84%)
#* 14: Age between 70 and 74 (1,527 - 8.17%)
#* 15: Age between 75 and 79 (1,071 - 5.73%)
#* 16: Age 80+               (1,457 - 7.80%)
df_cchs_1718_0 %>% 
  filter(SDCDVIMM == 1) %>% 
  count(DHHGAGE) %>% 
  mutate(prop = n/sum(n))

#* Immigrant population with values of length of time since migration, 
#* disaggregated by Physical activity indicator *PAADVACV*
#* - 1: PA ≥ recommended by CPAG (7,555 - 46.4%)
#* - 2: PA < recommended by CPAG (3,721 - 22.8%)
#* - 3: No activities reported   (4,101 - 25.2%)
#* - NA:                         (921   - 5.6%)
df_cchs_1718_0 %>% 
  filter(SDCDVIMM == 1) %>% 
  tidyr::drop_na(SDCDGRES) %>% 
  count(PAADVACV) %>% 
  mutate(prop = n/sum(n))


# CCC_195: Has a mood disorder (depression, bipolar, mania, dysthymia)
# CCC_200 : Has anxiety disorder (phobia, OCD, panic)

df_cchs_1718_0$CCC_195
df_cchs_1718_0$CCC_200

df_cchs_1718_0 %>% 
  filter(SDCDVIMM == 1) %>% 
  tidyr::drop_na(SDCDGRES) %>% 
  count(PAADVACV) %>% 
  mutate(prop = n/sum(n))


# 04 Filter data ----------------------------------------------------------

# Select following columns from data set
v_desired_variables <- c("ADM_RNO",  #* Sequential record number
                         "GEO_PRV",  #* Province
                         "DHH_SEX",  #* Sex
                         "DHHGMS",   #* Marital status
                         "DHHDGHSZ", #* Household size
                         "DHHDGLVG", #* living arrangement
                         "DHHGAGE",  #* Age
                         "MAC_010",  #* Worked at job/bussiness - 12 mo
                         "MAC_015",  #* currently attending school
                         "MACG020",  #* student status
                         "GEN_005",  #* perceived health (Exactly the same values as GENDVHDI)
                         "GEN_010",  #* satisfaction with life in general
                         "GEN_015",  #* perceived mental health
                         "GEN_030",  #* sense of belonging to local community
                         "CCC_195",  #* Has a mood disorder (depression, bipolar, mania, dysthymia)
                         "CCC_200",  #*  Has anxiety disorder (phobia, OCD, panic)
                         "HWTDGHTM", #* height, in meters, self reported
                         "HWTDGWTK", #* weight, in kilograms, self reported
                         "HWTDGBMI", #* BMI, self reported
                         "HWTDGCOR", #* BMI, adjusted
                         "PAADVMVA", #* Total minutes - moderate to vigorous physical activities - 7 d - (D)
                         "PAADVREC", #* Total number of minutes of *recreational* physical activities - last 7 days
                         "PAADVACV", #* Physical activity indicator - (D) [according to the Canadian Physical Activity Guidelines: have ≥ 150 min of moderate to vigorous intensity aerobic physical activities per week]
                         "PAADVAC2", #* *Alternate* Physical activity indicator - (D)
                         "PAADVVIG", #* Total number of minutes a respondent engaged in vigorous physical activities in the last 7 days.
                         "PAADVVOL", #* This derived variable represents the volume of physical activity performed in the last seven days. It is measured in METs*minutes/week.
                         "PAADVWHO", #* classifies respondents according to the volume of physical activity they had performed in the past week (PAADVVOL). This classification uses levels of activity volume recommended by the World Health Organization (WHO).
                         "CMH_005",  #* Consulted mental health professional - 12 mo
                         "CMHG010",  #* Consulted mental health professional - num of times - 12 mo
                         "SDC_025",  #* Knowledge of official languages
                         "SDCDVIMM", #* Immigrant flag
                         "SDCDGRES", #* Length of time in Canada since immigration
                         "INCDGHH",  #* Total household income - all sources
                         "INCDVRCA", #* Distribution of household income ratio - national level - (D) DECILES
                         "WTS_M")    #* Weights - master
                          

# Keep only desired columns
df_cchs_1718_0 <- df_cchs_1718_0 %>% 
  dplyr::select(all_of(v_desired_variables))


# a. Remove non-immigrants and people without imigration status
table(df_cchs_1718_0$SDCDVIMM, useNA = "always")

df_cchs_1718_a <- df_cchs_1718_0 %>% 
  filter(SDCDVIMM == 1)

nrow(df_cchs_1718_a)

# b. Remove individuals with ages under 18
df_cchs_1718_a %>% 
  count(age_group = DHHGAGE, name = "count") %>% 
  reframe(age_group, count, cum_count = cumsum(count))

df_cchs_1718_b <- df_cchs_1718_a %>% 
  filter(DHHGAGE >= 3)

nrow(df_cchs_1718_b)

# c. Remove individuals who did not report Physical activity
df_cchs_1718_b %>% 
  count(PAADVACV, name = "count")

df_cchs_1718_c <- df_cchs_1718_b %>% 
  filter(PAADVACV %in% 1:2)

nrow(df_cchs_1718_c)

#* d. Remove individuals who did not report Anxiety or Mood disorder (AMD) 
#* information
df_cchs_1718_c %>% 
  count(CCC_195, CCC_200, name = "count")

df_cchs_1718_1 <- df_cchs_1718_c %>% 
  filter(!is.na(CCC_195) | !is.na(CCC_200))

nrow(df_cchs_1718_1)

# 05 Wrangle data ---------------------------------------------------------

## 05.01 Outcome variable --------------------------------------------------


#* The outcome variable is composite. Participants who reported mood disorders 
#* *or* anxiety disorders will be considered 1, otherwise 0.
#* 
#* - *CCC_195*: Has a mood disorder (depression, bipolar, mania, dysthymia)
#* - *CCC_200*: Has anxiety disorder (phobia, OCD, panic)
df_cchs_1718_1 <- df_cchs_1718_1 %>% 
  mutate(disorder = if_else(condition = CCC_195 == 1 | CCC_200 == 1,
                            true    = 1,
                            false   = 0,
                            missing = NA_real_)) %>% 
  mutate(disorder = if_else(condition = is.na(disorder) & (CCC_195 == 2 | CCC_200 == 2),
                            true    = 0,
                            false   = disorder,
                            missing = NA_real_))


df_cchs_1718_1 %>% 
  count(CCC_195, CCC_200, disorder, name = "count")



## 05.02 Exposure variable --------------------------------------------------

#* Main exposure variable
#* 
#* Define dummy variable denoting individuals achieving the level of Physical 
#* Activity recommended by Canadian Physical activity guidelines
df_cchs_1718_1 <- df_cchs_1718_1 %>%
  mutate(PA = factor(x = PAADVACV, 
                     levels = c(2, 1),
                     labels = c(0, 1)))




#* *Alternate* exposure variable
#* 
#* Add variable indicating if the individual spent at least 150 min in the 
#* last week in *recreational* Physical activities.

df_cchs_1718_1 <- df_cchs_1718_1 %>%
  mutate(PA_rec =  if_else(condition = PAADVREC >= 150,
                           true      = 1,
                           false     = 0,
                           missing   = NA))

table(df_cchs_1718_1$PA_rec)
prop.table(table(df_cchs_1718_1$PA_rec))

table(df_cchs_1718_1$PA)
prop.table(table(df_cchs_1718_1$PA))

## 05.02 Interaction variable ----------------------------------------------

df_cchs_1718_1 <- df_cchs_1718_1 %>% 
  mutate(immigrant_new = case_when(SDCDGRES == 1 ~ "0_to_9_years",
                                   SDCDGRES == 2 ~ "10_and_more")) %>% 
  mutate(immigrant_new = factor(immigrant_new)) %>% 
  mutate(immigrant_new = relevel(x = immigrant_new, ref = "0_to_9_years"))


df_cchs_1718_1$immigrant_new
table(df_cchs_1718_1$immigrant_new)


# Alternative exposure variable
df_cchs_1718_1 <- df_cchs_1718_1 %>% 
  mutate(immigrant_10y = case_when(SDCDGRES == 1 ~ 0,
                                   SDCDGRES == 2 ~ 1))

## 05.03 Covariates --------------------------------------------------------


df_cchs_1718_1 <- df_cchs_1718_1 %>% 
  # Collapse age into three categories
  mutate(age_cat = case_when(DHHGAGE %in% 3:4   ~ "18-24",
                             DHHGAGE %in% 5:12  ~ "25-64",
                             DHHGAGE %in% 13:15 ~ "65+")) %>% 
  # Collapse sense of belonging to a local community into two categories
  mutate(sense_belong = case_when(GEN_030 %in% 3:4 ~ "weak",
                                  GEN_030 %in% 1:2 ~ "strong")) %>% 
  # Collapse household income into three categories
  mutate(household_inc_cat = case_when(INCDGHH %in% 1:3 ~ "[0,60)",
                                       INCDGHH %in% 4   ~ "[60,80)",
                                       INCDGHH %in% 5   ~ "[80,Inf)"))

# 06 Add categories to variables ------------------------------------------


df_cchs_1718_2 <- df_cchs_1718_1 %>% 
  # Convert the previous variables into categorical factors
  mutate(age_cat           = factor(x = age_cat,      ordered = F),
         sense_belong      = factor(x = sense_belong, ordered = F),
         household_inc_cat = factor(x = household_inc_cat)) %>% 
  # Add categories to sex
  mutate(sex_cat = factor(x       = DHH_SEX, 
                          levels  = 1:2, 
                          labels  = c("male", "female"),
                          ordered = F)) %>% 
  # Add categories to Physical Activity recommended by Canadian Physical 
  # activity guidelines
  mutate(PA = factor(x = PAADVACV, 
                     levels = c(2, 1),
                     labels = c(0, 1))) %>% 
  # Immigrant flag
  mutate(immigrant = factor(x = SDCDVIMM,
                            levels = c(2, 1),
                            labels = c("non-immigrant", "immigrant")))


# Define "weak" as the reference category in sense of belonging
df_cchs_1718_2$sense_belong <- relevel(df_cchs_1718_2$sense_belong, ref = "weak")



# 07 Save data ------------------------------------------------------------

#* Tabulate frequencies of values in outcome and exposure variables
table(df_cchs_1718_2$PAADVACV)
table(df_cchs_1718_2$PA)

table(df_cchs_1718_2$disorder)
prop.table(table(df_cchs_1718_2$disorder))*100

table(PA  = df_cchs_1718_2$PAADVACV, 
      AMD = df_cchs_1718_1$disorder)

prop.table(table(PA = df_cchs_1718_2$PAADVACV, 
                 AMD = df_cchs_1718_1$disorder),
           margin = NULL)

#* Check that number of observations is consistent in outcome and 
#* exposure variables
a <- sum(table(df_cchs_1718_2$PAADVACV))
b <- sum(table(df_cchs_1718_2$disorder))
c <- sum(table(df_cchs_1718_2$PAADVACV, df_cchs_1718_1$disorder))


# All of them must return TRUE
stopifnot(a == b,
          a == c,
          b == c)


saveRDS(object = df_cchs_1718_2, 
        file = "data/df_cchs_1718_prepared.rds")


gmodels::CrossTable(x     = df_cchs_1718_2$disorder,  # anxiety or mood disorder
                    y     = df_cchs_1718_2$PA, # Physical activity indicator
                    chisq = TRUE)

