#* Title: Initial overview of data
#* 
#* Code function: This script makes an initial overview of the data contained 
#*                in the Canadian Comunity Health Survey (2017-2018) to check 
#*                for research questions
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
library(haven)
library(tidyr)


# 02 Load data ------------------------------------------------------------
df_cchs_1718 <- readRDS(file = "data/df_cchs_1718.rds")



# 03 Filter data ----------------------------------------------------------

# Select following columns from data set

#* variables
#* - ADM_RNO: Sequential record number
#* - GEO_PRV: Province
#* - DHH_SEX: Sex
#* - DHHGMS: Marital status
#* - DHHDGHSZ: Household size
#* - DHHDGLVG: living arrangement
#* - DHHGAGE: Age
#* - MAC_010: Worked at job/bussiness - 12 mo
#* - MAC_015: currently attending school
#* - MACG020: student status
#* - GEN_005: perceived health (Exactly the same values as GENDVHDI)
#* - GEN_010: satisfaction with life in general
#* - GEN_015: perceived mental health
#* - GEN_030: sense of belonging to local community
#* - CCC_195: Has a mood disorder (depression, bipolar, mania, dysthymia)
#* - CCC_200 : Has anxiety disorder (phobia, OCD, panic)
#* - HWTDGHTM:  height, in meters, self reported
#* - HWTDGWTK: weight, in kilograms, self reported
#* - HWTDGBMI: BMI, self reported
#* - HWTDGCOR: BMI, adjusted
#* - PAADVMVA: Total minutes - moderate to vigorous physical activities - 7 d - (D)
#* - PAADVACV: Physical activity indicator - (D)
#* - PAADVACV: Physical activity indicator - (D)
#* - PAADVVIG: Total number of minutes a respondent engaged in vigorous physical activities in the last 7 days.
#* - PAADVVOL: This derived variable represents the volume of physical activity performed in the last seven days. It is measured in METs*minutes/week.
#* - PAADVWHO: classifies respondents according to the volume of physical activity they had performed in the past week (PAADVVOL). This classification uses levels of activity volume recommended by the World Health Organization (WHO).
#* - CMH_005: Consulted mental health professional - 12 mo
#* - CMHG010: Consulted mental health professional - num of times - 12 mo
#* - SDC_025: Knowledge of official languages
#* - SDCDVIMM: Immigrant flag
#* - SDCDGRES: Length of time in Canada since immigration
#* - INCDGHH: Total household income - all sources
#* - INCDVRCA: Distribution of household income ratio - national level - (D) DECILES


#* People who consults mental health professionals have a better mental health?
#* - or usually people wait to feel very bad until consult mental health professional
#* - Does this depend on the type of mental health professional?

v_desired_variables <- c("ADM_RNO",
                         "GEO_PRV",
                         "DHH_SEX",
                         "DHHGMS",
                         "DHHDGHSZ",
                         "DHHDGLVG",
                         "DHHGAGE",
                         "MAC_010",
                         "MAC_015",
                         "MACG020",
                         "GEN_005",
                         "GEN_010",
                         "GEN_015",
                         "GEN_030",
                         "CCC_195",
                         "CCC_200",
                         "HWTDGHTM",
                         "HWTDGWTK",
                         "HWTDGBMI",
                         "HWTDGCOR",
                         "PAADVMVA",
                         "PAADVACV",
                         "PAADVACV",
                         "PAADVVIG",
                         "PAADVVOL",
                         "PAADVWHO",
                         "CMH_005",
                         "CMHG010",
                         "SDC_025",
                         "SDCDVIMM",
                         "SDCDGRES",
                         "INCDGHH",
                         "INCDVRCA")


#* Perceived mental health
#* 
#* Sex
#* Age
#* Marital status
#* Work or school
#* Sense of community belonging
#* Income decile
#* Time since migration
#* Physical activity



# 03 Create tables --------------------------------------------------------


## 03.01 All population ----------------------------------------------------

# DHH_SEX: Sex
table(df_cchs_1718$DHH_SEX)
prop.table(table(df_cchs_1718$DHH_SEX))

# DHHGMS: Marital status
df_cchs_1718$DHHGMS
table(df_cchs_1718$DHHGMS)
prop.table(table(df_cchs_1718$DHHGMS))

# DHHGAGE: Age
df_cchs_1718$DHHGAGE
table(df_cchs_1718$DHHGAGE)
prop.table(table(df_cchs_1718$DHHGAGE))


# MAC_010: Worked at job/bussiness - 12 mo
df_cchs_1718$MAC_010
table(df_cchs_1718$MAC_010)
prop.table(table(df_cchs_1718$MAC_010))

# MAC_015: currently attending school
df_cchs_1718$MAC_015
table(df_cchs_1718$MAC_010)
prop.table(table(df_cchs_1718$MAC_010))

# GEN_030: sense of belonging to local community
df_cchs_1718$GEN_030
table(df_cchs_1718$GEN_030)
prop.table(table(df_cchs_1718$GEN_030))

# PAADVMVA: Total minutes - moderate to vigorous physical activities - 7 d - (D)
# PAADVWHO: classifies respondents according to the volume of physical activity they had performed in the past week (PAADVVOL). This classification uses levels of activity volume recommended by the World Health Organization (WHO).

# Create categories
df_cchs_1718$PAADVMVA_cat <- cut(df_cchs_1718$PAADVMVA, 
                                 breaks = c(0, 60, 120, 300, 600, 1000))

table(df_cchs_1718$PAADVMVA_cat)
prop.table(table(df_cchs_1718$PAADVMVA_cat))

# SDCDVIMM: Immigrant flag
table(df_cchs_1718$SDCDVIMM)
prop.table(table(df_cchs_1718$SDCDVIMM))

# SDCDGRES: Length of time in Canada since immigration
df_cchs_1718$SDCDGRES
table(df_cchs_1718$SDCDGRES)
prop.table(table(df_cchs_1718$SDCDGRES))

# INCDGHH: Total household income - all sources
df_cchs_1718$INCDGHH
table(df_cchs_1718$INCDGHH)
prop.table(table(df_cchs_1718$INCDGHH))

# INCDVRCA: Distribution of household income ratio - national level - (D) DECILES


# GEN_015: perceived mental health
df_cchs_1718$GEN_015
table(df_cchs_1718$GEN_015)
prop.table(table(df_cchs_1718$GEN_015))

# CCC_195: Has a mood disorder (depression, bipolar, mania, dysthymia)
df_cchs_1718$CCC_195
table(df_cchs_1718$CCC_195)
prop.table(table(df_cchs_1718$CCC_195))

# CCC_200 : Has anxiety disorder (phobia, OCD, panic)
df_cchs_1718$CCC_200
table(df_cchs_1718$CCC_200)
prop.table(table(df_cchs_1718$CCC_200))


# CCC_395 : Either mood or anxiety disorder

## Create variable
df_cchs_1718 <- df_cchs_1718 %>% 
  mutate(CCC_395 = if_else(condition = (CCC_195 == 1 | CCC_200 == 1),
                           true = 1,
                           false = 2))

df_cchs_1718$CCC_395
table(df_cchs_1718$CCC_395)
prop.table(table(df_cchs_1718$CCC_395))


## 03.02 Only immigrants ---------------------------------------------------

# Filter data to only keep immigrants
df_cchs_1718_immigrants <- df_cchs_1718 %>% 
  filter(SDCDVIMM == 1)

# DHH_SEX: Sex
table(df_cchs_1718_immigrants$DHH_SEX)
prop.table(table(df_cchs_1718_immigrants$DHH_SEX))

# DHHGMS: Marital status
df_cchs_1718_immigrants$DHHGMS
table(df_cchs_1718_immigrants$DHHGMS)
prop.table(table(df_cchs_1718_immigrants$DHHGMS))

# DHHGAGE: Age
df_cchs_1718_immigrants$DHHGAGE
table(df_cchs_1718_immigrants$DHHGAGE)
prop.table(table(df_cchs_1718_immigrants$DHHGAGE))


# MAC_010: Worked at job/bussiness - 12 mo
df_cchs_1718_immigrants$MAC_010
table(df_cchs_1718_immigrants$MAC_010)
prop.table(table(df_cchs_1718_immigrants$MAC_010))

# MAC_015: currently attending school
df_cchs_1718_immigrants$MAC_015
table(df_cchs_1718_immigrants$MAC_010)
prop.table(table(df_cchs_1718_immigrants$MAC_010))

# GEN_030: sense of belonging to local community
df_cchs_1718_immigrants$GEN_030
table(df_cchs_1718_immigrants$GEN_030)
prop.table(table(df_cchs_1718_immigrants$GEN_030))

# PAADVMVA: Total minutes - moderate to vigorous physical activities - 7 d - (D)
# PAADVWHO: classifies respondents according to the volume of physical activity they had performed in the past week (PAADVVOL). This classification uses levels of activity volume recommended by the World Health Organization (WHO).

# Create categories
df_cchs_1718_immigrants$PAADVMVA_cat <- cut(df_cchs_1718_immigrants$PAADVMVA, 
                                 breaks = c(0, 60, 120, 300, 600, 1000))

table(df_cchs_1718_immigrants$PAADVMVA_cat)
prop.table(table(df_cchs_1718_immigrants$PAADVMVA_cat))

# INCDGHH: Total household income - all sources
df_cchs_1718_immigrants$INCDGHH
table(df_cchs_1718_immigrants$INCDGHH)
prop.table(table(df_cchs_1718_immigrants$INCDGHH))

# INCDVRCA: Distribution of household income ratio - national level - (D) DECILES


# GEN_015: perceived mental health
df_cchs_1718_immigrants$GEN_015
table(df_cchs_1718_immigrants$GEN_015)
prop.table(table(df_cchs_1718_immigrants$GEN_015))

# CCC_195: Has a mood disorder (depression, bipolar, mania, dysthymia)
df_cchs_1718_immigrants$CCC_195
table(df_cchs_1718_immigrants$CCC_195)
prop.table(table(df_cchs_1718_immigrants$CCC_195))

# CCC_200 : Has anxiety disorder (phobia, OCD, panic)
df_cchs_1718_immigrants$CCC_200
table(df_cchs_1718_immigrants$CCC_200)
prop.table(table(df_cchs_1718_immigrants$CCC_200))


# CCC_395 : Either mood or anxiety disorder

## Create variable
df_cchs_1718_immigrants <- df_cchs_1718_immigrants %>% 
  mutate(CCC_395 = if_else(condition = (CCC_195 == 1 | CCC_200 == 1),
                           true = 1,
                           false = 2))

df_cchs_1718_immigrants$CCC_395
table(df_cchs_1718_immigrants$CCC_395)
prop.table(table(df_cchs_1718_immigrants$CCC_395))



# 0ther -------------------------------------------------------------------

# df_cchs_1718_select <- df_cchs_1718 %>% 
#   filter(SDCDVIMM == 1) %>% 
#   select(all_of(v_desired_variables)) %>% 
#   mutate(CCC_195_n = if_else(CCC_195 == 1, 1, 0),
#          CCC_200_n = if_else(CCC_200 == 1, 1, 0)) %>% 
#   mutate(CCC_395 = if_else(condition = (CCC_195_n == 1 | CCC_200_n == 1), 1, 0))
#   
# 
# df_cchs_1718_select %>%
#   dplyr::count(GEN_015, CCC_395) %>% 
#   tidyr::drop_na() %>% 
#   tidyr::pivot_wider(values_from = "n",
#                      names_from = 2)
# 
# 
# table(df_cchs_1718_select$GEO_PRV, df_cchs_1718_select$CCC_395)
# 
# table(df_cchs_1718_select$GEN_015, df_cchs_1718_select$SDCDGRES)
# 
# 
# 
# 
# 
# table(df_cchs_1718$GEN_015)
# table(df_cchs_1718$GEN_010)
# table(df_cchs_1718$GEN_010, df_cchs_1718$GEN_015)
# table(df_cchs_1718$GEN_015, df_cchs_1718$GEN_005)
# 
# table(df_cchs_1718$GEN_030, df_cchs_1718$PAADVMVA)
# 
# plot(x = df_cchs_1718$PAADVMVA, 
#      y = df_cchs_1718$CCC_195)
# 
# plot(x = df_cchs_1718$PAADVWHO, 
#      y = df_cchs_1718$CCC_195)
# 
# 
# plot(x = df_cchs_1718$GEN_030, 
#      y = df_cchs_1718$PAADVMVA)
# 
# ggplot(data = df_cchs_1718,
#        mapping = aes(x = PAADVWHO,
#                      y = CCC_195)) +
#   geom_jitter()
# 
# # Acivity vs mood disorder
# prop.table(table(df_cchs_1718$PAADVWHO, y = df_cchs_1718$CCC_195))
# 
# #* self-reported mental health and mood disorder
# #* GEN_015: 5 Poor - 1 Excellent
# #* CCC_195: 1 yes, 2 no
# table(df_cchs_1718$GEN_015, df_cchs_1718$CCC_195)
# 
# #* self-reported mental health and anxiety
# #* GEN_015: 5 Poor - 1 Excellent
# #* CCC_200: 1 yes, 2 no
# aa <- table(df_cchs_1718$GEN_015, df_cchs_1718$CCC_200)
# 
# 
# # Composite variable
# 
# 
# 
# df_cchs_1718_factor <- df_cchs_1718 %>% 
#   mutate(
#     GEN_005_f = factor(x = GEN_005, 
#                        levels = c(1:5, 7:8),
#                        labels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Don't know", "Refusal"),
#                        ordered = T),
#     GEN_010_f = factor(x = GEN_010, 
#                        levels = 0:10,
#                        labels = paste0("Satisfaction_", 0:10),
#                        ordered = T),
#     GEN_015_f = factor(x = GEN_015, 
#                        levels = c(1:5, 7:9),
#                        labels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Don't know", "Refusal", "Not stated"),
#                        ordered = T))
# 
# 
# 
# 
# 
# ggplot(data = df_cchs_1718_factor,
#        mapping = aes(x = GEN_010_f,
#                      y = GEN_015_f)) + 
#   geom_jitter() + 
#   labs(x = "Satisfaction with life in general",
#        y = "Perceived mental health")
# 
# 
# ggplot(data = df_cchs_1718_factor,
#        mapping = aes(x = GEN_005_f,
#                      y = GEN_015_f)) + 
#   geom_jitter() +
#   labs(x = "Perceived health",
#        y = "Perceived mental health")
# 
# 
# 
# # Now the filtering by sex works
# df_cchs_sex <- df_cchs_1718_unlab %>% 
#   filter(DHH_SEX == 2)
# 
