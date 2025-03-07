install.packages("haven")
install.packages("survey")
setwd("C:/Users/jyfen/OneDrive/Desktop/EPI 207/REPRODUCING ASSIGNMENT 3")
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(haven)
library(survey)
library(conflicted)

file <- "C:/Users/jyfen/OneDrive/Desktop/EPI 207/REPRODUCING ASSIGNMENT 3/adult.sas7bdat"
CHIS2020 <- read_sas(file)

#variables:
#agecat, srsex, racecat, educcat, incomecat, employment, 
#marit, UR_CLRT2, SMOKING, ILLIDRUG, AJ31, RESTLESSBIN, RESTLESSHL, AC115,
#MJ_MONTH, AC117V2, RAKEDWEIGHTS(they didn't change anything here)

#DATA CLEANING

CHIS2020 <- CHIS2020 %>%
  filter(AC116_P1 != -1) 

CHIS2020 <- CHIS2020 %>%
mutate(age_4cat = case_when(
  SRAGE_P1 >= 18 & SRAGE_P1 <= 34 ~ "18–34 years",
  SRAGE_P1 > 34 & SRAGE_P1 <= 49 ~ "35–49 years",
  SRAGE_P1 > 49 & SRAGE_P1 <= 64 ~ "50–64 years",
  SRAGE_P1 >= 65 ~ "65+ years",
  TRUE ~ NA_character_ 
)) %>% 
  mutate(age_4cat = factor(age_4cat,levels = c("18–34 years", "35–49 years", "50–64 years", "65+ years")))

CHIS2020 <- CHIS2020 %>%
  mutate(gendervar = case_when(
    SRSEX == 1 ~ "Male",
    SRSEX == 2 ~ "Female",
    TRUE ~ NA_character_ 
  )) %>%
  mutate(gendervar = factor(gendervar, levels = c("Male", "Female")))
  

CHIS2020 <- CHIS2020 %>%
  mutate(racecat = case_when(
    OMBSRR_P1 == 1 ~ "Hispanic, any Race",              # Hispanic, any Race
    OMBSRR_P1 == 2 ~ "NH White",                         # NH White
    OMBSRR_P1 == 3 ~ "NH African American",              # NH African American
    OMBSRR_P1 == 5 ~ "NH Asian only",                    # NH Asian only
    OMBSRR_P1 %in% c(4, 6) ~ "Other or two/more races",  # Other or two/more races
    TRUE ~ NA_character_  # This handles any other missing or invalid values
  )) %>%
  mutate(racecat = factor(racecat, levels = c("NH White", "NH African American", "NH Asian only", "Hispanic, any Race", "Other or two/more races")))  # Define the factor levels

CHIS2020 <- CHIS2020 %>% 
  mutate(educcat = case_when(
    SREDUC == 1 ~ "High school or less",
    SREDUC == 2 ~ "High school or less",
    SREDUC == 3 ~ "Some college",
    SREDUC == 4 ~ "University degree or higher",
    TRUE ~ NA_character_  # Handle missing values
  )) %>%
  mutate(educcat = factor(educcat, levels = c("High school or less", "Some college", "University degree or higher")))


CHIS2020 <- CHIS2020 %>%
  mutate(incomecat = case_when(
    AK22_P1 >= 1 & AK22_P1 <= 5 ~ "Less than 50,000",
    AK22_P1 >= 6 & AK22_P1 <= 10 ~ "50,000-99,999",
    AK22_P1 >= 11 & AK22_P1 <= 19 ~ "Greater than or equal to 100,000",
    TRUE ~ NA_character_  
  )) %>%
  mutate(incomecat = factor(incomecat, levels = c("Less than 50,000", "50,000-99,999","Greater than or equal to 100,000"))) 

CHIS2020 <- CHIS2020 %>%
  mutate(employment = case_when(
    WRKST_P1 == 1 ~ "Employed",
    WRKST_P1 == 2 ~ "Employed",
    WRKST_P1 == 3 ~ "Employed",
    WRKST_P1 == 4 ~ "Unemployed",
    WRKST_P1 == 5 ~ "Unemployed",
    TRUE ~ NA_character_  
  )) %>%
  mutate(employment = factor(employment, levels = c("Unemployed", "Employed")))  

CHIS2020 <- CHIS2020 %>%
  mutate(MARIT = case_when(
    MARIT == 1 ~ "Married",
    MARIT == 2 ~ "Not Married",
    MARIT == 3 ~ "Not Married",
    TRUE ~ NA_character_
  )) %>%
  mutate(MARIT = factor(MARIT, levels = c("Not Married", "Married"))) 


CHIS2020 <- CHIS2020 %>%
  mutate(Residence = case_when(
    UR_CLRT2 == 1 ~ "Urban",
    UR_CLRT2 == 2 ~ "Rural",
    TRUE ~ NA_character_  
  )) %>%
  mutate(Residence = factor(Residence, levels = c("Urban", "Rural")))


CHIS2020 <- CHIS2020 %>%
  mutate(Smoking = case_when(
    SMOKING == 1 ~ "Currently Smokes",
    SMOKING == 2 ~ "Quit Smoking",
    SMOKING == 3 ~ "Never smoked regularly",
    TRUE ~ NA_character_  
  )) %>%
  mutate(Smoking = factor(Smoking, 
                          levels = c("Never smoked regularly", "Quit Smoking" , "Currently Smokes")))

CHIS2020 <- CHIS2020 %>%
  mutate(illidrug = case_when(
    ILLIDRUG == 1 ~ "Used illicit drugs",
    ILLIDRUG == 2 ~ "Did not use illicit drugs",
    TRUE ~ NA_character_  
  )) %>%
  mutate(illidrug = factor(illidrug, levels = c("Did not use illicit drugs", "Used illicit drugs")))


CHIS2020 <- CHIS2020 %>%
  mutate(Restlessbin = case_when(
    AJ31 < 5 ~ 1,  # 1 for Yes (restless)
    AJ31 == 5 ~ 0,  # 0 for No (not restless)
    TRUE ~ NA_real_ # Handle cases where AJ31 is missing or out of range
  ))%>%
  mutate(Restlessbin = factor(Restlessbin, levels = c(0, 1), labels = c("No restlessness", "Some restlessness")))

CHIS2020 <- CHIS2020 %>%
  mutate(AJ31 = case_when(
    AJ31 == 1 ~ "All of the time",
    AJ31 == 2 ~ "Most of the time",
    AJ31 == 3 ~ "Some of the time",
    AJ31 == 4 ~ "A little of the time",
    AJ31 == 5 ~ "Not at all",
    TRUE ~ NA_character_ 
  ))

CHIS2020 <- CHIS2020 %>%
  mutate(RestlessHL = case_when(
    AJ31 == "Not at all" ~ "No restlessness",  # No restlessness
    AJ31 == "A little of the time" ~ "Low",    # Low restlessness
    AJ31 %in% c("Some of the time", "Most of the time", "All of the time") ~ "High", # High restlessness
    TRUE ~ NA_character_  # NA for other cases (missing data)
  ))

CHIS2020 <- CHIS2020 %>%
  mutate(AC115 = case_when(
    AC115 == 1 ~ "Yes",   # Yes = 1
    AC115 == 2 ~ "No",
    TRUE ~ NA_character_ # No = 0
  )) %>%
  mutate(AC115 = factor(AC115, levels = c("No", "Yes")))  

CHIS2020 <- CHIS2020 %>% 
  mutate(AC116bin = case_when(
    AC116_P1 == 1 ~ 1,  # Used marijuana in the past month
    AC116_P1 >= 2 & AC116_P1 <= 6 ~ 0,  # Used marijuana at some point in life
    TRUE ~ NA_real_  # Handle any unexpected values
  )) %>%
  mutate(AC116bin = factor(AC116bin, 
                           levels = c(0, 1), 
                           labels = c("Marijuana use ever", "Marijuana use in the past month")))


CHIS2020 <- CHIS2020 %>%
  mutate(MJ_MONTH = case_when(
    AC117V2 >= 1 & AC117V2 <= 7 ~ 1,  # 1 for Yes (marijuana use in past month)
    AC117V2 == -1 ~ 0,                 # 0 for No (no marijuana use)
    TRUE ~ NA_real_                    # Handle other cases (missing or invalid)
  ))%>%
  mutate(MJ_MONTH = factor(MJ_MONTH, levels = c(0,1), labels = c("No", "Yes")))

CHIS2020 <- CHIS2020 %>%
  mutate(AC117V2 = case_when(
    AC117V2 == -1 ~ "Inapplicable",
    AC117V2 == 1 ~ "0 days",
    AC117V2 == 2 ~ "1-2 days",
    AC117V2 == 3 ~ "3-5 days",
    AC117V2 == 4 ~ "6-9 days",
    AC117V2 == 5 ~ "10-19 days",
    AC117V2 == 6 ~ "20-29 days",
    AC117V2 == 7 ~ "30 days or more",
    TRUE ~ NA_character_  
  )) %>%
  mutate(AC117V2 = factor(AC117V2, 
                          levels = c("Inapplicable", "0 days", "1-2 days", "3-5 days", 
                                     "6-9 days", "10-19 days", "20-29 days", "30 days or more")))


# DATA EXCLUSION OR INCLUSION CRITERIA FOR ANALYSIS: 
CHIS2020_filtered <- CHIS2020 %>%
  filter(
    (AC115 != 2 | is.na(AC115)),
    !is.na(AC116bin)
  ) 

#exclude missing variables in the age categories 

CHIS2020_filtered <- CHIS2020_filtered %>%
  filter(!is.na(age_4cat))  




#Table 1 descriptive statistics only

table(CHIS2020_filtered$MJ_MONTH, CHIS2020_filtered$Restlessbin) #marijuana last 30 days

table(CHIS2020_filtered$age_4cat, CHIS2020_filtered$Restlessbin) #age frequency

table(CHIS2020_filtered$gendervar, CHIS2020_filtered$Restlessbin) #gender

table(CHIS2020_filtered$racecat, CHIS2020_filtered$Restlessbin) #race

table(CHIS2020_filtered$educcat, CHIS2020_filtered$Restlessbin) #education

table(CHIS2020_filtered$incomecat, CHIS2020_filtered$Restlessbin) #income

table(CHIS2020_filtered$employment, CHIS2020_filtered$Restlessbin) #employment

table(CHIS2020_filtered$MARIT, CHIS2020_filtered$Restlessbin) #marital

table(CHIS2020_filtered$Residence, CHIS2020_filtered$Restlessbin) #rural vs urban

table(CHIS2020_filtered$Smoking, CHIS2020_filtered$Restlessbin) #smoking

table(CHIS2020_filtered$illidrug, CHIS2020_filtered$Restlessbin) #illicit drugs

table(CHIS2020_filtered$AJ31, CHIS2020_filtered$Restlessbin) #restlessness

table(CHIS2020_filtered$RestlessHL, CHIS2020_filtered$Restlessbin) #restlesshigh low

table(CHIS2020_filtered$AC117V2, CHIS2020_filtered$Restlessbin) #days using marijuana

CHIS2020_filtered %>%
  group_by(age_4cat) %>%
  summarise(table(MJ_MONTH, Restlessbin))



#DATA ANALYSIS

#Survey model 1
survey_design <- svydesign(
  id = ~1,  
  strata = NULL,  # Stratify 
  weights = ~RAKEDW0, 
  repweights = paste0("RAKEDW", 1:80),  # Replicate weights, RAKEDW1 to RAKEDW80
  type = "JK1",  # Jackknife replication method
  data = CHIS2020_filtered 
)

table(CHIS2020_filtered$Restlessbin)
#Table 2 Model 1 unadjusted
model <- svyglm(AC116bin ~ Restlessbin, design = survey_design, family = binomial())
summary(model)
confint(model)

#Table 2 Model 2 - age and sex
model2 <- svyglm(AC116bin ~ Restlessbin + age_4cat + gendervar,design = survey_design, family = binomial())
summary(model2)
confint(model2)

#Table 2 Model 3 - sociodemographics
model3 <- svyglm(AC116bin ~ Restlessbin + age_4cat + gendervar + racecat + educcat + incomecat + employment + MARIT + Residence,
                 design = survey_design, family = binomial())
summary(model3)
confint(model3)

#Table 2 Model 4: age, sex, sociodemographics, + behaviors
model4 <- svyglm(AC116bin ~ Restlessbin + age_4cat + gendervar + racecat + educcat + incomecat + employment + MARIT + Residence
                 + Smoking + illidrug,
design = survey_design, family = binomial())
summary(model4)
confint(model4)

#survey model 2
survey_design2 <- svydesign(
  id = ~1,
  strata = ~age_4cat,  # Stratify by age
  weights = ~RAKEDW0, 
  repweights = paste0("RAKEDW", 1:80),  # Replicate weights, RAKEDW1 to RAKEDW80
  type = "JK1",  # Jackknife replication method
  data = CHIS2020_filtered  
)

#Table 3 Model 1 unadjusted
model5 <- svyglm(AC116bin ~ Restlessbin*age_4cat, design = survey_design2, family = binomial())
summary(model5)
confint(model5)

#Table 3 Model 2 - age and sex
model6 <- svyglm(AC116bin ~ Restlessbin* age_4cat + gendervar,design = survey_design2, family = binomial())
summary(model6)
confint(model6)

#Table 3 Model 3 - sociodemographics
model7 <- svyglm(AC116bin ~ Restlessbin + age_4cat + gendervar + racecat + educcat + incomecat + employment + MARIT + Residence,
                 design = survey_design2, family = binomial())
summary(model7)
confint(model7)

#Table 3 Model 4: age, sex, sociodemographics, + behaviors
model8 <- svyglm(AC116bin ~ Restlessbin + age_4cat + gendervar + racecat + educcat + incomecat + employment + MARIT + Residence
                 + Smoking + illidrug,
                 design = survey_design2, family = binomial())
summary(model8)
confint(model8)

#re-doing the AJ31 restlessness variable to make sure the RestlessHL variable is used in the stratification

unique(CHIS2020_filtered$AJ31)

CHIS2020_filtered <- CHIS2020_filtered %>%
  mutate(AJ31 = as.numeric(as.factor(AJ31)))

summary(CHIS2020_filtered$AJ31_num)

CHIS2020_filtered$AJ31 <- as.character(CHIS2020_filtered$AJ31)

CHIS2020_filtered <- CHIS2020_filtered %>%
  mutate(RestlessHL = case_when(
    AJ31 == "Not at all" ~ "No restlessness",
    AJ31 == "Some of the time" ~ "Low restlessness",
    AJ31 == "A little of the time" ~ "Low restlessness",
    AJ31 == "Most of the time" ~ "High restlessness",
    AJ31 == "All of the time" ~ "High restlessness", 
    TRUE ~ NA_character_))%>% 
  mutate(RestlessHL= factor(RestlessHL, levels = c("No restlessness", "Low restlessness", "High restlessness")))

levels(CHIS2020_filtered2$RestlessHL)

survey_design3 <- svydesign(
  id = ~1,  
  strata = ~RestlessHL, #Stratify by Restless high low
  weights = ~RAKEDW0,  
  repweights = paste0("RAKEDW", 1:80),  # Replicate weights, RAKEDW1 to RAKEDW80
  type = "JK1",  # Jackknife replication method
  data = CHIS2020_filtered2 
)

#Table 4 Model 1

model9 <- svyglm(AC116bin ~ RestlessHL, design = survey_design3, family = binomial())
summary(model9)
confint(model9)

#Table 4 Model 2 - age and sex

model10 <- svyglm(AC116bin ~ RestlessHL + gendervar,design = survey_design3, family = binomial())
summary(model10)
confint(model10)

#Table 4 Model 3 - sociodemographics
model11 <- svyglm(AC116bin ~ RestlessHL + age_4cat + gendervar + racecat + educcat + incomecat + employment + MARIT + Residence,
                 design = survey_design3, family = binomial())
summary(model11)
confint(model11)

#Table 4 Model 4: age, sex, sociodemographics, + behaviors
model12 <- svyglm(AC116bin ~ RestlessHL + age_4cat + gendervar + racecat + educcat + incomecat + employment + MARIT + Residence
                 + Smoking + illidrug,
                 design = survey_design3, family = binomial())
summary(model12)
confint(model12)

nrow(CHIS2020_filtered)
table(CHIS2020_filtered$MJ_MONTH, CHIS2020_filtered$RestlessHL)

#wish reference values were mentioned in paper in terms of marijuana, and no specification of varmethod jackknife
#found out about most of how they coded/analyzed not from their paper but via code
#results for later models varied a lot from their results
#seems there are two observations listed as na / other and that makes the diff between 12098 and 12100