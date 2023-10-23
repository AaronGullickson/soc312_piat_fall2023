# SOC 312, Fall 2023
# Organize the GSS data on environmental activism


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(mice)
set.seed(42)

# Read data ---------------------------------------------------------------

gss <- read_fwf(here("input","gss","GSS.dat"),
                col_positions =
                  fwf_positions(
                    start=c(1, 6,11,18,22,27,31,35,40,42,47,52,57,62),
                    end=  c(5,10,15,21,26,30,34,39,41,46,51,56,61,65),
                    col_names=c("HISPANIC","RACEN1","RACEN2","MARITAL",
                                "AGE","EDUC","DEGREE","SEX","REGION",
                                "GRNGROUP","GRNSIGN","GRNMONEY","GRNDEMO",
                                "INCOME16")))


# Recode variables --------------------------------------------------------

gss <- gss |>
  mutate(hispanic=ifelse(HISPANIC<0, NA, HISPANIC>1),
         race=factor(case_when(
           hispanic==TRUE| RACEN1==16 ~ "Latino",
           RACEN2>0 ~ "Multiple",
           RACEN1==1 ~ "White",
           RACEN1==2 ~ "Black",
           RACEN1==3 ~ "American Indian/Alaska Native",
           RACEN1==15 ~ "Other",
           RACEN1>4 ~ "Asian/Pacific Islander"),
           levels=c("White","Latino","Black","Asian/Pacific Islander",
                    "American Indian/Alaska Native","Other","Multiple")),
         gender=factor(SEX, levels=2:1, labels=c("Female","Male")),
         age=ifelse(AGE<0, NA, AGE),
         educ=ifelse(EDUC<0, NA, EDUC),
         degree=factor(DEGREE, levels=0:4,
                       labels=c("None","HS","AA","BA","Grad")),
         marital=factor(case_when(
           MARITAL==1 ~ "Married",
           MARITAL==2 ~ "Widowed",
           MARITAL==3 | MARITAL==4 ~ "Divorced/Separated",
           MARITAL==5 ~ "Never married"),
           levels=c("Never married","Married","Divorced/Separated",
                    "Widowed")),
         region=factor(case_when(
           REGION<3 ~ "Northeast",
           REGION>=3 & REGION<=4 ~ "Midwest",
           REGION>=5 & REGION<=7 ~ "South",
           REGION>=8 ~ "West")),
         income=ifelse(INCOME16<0, NA, INCOME16),
         grngroup=ifelse(GRNGROUP<0, NA, GRNGROUP==1),
         grnsign=ifelse(GRNSIGN<0, NA, GRNSIGN==1),
         grnmoney=ifelse(GRNMONEY<0, NA, GRNMONEY==1),
         grndemo=ifelse(GRNDEMO<0, NA, GRNDEMO==1),
         active_enviro=grngroup+grnsign+grnmoney+grndemo) |>
  select(age, gender, educ, degree, race, marital, region, income,
         active_enviro) |>
  filter(!is.na(active_enviro))

# use mice to impute data

temp <- mice(gss, 1)
gss <- as_tibble(complete(temp, 1))

# see where the cutoffs are for quintiles of the income distribution
cumsum(prop.table(table(gss$income)))

# 1-14 - quintile 1
# 15-18 - quintile 2
# 19-20 - quintile 3
# 21-23 - quintile 4
# 24-25 - quintile 5

gss <- gss |>
  mutate(income_quintile = factor(case_when(
    income<15 ~ "less than 25K",
    income>=15 & income<=18 ~ "25K-50K",
    income>=19 & income<=20 ~ "50K-75K",
    income>=21 & income<=23 ~ "75K-130K",
    income>23 ~ "greater than 130K"),
    levels=c("less than 25K", "25K-50K", "50K-75K", "75K-130K",
             "greater than 130K"))) |>
  select(active_enviro, income_quintile, age, gender, educ, race, marital,
         region)

save(gss, file=here("output","gss.RData"))
