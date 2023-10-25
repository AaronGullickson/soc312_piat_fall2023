# SOC 312, Fall 2023
# Organize the GSS data on environmental activism


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(mice)
set.seed(42)


# Read data ---------------------------------------------------------------

yrbs <- read_fwf(here("input","yrbs","XXH2021_YRBS_Data.dat"),
                 col_positions =
                   fwf_positions(
                     start=c(56, 116, 109, 17, 18, 414, 55, 117, 108),
                     end  =c(56, 116, 109, 17, 18, 415, 55, 117, 108),
                     col_names=c("feel_sad","mental_health", "screen_time",
                                 "age", "gender", "race", "bully_electronic",
                                 "sleep", "physical_activity")))



# Code data ---------------------------------------------------------------

yrbs <- yrbs |>
  mutate(feel_sad=feel_sad==1,
         #mental_health=factor(mental_health, levels=1:5,
         #                     labels=c("Never","Rarely","Sometimes",
         #                              "Most of the time","Always")),
         screen_time=factor(screen_time, levels=1:6,
                            labels=c("Less than 1 hr/day","1 hr/day",
                                     "2 hr/day","3 hr/day","4 hr/day",
                                     "5 or more hrs/day")),
         age=age+11,
         gender=factor(gender, levels=1:2, labels=c("Female","Male")),
         race=factor(case_when(
           race==1 | race==4 ~ "Indigenous",
           race==2 ~ "Asian",
           race==3 ~ "Black",
           race==5 ~ "White",
           race==6 ~ "Latino",
           race==7 | race==8 ~ "Multiple"),
           levels=c("White","Black","Latino","Asian","Indigenous","Multiple")),
        bully_electronic=bully_electronic==1,
        sleep=sleep+3,
        physical_activity=physical_activity-1
  ) |>
  select(mental_health, screen_time, age, gender, race,  bully_electronic,
         sleep, physical_activity) |>
  filter(!is.na(mental_health))

temp <- mice(yrbs, 1)
yrbs <- as_tibble(complete(temp, 1))

yrbs <- yrbs |>
  mutate(bully_electronic=factor(bully_electronic, levels=0:1,
                                 labels=c("No","Yes")))


save(yrbs, file=here("output","yrbs.RData"))
