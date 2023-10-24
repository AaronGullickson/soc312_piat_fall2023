# SOC 312, Fall 2023
# Organize the GSS data on environmental activism


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)
library(mice)
set.seed(42)


# Read data ---------------------------------------------------------------

nlsy <- read_csv(here("input","nlsy97","piat-divorce.csv"))


# Code data ---------------------------------------------------------------

# get income data
data_year <- nlsy |>
  select(R0000100, R5464100, R7227800, S1541700, S2011500, S3812400, S5412800,
         S7513700, T0014100, T2016200, T3606500, T5206900, T6656700,
         T8129100, U0008900, U1845500, U3444000) |>
  rename(pid=R0000100, `2000`=R5464100, `2001`=R7227800, `2002`=S1541700,
         `2003`=S2011500, `2004`=S3812400, `2005`=S5412800, `2006`=S7513700,
         `2007`=T0014100,`2008`=T2016200, `2009`=T3606500, `2010`=T5206900,
         `2011`=T6656700,`2013`=T8129100, `2015`=U0008900, `2017`=U1845500,
         `2019`=U3444000) |>
  pivot_longer(!pid, names_to="year", values_to="income") |>
  mutate(year=as.numeric(year),
         income=ifelse(income<0, NA, income/1000))

# get urban-rural data
data_year <- nlsy |>
  select(R0000100, R5484100, R7248400, S1564300, S2034400, S3835800,
         S5436300, S7537100, T0033700, T2021300, T3612000, T5211900,
         T6663300, T8134500, U0015000, U1853200, U3453600) |>
  rename(pid=R0000100, `2000`=R5484100, `2001`=R7248400, `2002`=S1564300,
         `2003`=S2034400, `2004`=S3835800,`2005`=S5436300, `2006`=S7537100,
         `2007`=T0033700, `2008`=T2021300, `2009`=T3612000, `2010`=T5211900,
         `2011`=T6663300, `2013`=T8134500, `2015`=U0015000, `2017`=U1853200,
         `2019`=U3453600) |>
  pivot_longer(!pid, names_to="year", values_to="urbanicity") |>
  mutate(year=as.numeric(year),
         urbanicity=factor(urbanicity, levels=0:1,
                           labels=c("Rural","Urban"))) |>
  left_join(data_year)



nlsy <- nlsy |>
  mutate(
    pid=R0000100,
    birth_month=R0536401,
    birth_year=R0536402,
    birth_date=birth_year+(birth_month-0.5)/12,
    mar_month=ifelse(Z9073200<1, NA, Z9073200),
    mar_year=ifelse(Z9073201<1, NA, Z9073201),
    mar_date=mar_year+(mar_month-0.5)/12,
    div_month=ifelse(Z9073500<1, NA, Z9073500),
    div_year=ifelse(Z9073501<1, NA, Z9073501),
    div_date=div_year+(div_month-0.5)/12,
    year_last_interview=case_when(
      Z9085100==1 ~ 1998,
      Z9085100==2 ~ 1999,
      Z9085100==3 ~ 2000,
      Z9085100==4 ~ 2001,
      Z9085100==5 ~ 2002,
      Z9085100==6 ~ 2003,
      Z9085100==7 ~ 2004,
      Z9085100==8 ~ 2005,
      Z9085100==9 ~ 2006,
      Z9085100==10 ~ 2007,
      Z9085100==11 ~ 2008,
      Z9085100==12 ~ 2009,
      Z9085100==13 ~ 2010,
      Z9085100==14 ~ 2011,
      Z9085100==15 ~ 2012,
      Z9085100==16 ~ 2014,
      Z9085100==17 ~ 2016,
      Z9085100==18 ~ 2018,
      Z9085100==19 ~ 2020
    ),
    mar_length=ifelse(is.na(div_date), year_last_interview-mar_date+0.5,
                      div_date-mar_date),
    age_mar=mar_date-birth_date,
    raised_two_bio=factor(R0532200, levels=0:1, labels=c("No","Yes")),
    race=factor(R1482600, levels=c(4,1,2,3),
                labels=c("Non-Black/Non-Hispanic","Black","Hispanic",
                         "Mixed Race")),
    high_grade=ifelse(Z9083800==95 | Z9083800<0, NA, Z9083800)
  ) |>
  # remove unmarried and one case of negative mar_length
  filter(!is.na(mar_date) & mar_length>0) |>
  # remove a few very young ages at marriage and marriages before 2000
  filter(age_mar>=16 & mar_date>=2000) |>
  # remove marriages of less than five years that did not end in divorce
  filter(!is.na(div_date) | mar_length>=5) |>
  # create dv
  mutate(divorce_five=!is.na(div_date) & mar_length<=5) |>
  select(pid, mar_date, div_date, mar_length, age_mar, divorce_five,
         raised_two_bio, race, high_grade)


# merge income data from the wave after the date
nlsy <- nlsy |>
  mutate(year=ceiling(mar_date),
         year=ifelse(year %in% c(2012,2014,2016,2018), year+1, year)) |>
  left_join(data_year)

# create final dataset
nlsy <- nlsy |>
  select(divorce_five, high_grade, urbanicity, income, age_mar, raised_two_bio,
         race)


# impute missing values
temp <- mice(nlsy, 1)
nlsy <- as_tibble(complete(temp, 1))

save(nlsy, file=here("output",'nlsy.RData'))
