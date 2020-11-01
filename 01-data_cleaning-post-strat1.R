#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA
# Author: Yifei Zhang, Ziyi Qu, Peilin Chen, Ziru Nie
# Data: 22 October 2020
# Contact: zoei.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####

library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/zhangyf/Desktop/STA304 A3")
raw_data <- read_dta("/Users/zhangyf/Desktop/STA304 A3/usa_00003.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(sex, 
         age, 
         race,
         educ,
         labforce)
         

#### What's next? ####
# change age to integer
reduced_data$age <- as.integer(reduced_data$age)

#Only select those who are eligible to vote by assuming only people older than 17 are eligible to vote. 
reduced_data <- reduced_data %>% filter(age >= 18)

# change education status to college status
reduced_data <- reduced_data %>% 
  mutate_at(vars(educ), .funs = funs(case_when(
    .=="n/a or no schooling"~"under college",
    .=="nursery school to grade 4"~"under college",
    .=="grade 5, 6, 7, or 8"~"under college",
    .=="grade 9"~"under college",
    .=="grade 10"~"under college",
    .=="grade 11"~"under college",
    .=="grade 12"~"under college",
    .=="1 year of college"~"college or above",
    .=="2 years of college"~"college or above",
    .=="3 years of college"~"college or above",
    .=="4 years of college"~"college or above",
    .=="5+ years of college"~"college or above"
  )))

# rename educ to education
reduced_data <- reduced_data %>% 
  clean_names() %>% 
  rename(education = educ,
  )

reduced_data <- na.omit(reduced_data)

## Here I am only splitting cells by age, sex, education, race, and labforce
reduced_data <- 
  reduced_data %>%
  count(age, sex, education,race, labforce) %>%
  group_by(age, sex, education,race, labforce) 

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



         