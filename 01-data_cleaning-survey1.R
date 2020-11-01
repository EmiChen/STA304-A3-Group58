#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund + UCLA Nationscape
# Author: Yifei Zhang, Ziyi Qu, Peilin Chen, Ziru Nie
# Data: 22 October 2020
# Contact: zoei.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(janitor)
setwd("/Users/zhangyf/Desktop/STA304 A3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(employment,
         registration,
         foreign_born,
         gender,
         vote_2020,
         extra_race_white_you,
         education,
         age)


reduced_data<-reduced_data %>% 
  filter(registration=="Registered")

#make vote a binary variable         
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0)) 

# change people's answers who said "Yes" to "White", and other answers to "Not White"
reduced_data<-
  reduced_data %>%
  mutate(race = 
           ifelse(extra_race_white_you=="Yes", "White", "Not White"))

# change employment status to laborforce status
reduced_data <- reduced_data %>% 
  mutate_at(vars(employment), .funs = funs(case_when(
    .=="Full-time employed"~"in laborforce",
    .=="Unemployed or temporarily on layoff"~"in laborforce",
    .=="Part-time employed"~"in laborforce",
    .=="Self-employed"~"in laborforce",
    .=="Homemaker"~"not in laborforce",
    .=="Retired"~"not in laborforce",
    .=="Permanently disabled"~"not in laborforce",
    .=="Student"~"not in laborforce",
    .=="Other"~"not in laborforce",
  )))

# rename variable employment to name
reduced_data <- reduced_data %>% 
  clean_names() %>% 
  rename(laborforce = employment,
  )

# change education status to college status 
reduced_data <- reduced_data %>% 
  mutate_at(vars(education), .funs = funs(case_when(
    .=="3rd grade or less"~"under college",
    .=="Middle school-Grades4-8"~"under college",
    .=="Completed some high school"~"under college",
    .=="High school graduate"~"under college",
    .=="Other post high school vocational training"~"under college",
    .=="Completed some college, but no degree"~"college or above",
    .=="Associate Degree"~"college or above",
    .=="College Degree (such as B.A., B.S.)"~"college or above",
    .=="Completed some graduate, but no degree"~"college or above",
    .=="Masters degree"~"college or above",
    .=="Doctorate degree"~"college or above"
  )))

# omit na
reduced_data <- na.omit(reduced_data)
  
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

