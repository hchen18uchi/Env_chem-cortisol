#The ideal dataset is pretty close to what I am having right now because my dataset is already a 
#prepared data. It combined several sepreate dataset from the database including the demographic,
# the depression measurment and all the urine measurement I might need from the database. The main
# coding would be select, filter, start with, some calculation and some plot. 
require(readr)
require(dplyr)
library(readr)
library(dplyr)
nhanes_data <- read_csv("~/Documents/Github/Env_chem-cortisol/nhanes2017.csv")

#Goal: 1.change all depression score of 7,9 to NA and add up the depression score.
#2. switch place for all the 0 and 1 in urine smaple.or count the number of zero so I can get a 
#sum of of many index each participants exceed detection limits.

#select the columns I want
fix_nhanes_data <- nhanes_data %>%
  select(starts_with("DPQ"), starts_with("UR"), starts_with("SE"), starts_with("RIAGENDR"))

#change all the 7 and 9 into NA in all DPQ columns
fix_nhanes_data <- fix_nhanes_data %>%
  mutate(across(starts_with("DPQ"), ~na_if(., 7))) %>%
  mutate(across(starts_with("DPQ"), ~na_if(., 9)))
#add up all the DPQ score
fix_nhanes_data <- fix_nhanes_data %>%
  mutate(DPQsum = rowSums(select(., starts_with("DPQ")), na.rm = TRUE))
#switch all 0 into 1 and 1 into 0 in all columns end with LC
fix_nhanes_data <- fix_nhanes_data %>%
  mutate(across(ends_with("LC"), ~ if_else(!is.na(.), 1 - ., .)))
#add up all the LC score
fix_nhanes_data <- fix_nhanes_data %>%
  mutate(LCsum = rowSums(select(., ends_with("LC")), na.rm = TRUE))




