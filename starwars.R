library(tidyverse)
library(tidyr)
library(dplyr)

## Create your goal tibble to replicate

# Run this line to see what your end product should look like
sw.wrangled.goal <- read_csv("~/Documents/Github/d2m-2024/data/sw-wrangled.csv") %>% 
  mutate(across(c(hair, gender, species, homeworld), factor)) # this is a quick-and-dirty fix to account for odd importing behavior

# View in console
sw.wrangled.goal 

# Examine the structure of the df and take note of data types
# Look closely at factors (you may need another function to do so) to see their levels
str(sw.wrangled.goal) 
str(starwars)



## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled
# If you get stuck, use comments to "hold space" for where you know code needs to go to achieve a goal you're not sure how to execute
#View the starwar dataset
View(starwars)
#Sepreate the name column into first name and last name.
starwars <- starwars %>%
  separate(name, into = c("first_name", "last_name"), sep = " ")
#Create initial column based on first and last name
starwars <- starwars %>%
  mutate(initial = paste0(substr(first_name, 1, 1), substr(last_name, 1, 1)))
#covert the height from cm to in
starwars <- starwars %>%
  mutate(height_in = round(height / 2.54, 4),  # Convert cm to inches and round to 4 decimal places
         height_cm = height,
         height = NULL)
#Change the NA in the haricolor column into 'none'
starwars <- starwars %>%
  mutate(hair_color = replace_na(hair_color, "none"))
#Show only the first letter of column 'gender' 
starwars <- starwars %>%
  mutate(gender = substr(gender, 1, 1))
#Create brown_hair, print out True if it is brown 
starwars <- starwars %>%
  mutate(brown_hair = hair_color == "brown")
#Arrange the columns based on last name
starwars <- starwars %>%
  arrange(last_name)
#delete unwnated columns
starwars <- starwars %>%
  select(-skin_color,-eye_color,-birth_year,-sex,-vehicles,-films,-starships )
#rename hair_color
starwars <- starwars %>%
  rename(hair = hair_color)
#arrange columns in specific orders
starwars <- starwars %>%
  select(first_name,last_name,initial,height_in,height_cm,mass,hair,gender,species,homeworld,brown_hair)

#delect all rows with height = NA
starwars <- starwars %>%
  filter(!is.na(height_cm))















## Check that your sw.wrangled df is identical to the goal df
# Use any returned information about mismatches to adjust your code as needed
all.equal(sw.wrangled.goal, sw.wrangled.goal)

