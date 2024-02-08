library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

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




#plot 1 count of height_cm
ggplot(starwars, aes(x = height_cm)) +
  geom_histogram(binwidth = 10) +
  ylim(0, 22.5)

#plot 2

ggplot(starwars, aes(x = fct_infreq(hair))) +
  geom_bar() +
  labs(title = "plot 2", 
       x = "Sorted_hair", 
       y = "Count")

#plot 3
ggplot(data = starwars %>%
         filter(mass<=500),
       aes(x = height_in, y = mass)) +
  geom_point(shape = 17, size = 2) +
  labs(x = "Height (in)", y = "Mass")


#Assignment 12
#plot 1
ggplot(starwars <- starwars %>% filter(mass<=500), aes(x = fct_infreq(hair), y = mass, fill = hair)) +
  geom_boxplot() +
  geom_point(shape = 19, size = 1) +  
  labs(
    x = "Hair Color",
    y = "Mass",
    fill = "Colorful Hair"
  ) +
  scale_fill_discrete(breaks = c("none", levels(fct_infreq(starwars$hair)))) #This line change the
#order of the legend
                 



#plot2
ggplot(starwars, aes(x = mass, y = height_in)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  # Add a trend line
  labs(
    title = "Mass vs. height by brown-hair-havingness ",
    subtitle = "a critical important analysis"
  ) +
  facet_wrap(~ ifelse(brown_hair, "Has brown hair", "No brown hair"), ncol = 2) +
  theme_minimal() +
  coord_cartesian(xlim = c(-200, 200), ylim = c(-4, NA))  # Set x and y-axis limits

#plot3

# Create a bar chart of the count of species' first letters, colored by gender
starwars_filtered_NA <- sw.wrangled.goal %>%
  filter(!is.na(gender) | !is.na(species))
ggplot(starwars_filtered_NA, aes(x = substr(species, 1, 1), fill = gender)) +
  geom_bar() +
  labs(
    x = "species_first_letter",
    y = "count",
    fill = "gender"
  ) +
  coord_flip() +
  annotate(
    geom = "text",
    x = 1,
    y = 30,
    label = "a clear male human bias"
  )
  theme_minimal() 

#Assign 13 plot
  ggplot(starwars_modified, aes(x = height, y = mass, color = gender)) +
    geom_jitter() + # Adds jitter to the plot
    geom_smooth(method = "lm", se = TRUE, aes(fill = gender)) + 
    facet_wrap(~gender, scales = "free", strip.position = "top", 
               labeller = labeller(gender = c(masculine = "Male", feminine = "Female", none = "Other"))) +
    theme_light(base_size = 8) +
    theme(
      panel.background = element_rect(fill = "lightpink"),
      strip.background = element_rect(fill = "darkgreen", color = "darkgreen"),
      strip.text = element_text(color = "white"),
      legend.position = "bottom",
      text = element_text(family = "Annai MN"), # Change font family to SimSun for example
      legend.title = element_text(family = "Annai MN"), # Apply font settings to legend title specifically
      legend.text = element_text(family = "Annai MN"),
      panel.grid.major.y = element_line(color = "darkgrey", linetype = "dotdash"), # White dotted horizontal lines
      panel.grid.major.x = element_line(color = "white", linetype = "dashed")
    ) +
    labs(
      x = "Height (cm)", 
      y = "Mass (kg)", 
      title = "Height and weight across gender presentation", 
      subtitle = "A cautionary tale in misleading 'free' axis scales and bad design choices",
      color = "Gender Presentation", 
      fill = "Gender Presentation" 
    ) +
    scale_color_manual(values = c("masculine" = "grey", "feminine" = "darkred", "none" = "orange"),
                       labels = c("Male", "Female", "Other")) + #color scale for points
    scale_fill_manual(values = c("masculine" = "lightblue", "feminine" = "lightblue", "none" = "lightblue"),
                      labels = c("Male", "Female", "Other")) #fill scale for linear regression areas
  
  







## Check that your sw.wrangled df is identical to the goal df
# Use any returned information about mismatches to adjust your code as needed
all.equal(sw.wrangled.goal, sw.wrangled.goal)

