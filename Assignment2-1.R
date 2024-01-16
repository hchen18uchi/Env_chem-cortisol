library("tidyverse")
library("papaja")
## This line is a comment
## This line is also a comment
Number_of_cars <- 40
Number_of_bikes <- 50
Car_brand <- "BMW"
Owner <- "car dealer"

helloworld <- function(firstname,lastname,sex){
  #Print a greeting line based on name
  sex <- ifelse( sex == "Female", "Ms.",ifelse( sex == "Male","Mr."))
  greeting <- paste("Hello", sex,firstname,lastname,"!")
}