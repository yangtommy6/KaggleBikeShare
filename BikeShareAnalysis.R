library(tidyverse) #specifically the dplyr library1
library(vroom)
library(lubridate)
bike <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/train.csv")
view(bike)

myCleanData <- bike %>%
  mutate(hour = hour(datetime)) # Hour

view(myCleanData)


##Feature engineering

library(tidymodels)
my_recipe <- recipe(rFormula, data=myDataSet) %>% # Set model formula and d2
  step_mutate(newVar=var1*var2) %>% #Create a new variable3
  step_poly(var, degree=2) %>% #Create polynomial expansion of var4
  step_date(timestamp, features="dow") %>% # gets day of week5
  step_time(timestamp, features=c("hour", "minute")) %>%