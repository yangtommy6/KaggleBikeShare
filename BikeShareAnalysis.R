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

rFormula = 

myrecipe <- recipe(~., data=bike) %>% 
  step_date(datetime, features="dow") %>% # gets day of week5
  step_time(datetime, features=c("hour", "minute"))
prepped_recipe <- prep(myrecipe) # Sets up the preprocessing using myDataSet14
bakedData <- bake(prepped_recipe, new_data= NULL)
view(bakedData)





