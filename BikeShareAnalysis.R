library(tidyverse) #specifically the dplyr library1
library(vroom)
library(lubridate)
bike <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/train.csv")


myCleanData <- bike %>%
  mutate(hour = hour(datetime)) # Hour




##Feature engineering

library(tidymodels)

rFormula = 

myrecipe <- recipe(~., data=bike) %>% 
  step_date(datetime, features="dow") %>% # gets day of week5
  step_time(datetime, features=c("hour", "minute"))
prepped_recipe <- prep(myrecipe) # Sets up the preprocessing using myDataSet14
bakedData <- bake(prepped_recipe, new_data= NULL)




library(tidymodels)
my_mod<- linear_reg() %>% #Type of model
  set_engine ("lm") # Engine = What R function to use

bike_workflow <- workflow() %>%
  add_recipe (myrecipe) %>%
  add_model (my_mod) %>%
  fit(data = myDataSet) # Fit the workflow
bike_predictions <- predict(bike_workflow,
                             new_data=myNewData) # Use fit to predict



## Read in the data
bikeTrain <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/train.csv")
bikeTest <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)
## Cleaning & Feature Engineering
bike_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors())%>%
  step_rm(datetime)
prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = bikeTrain) #Make sure recipe work on train
bake(prepped_recipe, new_data = bikeTest) #Make sure recipe works on test
## Define the model
lin_model <- linear_reg() %>%
  set_engine("lm")
## Set up the whole workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=bikeTrain)
## Look at the fitted LM model this way
extract_fit_engine(bike_workflow) %>%
  summary()
## Get Predictions for test set AND format for Kaggle
test_preds <- predict(bike_workflow, new_data = bikeTest) %>%
  bind_cols(., bikeTest) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write prediction file to CSV
vroom_write(x=test_preds, file="/Users/christian/Desktop/STAT348/yoyoyo.csv", delim=",")



#sep20
library(tidymodels)
library(poissonreg) #if you want to do penalized, poisson regression
## Create a recipe
my_recipe <- recipe(~., data=bike) %>%
  step_dummy(all_nominal_predictors()) %>% #make dummy variables
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1

## Penalized regression model
preg_model <- linear_reg(penalty=0.5, mixture=0.5) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=bikeTrain)
predict(preg_wf, new_data=bikeTest)

result <- predict(preg_wf, new_data = bikeTest)%>%
  bind_cols(., bikeTest)%>%
  select(datetime, .pred)%>%
  rename(count=.pred)%>%
  mutate(count=pmax(0,count))%>%
  mutate(datetime=as.character(format(datetime)))
vroom_write(x=result,file="/Users/christian/Desktop/STAT348/KaggleBikeShare/TestPreds1.csv", delim=",")
result


library(tidymodels)
##################################################
#0927

# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(rpart)
train <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/train.csv")
test <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/test.csv")

train <- select(train, -c(casual, registered))

# Convert datetime to DateTime type
train$datetime <- as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")
test$datetime <- as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M:%S")

# Define a recipe
recipe <- recipe(count ~ ., data = train) %>%
  step_date(datetime) %>%
  step_rm(datetime)


# Define a model with tuning parameter
model <- decision_tree(cost_complexity = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Create a workflow
workflow <- workflow() %>% 
  add_model(model) %>% 
  add_recipe(recipe)

# Set up grid of tuning values
grid <- grid_regular(
  cost_complexity(),
  levels = 10
)

# Set up K-fold Cross-Validation
folds <- vfold_cv(train, v = 3)

# Find best tuning parameters
res <- tune_grid(
  workflow,
  resamples = folds,
  grid = grid
)

best_params <- res %>% select_best("rmse")

# Finalize workflow and predict
final_workflow <- workflow %>% finalize_workflow(best_params)
fit <- final_workflow %>% fit(data = train)

predictions <- fit %>% predict(new_data = test)



submission <- tibble(
  datetime = test$datetime,
  count = predictions$.pred
)

write.csv(submission, "/Users/christian/Desktop/STAT348/KaggleBikeShare/Submission3.csv", row.names = FALSE)



#########929

write.csv(submission, "/Users/christian/Desktop/STAT348/KaggleBikeShare/Submission4.csv", row.names = FALSE)

#Create a workflow with model and recipe

#set up grid of tuning values
#Setup grid of tuning values
#setup K-fold CV
#fIND BEST Tuning parameters
#Finalize workflow and predict

library(tidyverse)
library(tidymodels)
library(ranger)
library(vroom)
library(lubridate)
library(rpart)

train_data <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/train.csv")
test_data <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/test.csv")


rec <- recipe(count ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")


workflow <- workflow() %>%
  add_model(model) %>%
  add_recipe(rec)

cv_folds <- vfold_cv(train_data, v = 5)

grid <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(1, 10)),
  levels = 5  # This will create a 5x5 grid, adjust as needed
)

tune_results <- tune_grid(
  workflow,
  resamples = cv_folds,
  grid = grid
)

best_params <- tune_results %>%
  select_best("metric_of_interest")  # replace with your chosen metric, e.g., rmse

final_workflow <- workflow %>%
  finalize_workflow(best_params)

fit <- final_workflow %>% fit(data = train)

predictions <- fit %>% predict(new_data = test)



submission <- tibble(
  datetime = test_data$datetime,
  count = predictions$.pred
)

write.csv(submission, "/Users/christian/Desktop/STAT348/KaggleBikeShare/Submission4.csv", row.names = FALSE)
########################################################
# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(rpart)
train <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/train.csv")
test <- vroom("/Users/christian/Desktop/STAT348/KaggleBikeShare/test.csv")

train <- select(train, -c(casual, registered))

# Convert datetime to DateTime type
train$datetime <- as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")
test$datetime <- as.POSIXct(test$datetime, format="%Y-%m-%d %H:%M:%S")

# Define a recipe
recipe <- recipe(count ~ ., data = train) %>%
  step_date(datetime) %>%
  step_rm(datetime)


# Define a model with tuning parameter
model <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Create a workflow
workflow <- workflow() %>% 
  add_model(model) %>% 
  add_recipe(recipe)

# Set up grid of tuning values
grid <- grid_regular(
  cost_complexity(),
  levels = 10
)

# Set up K-fold Cross-Validation
folds <- vfold_cv(train, v = 3)

# Find best tuning parameters
res <- tune_grid(
  workflow,
  resamples = folds,
  grid = grid
)

best_params <- res %>% select_best("rmse")

# Finalize workflow and predict
final_workflow <- workflow %>% finalize_workflow(best_params)
fit <- final_workflow %>% fit(data = train)

predictions <- fit %>% predict(new_data = test)



submission <- tibble(
  datetime = test$datetime,
  count = predictions$.pred
)

write.csv(submission, "/Users/christian/Desktop/STAT348/KaggleBikeShare/Submission3.csv", row.names = FALSE)



