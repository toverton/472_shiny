# Packages:
library(rsample)    
library(ggplot2)  
library(earth)     
library(caret)     
library(vip)       
library(pdp)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(reshape2)

#-----------------------------------Datasets------------------------------------------------------------
gunViolence = read_csv("final_df.csv")
yearly_pop = read_excel("yearly_pop.xlsx")

#-------------------------------------Data Cleaning------------------------------------------------------
gunViolence = na.omit(gunViolence)

yearly_pop |> 
  filter(!row_number() %in% seq(1:8)) |> 
  select(-c(2, 3, 4, 5, 6, 13)) |> 
  rename(state = 1, 
         "2013" = 2, 
         "2014" = 3,
         "2015" = 4,
         "2016" = 5,
         "2017" = 6,
         "2018" = 7) |> 
  na.omit() -> yearly_pop_final

remove_state_periods = function(a_state_str) {
  a_state_str |> 
    str_replace("\\.", "") -> period_rm_state
  return(period_rm_state)
}

yearly_pop_final$state = sapply(yearly_pop_final$state, remove_state_periods)

yearly_pop_final |>
  melt(na.rm = FALSE, value.name = "state_population", id = "state") |>
  rename(year = variable) -> yearly_pop_melt

yearly_pop_melt$year = as.numeric(as.character(yearly_pop_melt$year))

gunViolence = gunViolence |>
  inner_join(yearly_pop_melt, by=c("state","year"))

#adding number killed per 100,000
gunViolence |>
  mutate(per_hthous_killed = ((n_killed / state_population)*100000)) -> gunViolence

#-----------------------------------MARS----------------------------------------------------------
gunViolence2 = select(gunViolence, 
                     -incident_characteristics, -location_description, 
                     -incident_id, -date, 
                     -participant_age_group, -per_hthous_killed,
                     -participant_age, -participant_gender, 
                     -gun_type)


# Randomly selected 30% of the data
N = round(nrow(gunViolence2) * 0.50)
sampleIndices = sample(1:nrow(gunViolence2), size = N, replace = FALSE)

# New data 
gunViolence_sampled = gunViolence2[sampleIndices, ]
gunViolence_sampled = gunViolence_sampled[order(gunViolence_sampled$year), ]


MARS = earth(n_killed ~ ., data = gunViolence_sampled, degree = 2)
plot(x = MARS, which = 1)


cv_model1 = train(
  n_killed ~ ., 
  data = gunViolence_sampled, 
  method = "lm",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale")
)

#-------------------------------------------------------------------------------------------------------

train_indices = sample(1:nrow(gunViolence_sampled), size = .8 * nrow(gunViolence_sampled), replace = FALSE)
gv_train = gunViolence_sampled[train_indices,]
gv_test = gunViolence_sampled[-train_indices,]

gv_train = gv_train[order(gv_train$year), ]
gv_test = gv_test[order(gv_test$year), ]


x = gv_train[, -7]
y = gv_train[, 7]    


parameter_grid = floor(expand.grid(degree = 1:4, nprune = seq(5, 50, by = 5)))

cv_gv = train(x = x, y = gv_train$n_killed, 
              method = "earth",
              metric = "Rsquared", 
              trControl = trainControl(method = "cv", number = 10), 
              tuneGrid = parameter_grid)                                   


#----------------------------------------------------------------------------------------------------

# Check class of gunViolence_sampled
class_gv_sampled <- class(gunViolence_sampled)
print(class_gv_sampled)

# Check if it's a tibble
if ("tbl_df" %in% class_gv_sampled) {
  # Convert tibble to data frame
  gunViolence_sampled <- as.data.frame(gunViolence_sampled)
} else if ("data.frame" %in% class_gv_sampled) {
  # It's already a data frame
  print("gunViolence_sampled is already a data frame.")
} else {
  stop("gunViolence_sampled is neither a tibble nor a data frame.")
}

# Sample indices for training data
train_indices <- sample(1:nrow(gunViolence_sampled), 
                        size = 0.8 * nrow(gunViolence_sampled), 
                        replace = FALSE)

# Split data into training and test sets
gv_train <- gunViolence_sampled[train_indices, ]
gv_test <- gunViolence_sampled[-train_indices, ]

# Define predictors (x) and target (y) for training
x <- gv_train[, -7]
y <- gv_train[, 7]

# Define parameter grid for tuning
parameter_grid <- expand.grid(degree = 1:4, nprune = seq(5, 50, by = 5))

# Load required libraries
library(earth)     
library(caret)     

# Train the model using cross-validation
cv_gv <- train(x = x, 
               y = y, 
               method = "earth",
               metric = "Rsquared", 
               trControl = trainControl(method = "cv", number = 10), 
               tuneGrid = parameter_grid)

#-------------------------------------------------------------------------------------------------------





