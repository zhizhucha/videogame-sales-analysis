#' title: "VideoGames Scores'
#' author: "Quentin Cartier"
#' date: "July 27th, 2021"


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
#         I observe about 10 minutes on my laptop,
#         The regularization part especially require some time, as it need to
#         iterate through multiple value of lambda

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


# Video Game Sales with Ratings:
# https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings
#download.file("https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings", dl)


data <- readr::read_csv(unzip("Video_Games_Sales_as_at_22_Dec_2016.csv.zip","Video_Games_Sales_as_at_22_Dec_2016.csv" ),
                         col_names = TRUE, guess_max = 10000)



data <- tibble(data)

data$Critic_Score <- as.numeric(data$Critic_Score)
data$User_Score <- as.numeric(data$User_Score)

mean(data$Critic_Score, na.rm = TRUE)


# Convertion to the appropriate type
data$Critic_Score <- as.numeric(data$Critic_Score)
data <- data %>% mutate(User_Score = as.numeric(as.character(User_Score)) * 10)
data <- data %>% mutate(Year_of_Release = as.numeric(as.character(Year_of_Release), na.rm = TRUE))



# Make NA a  factor
dim(data)

data <- data %>% mutate(User_Score = round( User_Score / 10))
data <- data %>% mutate(Critic_Score = round( Critic_Score / 10))




data <- data %>% mutate(!is.na(Critic_Score))
data <- data %>% filter(!is.na(User_Score))
data <- data %>% mutate(User_Score = replace_na(User_Score, 5))
data <- data %>% mutate(Critic_Score = replace_na(Critic_Score, 5))
data <- data %>% group_by(User_Score) %>% filter(n() > 100) %>% ungroup(User_Score)

dim(data)
data <- data %>% filter(!is.na(Critic_Score))
data <- data %>% filter(!is.na(User_Score))
data <- data %>% filter(!is.na(Year_of_Release))
data <- data %>% filter(!is.na(Genre))
data$Genre <- as.factor(data$Genre)

data <- data %>% group_by(Critic_Score) %>% filter(n() > 50) %>% ungroup(Critic_Score)
data <- data %>% group_by(User_Score) %>% filter(n() > 50) %>% ungroup(User_Score)
data <- data %>% group_by(Year_of_Release) %>% filter(n() > 50) %>% ungroup(Year_of_Release)
data <- data %>% group_by(Genre) %>% filter(n() > 50) %>% ungroup(Genre)
data <- data %>% group_by(Publisher) %>% filter(n() > 50) %>% ungroup(Publisher)


data %>% summarise(user_scor_avg = mean(User_Score), critic_score_avg = mean(Critic_Score))
data$User_Score <- as.factor(data$User_Score)
data$Critic_Score <- as.factor(data$Critic_Score)
data$Year_of_Release <- as.factor(data$Platform)

test_index <- createDataPartition(data$Critic_Score, times = 1, p = 0.2, list = FALSE)
test_set <-  data %>% slice(test_index) %>% select(Critic_Score, User_Score, Year_of_Release, Genre)
train_set <- data  %>% slice(-test_index) %>% select(Critic_Score, User_Score, Year_of_Release, Genre)

##############################################
# RMSE with Critic_Score predictor
#############################################
#We'll try to predict using a Random Forest Model
train_rf <- train(User_Score ~ Critic_Score  , method = "rf", data = train_set)

y_hat = predict(train_rf, newdata = test_set)
test_set <- test_set %>%
  mutate(m_prediction = predict(train_rf, newdata = test_set)) 

confusionMatrix(predict(train_rf, test_set),
                test_set$User_Score)$overall["Accuracy"]

RMSE <- function(m_actual_rating, m_predicted_rating){
  sqrt(mean((as.numeric(as.character(m_actual_rating)) - as.numeric(as.character(m_predicted_rating)))^2))
}

##############################################
# RMSE with Critic_Score - Genre predictor
#############################################
RMSE(test_set$User_Score, test_set$m_prediction)


train_rf <- train(User_Score ~ Critic_Score + Genre , method = "rf", data = train_set)

y_hat = predict(train_rf, newdata = test_set)
test_set <- test_set %>%
  mutate(m_prediction = predict(train_rf, newdata = test_set)) 

confusionMatrix(predict(train_rf, test_set),
                test_set$User_Score)$overall["Accuracy"]

RMSE <- function(m_actual_rating, m_predicted_rating){
  sqrt(mean((as.numeric(as.character(m_actual_rating)) - as.numeric(as.character(m_predicted_rating)))^2))
}

# RMSE
RMSE(test_set$User_Score, test_set$m_prediction)




