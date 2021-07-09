# Warning: depending on the dplyr version a friendly warning may appear when 
# 'summarise' is called. It is just ok.

## 1.2 Introduction

# load libraries and the data

library(tidyverse)
library(lubridate)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# removing objects and garbage collection
rm(dl, ratings, movies, test_index, temp, movielens, removed)
gc()

#A sample of 10 observations is shown here.
edx[c(sample(edx$userId, 10)),] %>% as_tibble()

## 2.1 Initial data exploration

# check movies and user numbers

edx %>% pull(movieId) %>% unique() %>% length()

edx %>% pull(userId) %>% unique() %>% length()

# df for movies and reduce edx to 4 just variables
movies <- edx %>% select(movieId, title, genres) %>% unique()

edx <- edx %>% select(-title, -genres)

#r check title Vs movieId and genres
movies %>%
  unique() %>%
  group_by(title) %>%
  mutate(id_amount = n()) %>%
  filter(id_amount > 1)

# are the 2 "war of the worlds" different movies?}
edx %>%
  filter(movieId %in% c(34048,64997)) %>%
  group_by(userId) %>%
  mutate(count = n()) %>%
  filter(count >1)

# plot ratings
edx %>% ggplot(aes(rating)) + geom_bar()

# count ranking by movie
edx %>%
  group_by(movieId) %>%
  summarise(count = n()) %>%
  slice(1:20) %>%
  ggplot(aes(x=as.factor(movieId), y = count)) +
  geom_bar(stat = "identity")

# count ranking by user
edx %>%
  group_by(userId) %>%
  summarise(count = n()) %>%
  slice(1:20) %>%
  ggplot(aes(x=as.factor(userId), y = count)) +
  geom_bar(stat = "identity")

# global mean
mu <- edx %>% pull(rating) %>% mean()
mu

# rating by movie
edx %>%
  group_by(movieId) %>%
  summarise(avg_rating = mean(rating),
            count = n()) %>%
  filter(count > 2000) %>%
  slice(1:20) %>%
  ggplot(aes(x=as.factor(movieId), y = avg_rating - mu)) +
  geom_bar(stat = "identity")

# rating by user
edx %>%
  group_by(userId) %>%
  summarise(avg_rating = mean(rating),
            count = n()) %>%
  filter(count > 50) %>%
  slice(1:20) %>%
  ggplot(aes(x=as.factor(userId), y = avg_rating - mu)) +
  geom_bar(stat = "identity")

# plot rating by year
movies %>%
  mutate(movie_year = str_remove_all(str_extract(title, pattern = "\\(\\d{4}\\)$"), pattern = "\\(|\\)")) %>%
  select(movieId, movie_year) %>%
  right_join(edx, by = "movieId") %>%
  group_by(movie_year) %>%
  summarise(avg_rating = mean(rating)) %>%
  mutate(movie_year = as.integer(movie_year)) %>%
  ggplot(aes(x = movie_year, y = avg_rating)) + geom_line()

# plot amount by year
movies %>%
  mutate(movie_year = str_remove_all(str_extract(title, pattern = "\\(\\d{4}\\)$"), pattern = "\\(|\\)")) %>%
  group_by(movie_year) %>%
  select(movieId, movie_year) %>%
  right_join(edx, by = "movieId") %>%
  summarise(number_of_ratings = n()) %>%
  mutate(movie_year = as.integer(movie_year)) %>%
  ggplot(aes(x = movie_year, y = number_of_ratings)) + geom_line()

# several ratings assigned to a single date
edx %>%
  as_tibble() %>%
  mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
  group_by(userId, date = date(timestamp)) %>%
  summarise(count = n()) %>%
  filter(count > 10)

# how much observations were lively rated
edx %>%
  as_tibble() %>%
  mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
  group_by(userId, date = date(timestamp)) %>%
  summarise(count = n()) %>%
  mutate(lively_rated = if_else(count < 4, TRUE, FALSE)) %>%
  group_by(lively_rated) %>%
  summarise(total = sum(count))

# timestamp removed
edx <- edx %>% select(-timestamp)


## 2.3. Spliting training and test set
# split training and test
# Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in training set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into training set
removed <- anti_join(temp, test)
train <- rbind(train, removed) %>% arrange(userId, movieId)

rm(test_index, temp, removed, edx)
gc()

## 2.4. RMSE function
# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## 2.5.1. Naive model with just the global mean as predictor
# just the mean
mu <- mean(train$rating) #gets the mean for the training set

predicted_ratings <- test %>% 
  mutate(pred = mu) %>% #for each observation on the test set makes the prediction
  pull(pred)

RMSE(test$rating, predicted_ratings)

## 2.5.2. Movie effect
# movie effect
l_i <- 5 #term used to penalize movies with few observations

movie_eff <- function(train, test, l_i){
  mu <- mean(train$rating) #gets the mean for the training set
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l_i)) #get the difference relative to global mean for each movie
  predicted_ratings <- test %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)

}

predicted_ratings <- movie_eff(train, test, l_i)

RMSE(test$rating, predicted_ratings)

# l_i tuning
l_i <- seq(from=0, to = 10, by = 1) #numbers from 1 to 20 will be tried

rmses <- sapply(l_i, function(X){
  predicted_ratings <- movie_eff(train, test, X)

  RMSE(test$rating, predicted_ratings)
})
plot(rmses, x = l_i)

## 2.5.3. User effect
# user effect
l_u <- 5 #term used to penalize users with few observations

user_eff <- function(train, test, l_u){
  b_u <- train %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu)/(n()+l_u)) #get the difference relative to global mean for each user
  
  predicted_ratings <- test %>% 
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_u) %>%
    pull(pred)
}

predicted_ratings <- user_eff(train, test, l_u)

RMSE(test$rating, predicted_ratings)

# l_u tuning
l_u <- seq(from=0, to = 20, by = 1) #numbers from 1 to 20 will be tried

rmses <- sapply(l_u, function(X){
  predicted_ratings <- user_eff(train, test, X)
  
  RMSE(test$rating, predicted_ratings)
})
plot(rmses, x = l_u)

## 2.5.4. Both movie and user effects
# movie and user combined
l_i <- 2 #term used to penalize movies with few observations
l_u <- 5 #term used to penalize movies with few observations

lambdas <- expand.grid(l_i, l_u)
names(lambdas) <- c("li", "lu")

m_and_u_eff <- function(train, test, lambdas, i = 1){
  l_i <- lambdas[i,]$li
  l_u <- lambdas[i,]$lu
  
  mu <- mean(train$rating)
  
  b_i <- train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l_i)) #get the difference relative to global mean for each movie

  b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l_u)) #get the difference relative to global mean and movie effect for each user

predicted_ratings <- test %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

}

predicted_ratings <- m_and_u_eff(train, test, lambdas, i = 1)

RMSE(test$rating, predicted_ratings)

# setting rating limits
print(paste0("maximum: ", max(predicted_ratings)))
print(paste0("minimum: ", min(predicted_ratings)))

# adding a correction for limits
m_and_u_eff <- function(train, test, lambdas, i = 1){
  l_i <- lambdas[i,]$li
  l_u <- lambdas[i,]$lu
  
  mu <- mean(train$rating)
  
  b_i <- train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l_i)) #get the difference relative to global mean for each movie

  b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l_u)) #get the difference relative to global mean and movie effect for each user

predicted_ratings <- test %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  mutate(pred = if_else(pred > 5, 5, pred)) %>% #if pred is higher than 5 is replaced by 5
  mutate(pred = if_else(pred < 0.5, 0.5, pred)) %>% #if pred is lower than 0.5 is replaced by 0.5
  pull(pred)

RMSE(test$rating, predicted_ratings) #RMSE calculation is the final step because predicted values are no longer used

}

# tuning l_i and l_u
l_i <- seq(3, 7, 1) #term used to penalize movies with few observations
l_u <- seq(3, 7, 1) #term used to penalize movies with few observations

lambdas <- expand.grid(l_i, l_u)
names(lambdas) <- c("li", "lu")

rmses <- sapply(1:length(lambdas$li), m_and_u_eff, train = train, test = test, lambdas = lambdas)

# plot rmses
plot(rmses)

# pick best l_i and l_u
lambdas %>% mutate(rmse = rmses) %>%
  arrange(rmse) %>%
  slice(1)

# movie and user combined, user first

u_first_and_m_eff <- function(train, test, lambdas, i = 1){
  l_i <- lambdas[i,]$li
  l_u <- lambdas[i,]$lu
  
  mu <- mean(train$rating)

  b_u <- train %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu)/(n()+l_u)) #get the difference relative to global mean for each movie

  b_i <- train %>% 
    left_join(b_u, by="userId") %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - b_u - mu)/(n()+l_i)) #get the difference relative to global mean and movie effect for each user

  predicted_ratings <- test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    mutate(pred = if_else(pred > 5, 5, pred)) %>% #if pred is higher than 5 is replaced by 5
    mutate(pred = if_else(pred < 0.5, 0.5, pred)) %>% #if pred is lower than 0.5 is replaced by 0.5
    pull(pred)
  
  RMSE(test$rating, predicted_ratings)
}

l_i <- seq(1, 4, 1) #term used to penalize movies with few observations
l_u <- seq(1, 30, 3) #term used to penalize movies with few observations

lambdas <- expand.grid(l_i, l_u)
names(lambdas) <- c("li", "lu")

rmses <- sapply(1:length(lambdas$li), u_first_and_m_eff, train = train, test = test, lambdas = lambdas)

lambdas %>% mutate(rmse = rmses) %>%
  arrange(rmse) %>%
  slice(1)

plot(rmses)

## 2.5.5. Genre effect for users as a whole
# check different genres
movies %>%
  select(title,genres) %>%
  unique() %>%
  mutate(count = 1 + str_count(genres, pattern = "\\|")) %>%
  arrange(-count) %>%
  slice(1)

# create df gen_by_movie}
# as separate into 8 columns is being used, all movies with less than 8 genres associated to it 
# will generate a warning "Expected 8 pieces. Missing pieces filled with NA". It is just ok.
gen_by_movie <- movies %>%
  select(movieId, genres) %>%
  unique() %>%
  separate(genres, into = c("gen1", "gen2", "gen3", "gen4", "gen5", "gen6", "gen7", "gen8"), sep = "\\|") %>%
  pivot_longer(cols = -movieId, values_to = "single_genre") %>%
  select(-name) %>%
  filter(!is.na(single_genre)) %>%
  group_by(movieId) %>%
  mutate(genres_number = n()) %>%
  ungroup()

# how many different genres
gen_by_movie %>% pull(single_genre) %>% unique()

# no genres listed
movies %>%
  filter(str_detect(genres, "no genres listed")) %>%
  select(title, movieId, genres) %>%
  unique()

# mu + b_i + b_u + b_g
genre_eff <- function(train, test, lambdas, i = 1){

  l_i <- lambdas[i,]$l_i
  l_u <- lambdas[i,]$l_u
  l_g <- lambdas[i,]$l_g
  
  mu <- mean(train$rating)   
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l_i)) #get the difference relative to global mean for each movie
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l_u)) #get the difference relative to global mean and movie effect for each user

  b_g <- train %>% 
    select(userId, movieId, rating) %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    mutate(b_g = rating - b_u - b_i - mu) %>% #get the difference relative to global mean, movie effect and user effect
    left_join(gen_by_movie, by="movieId") %>%
    mutate(b_g = b_g/genres_number) %>% #divide the bias by the number of genres the movie has
    group_by(single_genre) %>%
    summarize(b_g = sum(b_g)/(n()+l_g)) #get the mean for each genre
  
  #when summarizing b_g, NAs shall be removed. NA may occur because the movie in the test set that is going to be predicted may have a genre for which there is no data in the training set for the user
  predicted_ratings <- test %>%
    select(userId, movieId) %>%
    as_tibble() %>%
    left_join(gen_by_movie, by = "movieId") %>%
    left_join(b_g, by = "single_genre") %>%
    group_by(userId, movieId) %>%
    summarise(b_g = sum(b_g, na.rm = TRUE)) %>% #NA shall be removed
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    mutate(pred = if_else(pred > 5, 5, pred)) %>% 
    mutate(pred = if_else(pred < 0.5, 0.5, pred)) %>%
    pull(pred)

  RMSE(test$rating, predicted_ratings)
}

# Running the algorithm for each set of lambdas took about 30 seconds (in the computer this task was developed). 
# The hardest step is calculating b_g, the table that comprises the genres biases.
# In order to make it easy for the reader to run the code, the previous chunk only
# tries a couple of different lambdas' combinations but many more were tried when 
# developing the algorithm.

l_i <- seq(3,7,2)
l_u <- seq(3,7,2)
l_g <- seq(3,7,2) #term used to penalize genres with few observations

lambdas <- expand.grid(l_i, l_u, l_g)
names(lambdas) <- c("l_i", "l_u", "l_g")


rmses <- sapply(1:length(lambdas$l_i), genre_eff, train = train, test = test, lambdas = lambdas)

lambdas %>% mutate(rmse = rmses) %>%
  arrange(rmse) %>%
  slice(1)

# plot rmses for l_i, l_u and l_g}
lambdas %>%
  mutate(rmse = rmses) %>%
  ggplot(aes(x = l_i, y = rmse)) +
  geom_point() + 
  facet_grid(l_g ~ l_u)

# remove b_g
rm(l_g, genre_eff)

## 2.5.6. Genre effect for users individually

# mu + b_i + b_u + b_g_u
gen_by_user_eff <- function(train, test, lambdas, i, return_list = FALSE){

  l_i <- lambdas[i,]$l_i
  l_u <- lambdas[i,]$l_u
  l_g_u <- lambdas[i,]$l_g_u
  
  mu <- mean(train$rating)   
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l_i)) #get the difference relative to global mean for each movie
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l_u)) #get the difference relative to global mean and movie effect for each user

  b_g_u <- train %>% 
    select(userId, movieId, rating) %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    mutate(b_g_u = rating - b_u - b_i - mu) %>%
    left_join(gen_by_movie, by="movieId") %>%
    mutate(b_g_u = b_g_u/genres_number) %>%
    group_by(userId, single_genre) %>%
    summarize(b_g_u = sum(b_g_u)/(n()+l_g_u)) %>%
    ungroup()
  
  predicted_ratings <- test %>%
    select(userId, movieId) %>%
    left_join(gen_by_movie, by = "movieId") %>%
    left_join(b_g_u, by = c("userId", "single_genre")) %>%
    group_by(userId, movieId) %>%
    summarise(b_g_u = sum(b_g_u, na.rm = TRUE)) %>% #ver que hay que sacar los NA
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u + b_g_u) %>%
    mutate(pred = if_else(pred > 5, 5, pred)) %>% #las 2 lineas que siguen son para acotar
    mutate(pred = if_else(pred < 0.5, 0.5, pred)) %>%
    pull(pred)

  rmse <- RMSE(test$rating, predicted_ratings)
  
  # this "return list" is used to get the predictions both on the training and the test set. 
  # It was useful to analyze errors during development
  if (return_list == TRUE){
    pred_train <- train %>%
      select(userId, movieId) %>%
      left_join(gen_by_movie, by = "movieId") %>%
      left_join(b_g_u, by = c("userId", "single_genre")) %>%
      group_by(userId, movieId) %>%
      summarise(b_g_u = sum(b_g_u, na.rm = TRUE)) %>% #ver que hay que sacar los NA
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u + b_g_u) %>%
      mutate(pred = if_else(pred > 5, 5, pred)) %>% #las 2 lineas que siguen son para acotar
      mutate(pred = if_else(pred < 0.5, 0.5, pred)) %>%
      pull(pred)
    result_list <- list(rmse, predicted_ratings, pred_train)
    names(result_list) <- c("rmse", "pred_test", "pred_train")
    return(result_list)
  } else {
      return(rmse)
  }
  
}

l_i <- seq(from = 3, to = 7, by = 2)
l_u <- seq(from = 6, to = 14, by = 4)
l_g_u <- seq(from = 1, to = 5, by = 2)

lambdas <- expand.grid(l_i, l_u, l_g_u)
names(lambdas) <- c("l_i", "l_u", "l_g_u")

rmses <- sapply(1:length(lambdas$l_i), gen_by_user_eff, train = train, test = test, lambdas = lambdas, return_list = FALSE)

lambdas %>% mutate(rmse = rmses) %>%
  arrange(rmse) %>%
  slice(1)

# plot rmses for l_i, l_u and l_g_u
lambdas %>%
  mutate(rmse = rmses) %>%
  ggplot(aes(x = l_i, y = rmse)) +
  geom_point() + 
  facet_grid(l_g_u ~ l_u)

## 2.7. Final parameter tuning
# recontruct edx data set
edx <- train %>% bind_rows(test) %>% arrange(userId, movieId)
rm(train, test)
gc()

# final paratemer tuning

set.seed(1, sample.kind = "Rounding")
indexes <- sample(length(edx$rating))

cross_valid <- function(df, indexes, k, folds, lambdas){
  valid_index <- indexes[(1+length(indexes)/folds*(k-1)):(length(indexes)/folds*k)]
  
  train_set <- df[-valid_index,]
  temp <- df[valid_index,]
  
  # Make sure userId and movieId in validation set are also in training set
  valid_set <- temp %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId") %>%
    arrange(userId, movieId)
  
  # Add rows removed from validation set back into training set
  removed <- anti_join(temp, valid_set)
  train_set <- rbind(train_set, removed) %>%
    arrange(userId, movieId)
  
  rm(df, valid_index, temp, removed)
  gc()
  
  rmses <- sapply(1:length(lambdas$l_i), gen_by_user_eff, train = train_set, test = valid_set, lambdas = lambdas, return_list = FALSE)
  
  rmses <- lambdas %>% mutate(rmse = rmses)
  return(rmses)
}

l_i <- seq(from = 3, to = 7, by = 2)
l_u <- seq(from = 6, to = 14, by = 4)
l_g_u <- seq(from = 1, to = 5, by = 2)

lambdas <- expand.grid(l_i, l_u, l_g_u)
names(lambdas) <- c("l_i", "l_u", "l_g_u")

rmses <- lapply(1:5, cross_valid, df = edx, indexes = indexes, folds = 5, lambdas = lambdas)

# plot rmses with ultimate parameters
rmses %>%
  bind_rows() %>%
  group_by(l_i, l_u, l_g_u) %>%
  summarise(rmse = mean(rmse)) %>%
  ggplot(aes(x = l_i, y = rmse)) +
  geom_point() + 
  facet_grid(l_g_u ~ l_u)

# pick best
rmses %>%
  bind_rows() %>%
  group_by(l_i, l_u, l_g_u) %>%
  summarise(rmse = mean(rmse))%>%
  ungroup() %>%
  arrange(rmse) %>%
  slice(1)

## 2.8. Training the model with the entire edx data set
# train model with entire edx data set
l_i <- 5
l_u <- 10
l_g_u <- 3

mu <- mean(edx$rating)   

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l_i)) #get the difference relative to global mean for each movie

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l_u)) #get the difference relative to global mean and movie effect for each user

b_g_u <- edx %>% 
  select(userId, movieId, rating) %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  mutate(b_g_u = rating - b_u - b_i - mu) %>%
  left_join(gen_by_movie, by="movieId") %>%
  mutate(b_g_u = b_g_u/genres_number) %>%
  group_by(userId, single_genre) %>%
  summarize(b_g_u = sum(b_g_u)/(n()+l_g_u)) %>%
  ungroup()

## 3.1. RMSE calculation on the validation data set
# predictions on the validation set
predicted_ratings <- validation %>%
  select(userId, movieId) %>%
  left_join(gen_by_movie, by = "movieId") %>%
  left_join(b_g_u, by = c("userId", "single_genre")) %>%
  group_by(userId, movieId) %>%
  summarise(b_g_u = sum(b_g_u, na.rm = TRUE)) %>% #ver que hay que sacar los NA
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u + b_g_u) %>%
  mutate(pred = if_else(pred > 5, 5, pred)) %>% #las 2 lineas que siguen son para acotar
  mutate(pred = if_else(pred < 0.5, 0.5, pred)) %>%
  pull(pred)

# RMSE on the validation set
rmse <- RMSE(validation$rating, predicted_ratings)
rmse