#---
#title: "MovieLens"
#author: "Alejandro Greco"
#date: "10/10/2019"
#---
  
# 1 - Introduction


#This is a MovieLens Project to make a movie recommendation system based on movies data set provided in the Edx course.

#The objective of this project is to obtain the lower possible Root Mean Squared Error (RMSE), lower than 0.8649, using the edx data provided validating with the validation set. The original data content about 10M registers but after EDX processing were created 2 data set, one named "edx" that is composed for about 9M observations and other named "validation" with about 1M registers both with 6 columns with not null variables.


# 2 - Methods

#In this project was evaluated 4 models to make the best recommendation possible with the lower value in loss function Root mean squared error (RMSE), the models are:
  
  
#   1 - Naive: A model using only the average of all ratings.

#   2 - Movies: A model using model 1 adding the movies because some movies are rated higher than others. 

#   3 - Users: A model using model 2 adding the users because some users are very cranky and others love every movie. This implies that rating depends on the movies and the users.

#   4 - Genres: A model using model 3 adding the genre of the movie because some genres like sci-fi are more popular than others like documentaries.

#Using Root mean squared error (RMSE) will would compare the efficiency of the models.


## 2.1 - Data


#This is the original data set transformation given edx set and validation set:
  


################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()

download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################ end of the data given


## 2.2 - Requirements 


#These packages are required for this project:
  
  
# Installation of packages 

if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")



## 2.3 - Data cleaning


#First, it is suggested to create a copy of the edx and validation data set to protect the integrity of the original data.



edx_clean        <-  edx
validation_clean <-  validation



#In the table below is the head of the data set.



head(edx_clean)



#As you could observe, there are two columns hard to read, timestamp and the genres. 

#The timestamp column is not in date-time format and genres are multiple combinations. We will start with the timestamp changing its format.



edx_clean$timestamp_format <- as.POSIXct(edx_clean$timestamp, origin="1970-01-01")
validation_clean$timestamp_format <- as.POSIXct(validation_clean$timestamp, origin="1970-01-01")


#On one hand, based on that new things are more attractive than the old ones, the year when the movie was made could affect the rate.



edx_clean$year_evaluated<-format(edx_clean$timestamp_format,"%Y")
validation_clean$year_evaluated<-format(validation_clean$timestamp_format,"%Y")



#On the other hand, genres is other column to transform because some of them are more popular than others and for this reason is necessary to split one genre per row in a new column.



edx_clean$genre <- edx_clean$genres
edx_clean <- edx_clean %>% separate_rows(genre,sep = "\\|")

validation_clean$genre <- validation_clean$genres
validation_clean <- validation_clean %>% separate_rows(genre,sep = "\\|")



## 2.4 - Data exploration and visualization


#As you can notice bellow, there are 10677 uniques movies, 69878 uniques users that rate from 1 to 5 since 1995-01-09 08:46:49 to 2009-01-05 02:02:16 -03 and there is no null value.
#The first and last year were particulate because in 1995 were 2 evaluations and in 2009 where less evaluation than 2008. There are 20 uniques genres and  797 combinations of one or more because each movie could have more than one genre.

#The size of the data is about 10 million rows, split into edx data with 9M and validation 1 million.



head(edx_clean, 10)
which(is.na(edx_clean))

movies_by_year<-edx_clean %>% 
  group_by(year_evaluated) %>% 
  summarise(number_uniques_movies=n_distinct(movieId)
  )
kable(movies_by_year)


# Movies, Users and Genres
# The unique genres are 20 and there are 797 combinations of one or more.

resumen_unique <- edx_clean %>% 
  summarise(
    Movies = n_distinct(movieId),
    Users = n_distinct(userId),
    Genres = n_distinct(genres),
    Genre = n_distinct(genre)
  )
resumen_unique

#### Min time and max time
date_min<-min(edx_clean$timestamp_format)
date_max<-max(edx_clean$timestamp_format)

date_min
date_max



#Because the size of the data set is too big to process it or to graphic it for some computers, I made a random sample of 666 rows to represent the data. For example, for 9 million rows, the size of a sample could be 16,610 with a confidence level of 99% and a confidence interval of 1%. This sample is just to explore graphically and will not be used for the models. I will use a 99% confidential level and a 5%  margin of error.


edx_sample<-edx_clean[sample(nrow(edx_clean),666),]


### 2.4.1 - Time


#Using the sample to make a scatter plot and box plot to graphic time and rating, we can observe something interesting. The half-point ratings started about 2003 when we are guessing was enable and the rating looks very stable between 3 and 4 over the years.


sp_rating_in_time_temp <-  qplot(timestamp_format, rating, colour = userId,   data = edx_sample) + 
                          geom_smooth() + 
                          labs(x="Years", y="Rate")  

sp_rating_in_time <- sp_rating_in_time_temp + 
                    theme_minimal() +
                    ggtitle("Rate in time") + 
                    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                          plot.title=element_text(hjust=0.5))

sp_rating_in_time

boxplot_year_ratting <- ggplot(edx_sample, 
                               aes(group=year_evaluated, 
                                   x=year_evaluated, 
                                   y=rating)) +
                        geom_boxplot(fill='#A4A4A4', color="darkred") + theme_minimal()+ 
                        labs(x="Years", y="Rate")   +
                        ggtitle("Rate in time")+ 
                        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                              plot.title=element_text(hjust=0.5))

boxplot_year_ratting

edx_clean$year_evaluated<-format(edx_clean$timestamp_format,"%Y")

date_min
date_max



#Now, using the full data, we can notice that started in 1995-01-09 to 2009-01-05, the years 1995 and 2009 are incomplete and more of that, in the 2003 open to half points evaluation. The graph shows that every year increase the number of movies rated (2009 is incomplete).




year_mean_rating <- edx_clean %>% 
                    group_by(year_evaluated) %>% 
                    summarise(number_movies=n(), 
                              avg_rating=mean(rating),
                              max_rating=max(rating),
                              min_rating=min(rating),
                              sd_rating=sd(rating)
                    )

year_mean_rating

attach(year_mean_rating)
year_mean_rating_sorted <- year_mean_rating[order(-number_movies),]

head(year_mean_rating_sorted)

movies_time_size <- qplot(year_evaluated,avg_rating , data = year_mean_rating, size = number_movies)+ 
                      xlab("Year")+ylab("Avg Rate")  + theme_minimal()+ 
                      ggtitle("Movies rated in time")+ 
                      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            plot.title=element_text(hjust=0.5))

movies_time_size

#detach(year_mean_rating)  #only if is necessary to run it more than one time because if is already attached it will generate an error.


### 2.4.2 - User

#Each person is a world, and they express themselves through the rating of the movies been some of them too good and others too mean.
#Additionally, some of them have rated more than 6 thousand movies and some of them just 10.


sp_users_rating_temp <- qplot(userId, 
                              rating,
                              colour = movieId, 
                              data = edx_sample) + 
                        geom_smooth() + 
                        labs(x="Users ID", y="Rate")  

sp_users_rating <- sp_users_rating_temp + 
                  theme_minimal() + 
                  ggtitle("Users rate") + 
                  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                        plot.title=element_text(hjust=0.5))

sp_users_rating

###It was used the edx original data to calculate the unique number of movies rated by users easer.

nomber_movie_users_rated <- edx %>% 
                            group_by(userId) %>% 
                            summarise(unique_number_movies=n_distinct(movieId),
                                      avg_rating=mean(rating),
                                      sd_rating=sd(rating)
                            )

nomber_movie_users_rated   

top <- head(arrange(nomber_movie_users_rated,desc(unique_number_movies)), n = 10)
bottom <- tail(arrange(nomber_movie_users_rated,desc(unique_number_movies)), n = 10)

kable(top)
kable(bottom)

#average standard deviation

sd_general_rating <- edx %>%  summarise( sd_general_srating=sd(rating) )
sd_general_rating

avg_sd_top <- top  %>%  summarise( avg_sd_rating_top=mean(sd_rating) )
avg_sd_top

avg_sd_bottom <- bottom  %>%  summarise( avg_sd_rating_bottom=mean(sd_rating) )
avg_sd_bottom


### 2.4.3 - Movies


#Each movie is a piece of art, some of them are very famous but some others not, for this reason, the rating of the movie was definitely affected by itself.
#Here we can clearly observe that it is related to the rating with the movie, and the evaluator (users).


sp_movie_rating <- qplot(movieId, 
                       rating, 
                       colour = userId,
                       data = edx_sample) + 
                  geom_smooth()+  theme_minimal() + 
                  xlab("Movies")+ylab("Avg Rate")  +
                  ggtitle("Rate of movies")+ 
                  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                        plot.title=element_text(hjust=0.5))

sp_movie_rating


### 2.4.4 - Genre


#The genre could affect the rate because some genres have more passionate followers that could rate higher or lower for example sci-fi.


genre_mean_rating <- edx_clean %>% 
                    group_by(genre) %>%
                    summarise(number_movies=n(), 
                              avg_rating=mean(rating),
                              max_rating=max(rating),
                              min_rating=min(rating),
                              sd_rating=sd(rating))

attach(genre_mean_rating)
genre_mean_rating_sorted<-genre_mean_rating[order(-number_movies),]

head(genre_mean_rating_sorted,10)

movie_genre_graph_temp <- ggplot(data=genre_mean_rating, 
                               aes(x=reorder(genre,-number_movies),
                                   y=number_movies)) + 
                          geom_bar(stat="identity") + 
                          xlab("Genre")+
                          ylab("Movies")  

movie_genre_graph <- movie_genre_graph_temp  + 
                      theme_minimal()+ 
                      ggtitle("Movies by genre") + 
                      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                            plot.title=element_text(hjust=0.5))

movie_genre_graph

#detach(genre_mean_rating)  #only if is necessary to run it more than one time because if is already attached it will generate an error.

genre_sum<-edx_clean %>%
                    group_by(genre) %>%
                    summarize(count = n()) %>%
                    arrange(desc(count))

genre_sum

kable(resumen_unique)


## 2.5 - Insights gained


#It is interesting how in this data every year more and more movies are rated and some users have 6,616 movies rated, one rate for each movie. This is the user with more rates but on average a movie is 100 minutes (1.66667 horas) for each movie, that represent 6,616,000 minutes or 11,026.67 horas, or 459.44 days, or 1.25875 years of continues watching. In a more realistic way, if we divide the total hours between 40 working hours per week we have 275.67 working weeks divided by 52 weeks per year, which means 5.30 working years without vacations. But this is just the number one, is interested how from this user to the 10Th drop the rates to half. This tops 10 users with more than 3,000 rates with a standard deviation on average of 0.79 vs the bottom of the user with less than 14 movies and more than 10 they have an average of SD of 1.042. for the whole data set is 1.06 that is much closer to the bottom than to the top.

#As you could observe, the number of uniques movies is about 10 thousand but each movie is rated more than one time and for more than one user, and on top of that, each movie could have more than one genre making this data a matrix of information.


## 2.6 - Modeling approach


#This study considered 4 different approaches:
  
  
#   1 - The simple average of the rating;

#   2 - The previus model adding movies effect;

#   3 - The previus model adding users effect;

#   4 - The previous model adding genres effect.


#All of them were evaluated using RMSE and the best option was the 4th as you could observe in the results.


RMSE <- function(true_ratings, predicted_ratings)
{ sqrt(mean((true_ratings - predicted_ratings)^2))}


### 2.6.1 - Method 1, rating average


#This method have the recommendation using just the rating.


mu_hat <- mean(edx_clean$rating)
mu_hat

rmse_1_mu <- RMSE(edx_clean$rating, mu_hat)
rmse_1_mu

methods_rmse_results <- data.frame(model="General rating average only", RMSE=rmse_1_mu)

kable(methods_rmse_results)



### 2.6.2 - Method 2, movies effect


#This method have the recommendation using the rating and considering the effect of each movie.


# first we need movie average
mu <- mean(edx_clean$rating)
mu

movie_mu <- edx_clean %>%
            group_by(movieId) %>% 
            summarize(b_m = mean(rating - mu))
movie_mu

predicted_ratings_2 <- mu + validation_clean %>% 
                      left_join(movie_mu, by='movieId') %>% 
                      pull(b_m)

model_2_rmse <- RMSE(predicted_ratings_2, validation_clean$rating)
model_2_rmse

methods_rmse_results <- methods_rmse_results %>% 
                        add_row(model="Movies effect", RMSE=model_2_rmse)

kable(methods_rmse_results)


### 2.6.3 - Method 3, users effect


#This method have the recommendation using the rating and considering the effect of each movie and users.


user_mu <- edx_clean %>% 
          left_join(movie_mu, by='movieId') %>% 
          group_by(userId) %>%
          summarize(b_u = mean(rating - mu - b_m))

predicted_ratings_3 <- validation_clean %>% 
                      left_join(movie_mu, by='movieId') %>% 
                      left_join(user_mu, by='userId') %>% 
                      mutate(pred = mu + b_m + b_u) %>% 
                      pull(pred)

model_3_rmse <- RMSE(predicted_ratings_3, validation_clean$rating)
model_3_rmse


methods_rmse_results <- methods_rmse_results %>% 
                        add_row(model="Users effect", RMSE=model_3_rmse)

kable(methods_rmse_results)


### 2.6.4 - Method 4, genres effect

#This method have the recommendation using the rating and considering the effect of each movie, users and genre.


genre_mu <- edx_clean %>% 
            left_join(movie_mu, by='movieId') %>%
            left_join(user_mu, by='userId') %>% 
            group_by(genre) %>%
            summarize(b_g = mean(rating - mu - b_m - b_u))
genre_mu

predicted_ratings_4 <- validation_clean %>% 
                      left_join(movie_mu, by='movieId') %>% 
                      left_join(user_mu, by='userId') %>% 
                      left_join(genre_mu, by='genre') %>% 
                      mutate(pred = mu + b_m + b_u + b_g) %>% 
                      pull(pred)

model_4_rmse <- RMSE(predicted_ratings_4, validation_clean$rating)
model_4_rmse

methods_rmse_results <- methods_rmse_results %>% 
                        add_row(model="Genres effect", RMSE=model_4_rmse)

kable(methods_rmse_results)


# 3 Results 


#In conclusion, the best model is the number 4 with excellent RMSE of 0.8632723 Adding more variables to the model increase the complexity giving better predictions but it is no infinity. Every time a new dimension is added to the model improves the RMSE but in this study after the 3rd and 4Th, the improvement is very low because it is harder to predict better even with more dimensions. A movie with an average 100 minutes composed of scenes, actors, stories, sounds, special effects and expression of feelings that combined in a specific order and in a specif way could influence the watcher perception. These variables are hard to measure objectively making hard to improve more the recommendation system for movies.


kable(methods_rmse_results)


# 4 Conclusion 


## 4.1 Summary of the report


#In this project we could evaluated more than 10 thousands of movie data set of about 13 years of evaluation of almost 70 thousand users to demonstrate that despite of the fact that each movie is an expression of art, we use the rating of all the users had made, considering that some movies are more popular than others, that each user is unique and the genre of the movie to make a recommendation with an RMSE of 0.86327 that is very good based on the difficulty of making a recommendation to a variety of people.


## 4.2 - Limitations 


#In general, the limitations of this project are the data that is limited in a 13-year movie rating sample of some users with a change of methodology in the rating adding half points in 2003 and the availability of hardware that is definitely a limitation taking a lot of time to make some calculations or graphics. 

#The universe of the data is not the only limitation, also the format of the data classification like the genre that could be better if a movie could be classified in just one genre and not in multiples. Like the music how could be classified as only pop? what is rock? the same happens with the movie because they are human expressions been considered as art and are not easy to classify in just one genre.

#Additionally, the only movie variables we have are the title, genre and year in the title limiting our possibilities of analysis. Some information about the movie could be affecting rate like the duration of the movie, the number of scenes, actors, stories, sounds, special effects and expression of feelings by each scene and order of them. 


## 4.3 - Future work


#This study riched great results but it still has great potential to improve. The above analysis is limited and we want to extend the discussion and make a few further recommendations. 

#From the movie perspective, the year when were rated it doesn't look like affected the user preference, but perhaps could be relevant related to the year when the movie appears in the catalog and when the movie was on theaters. Perhaps, with more details of the movies like actors, duration, synopsis, the number of scenes, stories, script, music and special effects could help not only to determinate a better recommendation but also could help to developed better movies.

#From the user perspective, there are several studies that suggest that the genre of the person, age, environment and culture determines the behavior and preferences of the person that could affect the rate. Based on this, could be interested to improve customer satisfaction to considered their genre, location, age and culture.  The location is affecting the rate? by countries? or by cities? Is it more probably a better rate when it is new rather than if it is old? The people that were born in the '80s prefer more sci-fi than comedy? Are some actors better than others? This question could be answered in future works.

