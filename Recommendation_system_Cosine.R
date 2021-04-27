#-------------------------------------------------------------------------------
# Import required packages and data.============================================

library(readr) # for reading the data set
library(dplyr) # for data manipulation
library(tidyr) # for converting data formats
library(ggplot2) # for data visualization
library(stringr) # for str_replace
library(reshape2) # for pivot option

#Updating the current work directory
setwd("C:/Users/appus/Downloads/MSBANA Course/Stats Computing/Stat Comp Projects")

#Reading the Data File
rating<-read_csv('rating.csv')
movies<-read_csv('movie.csv')

#-------------------------------------------------------------------------------
# Exploratory Data Analysis.====================================================
str(rating)
str(movies)

head(rating)
head(movies)

#Summary Statistics
summary(rating)
summary(movies)

#checking for null
sum(is.na(movies$title))
sum(is.na(movies$movieId))
sum(is.na(rating$userId))
sum(is.na(rating$rating))

#Total types of Genres and Total movies 
length(unique(movies$genres))
length(unique(movies$title))
length(unique(rating$userId))

#Separating the years from title column
movies$year<-regmatches(movies$title, gregexpr("\\((\\d{4})\\)", movies$title))
movies$title<-str_replace(movies$title, "\\((\\d{4})\\)","")
movies$year<-gsub("[()]", "", movies$year)
View(movies)

#How many movies reviewed by each user
x<-as.data.frame(rating$userId, col.names="userId")
colnames(x)<-c("userId")
x<-x %>% 
  group_by(userId) %>% 
  summarise(ratings_count=n()) 
head(x)

#User with highest rating count
x %>% 
  filter(ratings_count == max(x$ratings_count))  

# Count of different genres
df_tmp2<-movies %>% separate_rows(genres, sep ="\\|") 
df_tmp3<-df_tmp2 %>% group_by(genres) %>% summarise(count_id=n()) %>%
  filter(genres != '(no genres listed)') #removing unknown genres

#-------------------------------------------------------------------------------
# Graphical Analysis.===========================================================
df_tmp<-movies %>% group_by(year) %>% summarise(count_id=n())
df_tmp<-head(df_tmp,-1) #removing unknown years

#Explore the number of movies released each Year
ggplot(data=df_tmp, aes(as.Date(ISOdate(year, 1, 1)), count_id, group=0)) +
  geom_line(color="sky Blue", size=1, alpha=0.7, linetype=1)+
  geom_point()+ 
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Count of Movies per Year")+ 
  xlab("Year") + ylab("Number of movies released")

#Explore the distribution of movies by Genres
ggplot(data = df_tmp3,aes(reorder(genres, count_id), count_id, fill= count_id)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller() + xlab("Genre") +ylab("Frequency") +
  ggtitle("Distribution of Movies by Genres")

#Explore the frequency of User Ratings
rating %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram() +
  scale_x_log10() +
  ggtitle("Number of Users Ratings")

#-------------------------------------------------------------------------------
# Data Preparation.=============================================================

#Updating ratings data set to incorporate only expert reviews
quantile(x$ratings_count)
z<-x[x$ratings_count>quantile(x$ratings_count, probs = 0.75),] #Extracting expert user Id
rating<-rating[rating$userId %in% z$userId,]#Updating

#Merge the movies and ratings data set based on movie ID 
movie_details<-merge(x=movies,y=rating,by='movieId')
head(movie_details)

#Dropping extra columns
movie_details<-movie_details %>%
  select(-c('timestamp', 'year'))
head(movie_details)

#Finding out how many times a movie is rated
number_rating<-movie_details %>% 
  group_by(title) %>% 
  summarise(number_of_rating = n())
head(number_rating)

#Filtering out those movies which have been rated less than 75% quantile
quantile(number_rating$number_of_rating)
y<-quantile(number_rating$number_of_rating,probs = 0.75)
number_rating<-number_rating[number_rating$number_of_rating>=y,]

#Incorporating above information in our data
df<-merge(x=movie_details,y=number_rating, by='title')
head(df)

#Making sure we don't have any duplicates
df<-df %>% distinct(title, userId, .keep_all = TRUE)

#Dropping extra columns
df<-df %>% select(-c('number_of_rating', 'movieId'))
head(df)
length(unique(df$title))

#Increasing memory to counter error " cannot allocate vector of size n Mb" 
memory.size()
memory.limit()
memory.limit(size=56000) #Max

#Pivoting the data set
df_wide <- df %>%
  ungroup() %>%
  distinct(userId, title, rating) %>%
  spread(title, rating, fill = 0)

#Removing the userId column
movie_matrix <- as.matrix(df_wide[,-1])

#-------------------------------------------------------------------------------
# Movies Recommendation: Using Cosine Similarity.===============================

#Defining the cosine similarity function
cos_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))  


#Defining the Recommender system
cos_sim_fun <- function(movie_title, rating_mat = movie_matrix,
                         movies = df,
                         return_n = 10)
{
  # finding our movie
  # calculate cosine similarity for each movie based on rating
  # apply iterates over the columns of a matrix
  movie_col_index <- which(colnames(rating_mat) == movie_title)
  cos_sims <- apply(rating_mat, 2,
                    FUN = function(y) 
                      cos_sim(rating_mat[,movie_col_index], y))
  # return results
  tibble(title = names(cos_sims), cos_sim = cos_sims) %>%
    filter(title != movie_title) %>% # remove self reference
    inner_join(movies) %>%
    distinct(title,genres,cos_sim) %>%
    arrange(desc(cos_sim)) %>%
    top_n(return_n, cos_sim) %>%
    select(title,genres,cos_sim) 
}
#-------------------------------------------------------------------------------
# Enter Movie Name below.=======================================================
cos_sim_fun("Shawshank Redemption, The ")
#-------------------------------------------------------------------------------
