### Import dataset ###

# Read .csv file
movies <- read.csv("movie_metadata.csv", header = T, stringsAsFactors = F)

### Data cleaning ####

# Remove instances which have at least one NA variable
movies <- movies[complete.cases(movies), ]
# Remove instances which are duplicated (duplicated based on title)
movies <- movies[!duplicated(movies$movie_title),]

# Function to remove Â, leading and trailing whitespace from movies$movie_title
movie_title_processing <- function(str){
  str <- sub(pattern = "Â", replacement = "", str)
  str <- sub(pattern = "^\\s+|\\s+$", replacement ="", str)
}

# Apply previous function
movies$movie_title <- sapply(movies$movie_title, FUN = movie_title_processing)

# Write clean dataset in a csv file
write.csv(movies, "movie_clean.csv")

### Add columns ###

# profit = revenue - budget
movies$profit <- movies$gross - movies$budget

# roi = revenue / budget
movies$roi <- movies$gross / movies$budget

# Profitable movies: profit > 0
movies$profitable <- F
movies$profitable[movies$profit > 0] <- T

### Remove columns ###

# We are not going to focus on these variables
movies[, c("movie_imdb_link", "color", "content_rating", "aspect_ratio", "facenumber_in_poster")] <- NULL

### Reorder columns ###

movies <- movies[c("movie_title", "imdb_score", "genres", "plot_keywords", "duration", "title_year", 
                   "country", "language", "num_critic_for_reviews", "num_user_for_reviews", "num_voted_users",
                   "movie_facebook_likes", "cast_total_facebook_likes",
                   "director_name", "director_facebook_likes", "actor_1_name", "actor_1_facebook_likes",
                   "actor_2_name", "actor_2_facebook_likes", "actor_3_name", "actor_3_facebook_likes",
                   "budget", "gross", "profit", "roi", "profitable")]

#### Edit columns ###

# Genres of movies and the number of movies that belong to each genre
# note: Each movies can belong to more than one genres
genres <- c()
gsum <- c()
i <- 1
for (ins in movies$genres){
  #print(ins)
  g <- strsplit(ins, "[|]")
  for (gnr in g[[1]]){
    # if the current genre doesn't exist in the vector
    if (!(gnr %in% genres)){
      genres[i] <- gnr
      gsum[i] <- 1
      i =  i + 1
    } else{
      gsum[which(genres == gnr)] = gsum[which(genres == gnr)] + 1
    }
  }
}
# A datafrma with genres and the number of movies belong to each genre
genres_sum <- data.frame(genres = factor(genres), sum = gsum)

# create a dataframe with logical values which indiactes in which
# category each movie belongs to
movies$genres <- strsplit(movies$genres, "[|]")
genres_idx <- movies[, c("movie_title", "genres")]
i = 1
mat <- matrix(rep(0, (dim(movies)[1] * length(genres))), nrow = dim(movies)[1])
for (g in genres_idx$genres){
  idx <- which(genres %in% g)
  mat[i, idx] <- 1
  i = i + 1
}
colnames(mat) <- genres

library(reshape2)
movies_and_genres <- cbind(movie_title = genres_idx$movie_title, data.frame(mat), stringsAsFactors = F)
movies_and_genres <- melt(movies_and_genres, id = "movie_title")
x <- merge(movies_and_genres, movies, by = c("movie_title", "movie_title"))
x$genres <- NULL

library(ggplot2)
ggplot(x, aes(x = profit, y = imdb_score, colour = variable)) + geom_point()


# Explore keywords in movies
# note: each movie can have more then one keywords
keywords <- c()
kwsum <- c()
i <- 1
for (ins in movies$plot_keywords){
  kw <- strsplit(ins, "[|]")
  for (kword in kw[[1]]){
    # if the current keyword doesn't exist in the vector
    if (!(kword %in% keywords)){
      keywords[i] <- kword
      kwsum[i] <- 1
      i = i + 1
    } else{
      kwsum[which(keywords == kword)] = kwsum[which(keywords == kword)] + 1
    }
  }
}
keywords_sum <- data.frame(keywords = keywords, sum = kwsum)
keywords_sum <- keywords_sum[order(keywords_sum$sum, decreasing = T),]



# Explore actors and their popularity
actors1 <- data.frame(name = movies$actor_1_name, fb_likes = movies$actor_1_facebook_likes)
actors2 <- data.frame(name = movies$actor_2_name, fb_likes = movies$actor_2_facebook_likes)
actors3 <- data.frame(name = movies$actor_3_name, fb_likes = movies$actor_3_facebook_likes)
actors <- rbind(actors1, actors2, actors3)
occurences <- table(unlist(actors$name))
occurences <- as.data.frame(occurences[order(occurences, decreasing = T)])
colnames(occurences) <- c("name", "movies_played")
actors <- actors[!duplicated(actors), ]
actors_full <- merge(occurences, actors, by = "name")

### Create beautiful plots ###
library(ggplot2)

# Number of movies belonging to each genre
ggplot(genres_sum, aes(x = reorder(genres, sum), y = sum, fill = reorder(genres, sum))) + 
  geom_bar(stat = "identity", colour = "black") + coord_flip() +
  labs(title = "Movie genres", x = "", y = "") + 
  geom_text(aes(label = sum), hjust = -0.2, vjust = 0.4) + 
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + 
  theme(legend.position = "None")


# Number of 30 most popular keywords  
ggplot(keywords_sum[1:30, ], aes(x = reorder(keywords, sum), y = sum, fill = reorder(keywords, sum))) + 
  geom_bar(stat = "identity", colour = "black") + coord_flip() +
  labs(title = "Keywords", x = "", y = "") + 
  geom_text(aes(label = sum), hjust = -0.2, vjust = 0.4) + 
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + 
  theme(legend.position = "None")

# Number of movies in each language
ggplot(movies, aes(factor(language))) + 
  geom_bar(colour = "black") + coord_flip() +
  labs(title = "Language", x = "", y = "") + 
  theme(axis.ticks.y = element_blank()) 
  
# IMDb score and Budget of movie
ggplot(movies, aes(x = budget, y = imdb_score)) + 
  geom_jitter(alpha = 0.25) + 
  scale_x_log10(breaks = c(1e+02, 1e+04, 1e+06, 1e+08, 1e+10), 
                labels = c("100", "10,000", "1 million", "100 millions", "1 billion")) +
  labs(x = "Movie budget", y = "IMDb Score")

# Facebook likes and Budget of movie
# We remove those movies who have zero facebook likes (maybe they don't even have a facebook page)
ggplot(movies[movies$movie_facebook_likes != 0, ], 
       aes(x = movie_facebook_likes, y = imdb_score)) + 
  geom_jitter(alpha = 0.25) + 
  scale_x_log10(breaks = c(1e+02, 1e+04, 1e+06, 1e+08, 1e+10), 
                labels = c("100", "10,000", "1 million", "100 millions", "1 billion")) +
  scale_y_log10(breaks = c(0, 1e+02, 1e+04, 1e+06), 
                labels = c("0", "100", "10,000", "100,000")) +
  labs(x = "Facebook Likes", y = "IMDB Score") 

# Gross revenue and Budget of movie
ggplot(movies, aes(x = budget, y = gross, colour = (profitable))) + geom_abline(intercept = 0, size = 0.5) +
  geom_jitter(alpha = 0.25) + 
  scale_x_log10(breaks = c(1e+02, 1e+04, 1e+06, 1e+08, 1e+10), 
                labels = c("100", "10,000", "1 million", "100 millions", "1 billion")) +
  scale_y_log10(breaks = c(1e+02, 1e+04, 1e+06, 1e+08, 1e+10), 
                labels = c("100", "10,000", "1 million", "100 millions", "1 billion")) +
  labs(x = "Movie budget", y = "Gross Revenue") + 
  theme(legend.position = "None") 



# Most popular actors
# We remove those actors who have zero facebook likes (maybe they don't even have a facebook page)
ggplot(actors_full[actors_full$fb_likes != 0, ], aes(x = movies_played, y = fb_likes)) + 
  geom_jitter(width = 0.75, alpha = 0.1) +
  labs(title = "Actors", x = "Movies played", y = "Facebook likes") +
  scale_y_log10(breaks = c(1e+02, 1e+04, 1e+06), labels = c("100", "10,000", "1 million"))
  
# Movies per year
ggplot(movies, aes(factor(title_year))) + 
  geom_bar(colour = "black") + coord_flip() +
  labs(title = "Language", x = "", y = "") + 
  theme( axis.ticks.y = element_blank()) 



### Summary statistics ###

# Take a glimpse at the dataset
head(movies)
dim(movies)
str(movies)
summary(movies)










