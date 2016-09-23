### Import dataset ###

# Read .csv file
movies <- read.csv("movie_metadata.csv", header = T, stringsAsFactors = F)

### Data cleaning ####

# Remove instances which have at least one NA variblae
movies <- movies[complete.cases(movies), ]

# Function to remove Â, leading and trailing whitespace from movies$movie_title
movie_title_processing <- function(str){
  str <- sub(pattern = "Â", replacement = "", str)
  str <- sub(pattern = "^\\s+|\\s+$", replacement ="", str)
}

# Apply previous function
movies$movie_title <- sapply(movies$movie_title, FUN = movie_title_processing)

# Find all possible genres of movies
genres <- c()
i <- 1

for (ins in movies$genres){
  #print(ins)
  g <- strsplit(ins, "[|]")
  #print(g)
  for (gnr in g[[1]]){
    #print(gnr)
    if (!(gnr %in% genres)){
      genres[i] <- gnr
      i =  i + 1
    }
  }
}

# Create a dataframe with all generations of movies: 1 mean the movie belongs to this category, 0 otherwise
options(warn = -1)
mat <- c()
for (ins in movies$genres){
  mat <- rbind(mat, as.integer(genres == strsplit(ins, "[|]")[[1]]))
}
colnames(mat) <- genres
genres_df <- data.frame(Title = movies$movie_title, mat)
options(warn = 0)


### Summary statistics ###

# Take a glimpse at the dataset
head(movies)
dim(movies)
str(movies)
summary(movies)
