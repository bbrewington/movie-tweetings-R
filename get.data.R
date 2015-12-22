## get.data.R: Gets data from github repo sidooms/MovieTweetings
## and prepares it for analysis in R (as 3 data frames: users, movies, and ratings)

get.users <- function(){
     # userid::twitter_id
     users.con <- url("https://github.com/sidooms/MovieTweetings/raw/master/latest/users.dat")
     users <- readLines(users.con)
     close(users.con)
     users.parse <- strsplit(users, "::")
     data.frame(
          user_id = sapply(users.parse, function(x) x[1]),
          twitter_id = sapply(users.parse, function(x) x[2]),
          stringsAsFactors = FALSE)
}
get.movies <- function(){
     # movie_id::movie_title (movie_year)::genre|genre|genre
     movies.con <- url("https://github.com/sidooms/MovieTweetings/raw/master/latest/movies.dat")
     movies <- readLines(movies.con)
     close(movies.con)
     movies.parse <- strsplit(movies, "::")
     movies <- data.frame(
          movie_id = sapply(movies.parse, function(x) x[1]),
          movie_title_year = sapply(movies.parse, function(x) x[2]),
          genres = sapply(movies.parse, function(x) x[3]),
          stringsAsFactors = FALSE)
     df <- movies %>% select(genres) %>% 
          distinct() %>% mutate(genres.split = strsplit(genres, "\\|"))
     
     genres.vec <- character()
     
     for(i in 1:length(df[,1])){
          genres.vec <- c(genres.vec, df$genres.split[[i]])
          genres.vec <- sort(unique(genres.vec))
     } 
     
     df[,genres.vec] <- NA
     
     # Function to go through each element of df$genres, and check whether
     # each value of genres.vec is in that row.  Return vector of same length
     # as genres.vec
     
     set.convolve <- function(test.vec,reference.vec){ #y is reference vector, x is test vector 
          as.numeric(sapply(reference.vec, function(x) x %in% test.vec))
     }
     
     df[, 3:(2+length(genres.vec))] <- 
          t(sapply(df$genres.split, function(x) set.convolve(x, genres.vec)))
     df
}
get.ratings <- function(){
     # user_id::movie_id::rating::rating_timestamp
     ratings.con <- url("https://github.com/sidooms/MovieTweetings/raw/master/latest/ratings.dat")
     ratings <- readLines(ratings.con)
     close(ratings.con)
     ratings.parse <- strsplit(ratings, "::")
     data.frame(
          user_id = sapply(ratings.parse, function(x) x[1]),
          movie_id = sapply(ratings.parse, function(x) x[2]),
          rating = sapply(ratings.parse, function(x) x[3]),
          rating_timestamp = sapply(ratings.parse, function(x) x[4]))
}

users <- get.users()
movies <- get.movies()
ratings <- get.ratings()
