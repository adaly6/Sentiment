# Install academictwitter package for accessing Twitter API
install.packages("academictwitteR")

# Load the academictwitteR package
library(academictwitteR)

# Install edit_r_enriron to add bearer token to the environment
install.packages("usethis")
usethis::edit_r_environ()

# *Note: bearer token stored in the environment
get_bearer()

# Return all tweets that have the word Fanatics in them in English from 1/20-10/23
tweets <-
  get_all_tweets(
    query = "fanatics",
    start_tweets = "2020-01-01T00:00:00Z",
    end_tweets = "2023-10-31T00:00:00Z",
    n = 1,
    data_path = "C:/Users/adaly/OneDrive/Desktop/Data Wrangling",
    bind_tweets = FALSE,
    lang = "en",
    get_bearer()
  )
