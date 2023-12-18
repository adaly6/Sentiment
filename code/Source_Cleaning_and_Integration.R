# Clear the environment
rm(list=ls())

# Install and load necessary packages for text mining techniques
install.packages(c("readxl", "stringr", "jsonlite", "purrr", "dplyer"))
library(stringr) 
library(readxl)
library(jsonlite)
library(purrr)
library(dplyr)

# Set the working directory to the main folder - directory will change for twitter data
setwd("C:/Users/X") # Fill with your working directory

## Trust Pilot

# Load trust pilot csv 
trust_pilot <- read.csv("trustpilot_reviews.csv", encoding = "UFT-8")

# Rename columns appropriately 
names(trust_pilot)[names(trust_pilot) == "usernames"]<-"Username"
names(trust_pilot)[names(trust_pilot) == "contents"]<-"Content"
names(trust_pilot)[names(trust_pilot) == "times"]<-"Date"
names(trust_pilot)[names(trust_pilot) == "ratings"]<-"Rating"
names(trust_pilot)[names(trust_pilot) == "source"]<-"Source"

# Clean the date feature to just year month day
trust_pilot$Date <- sub("T.*$", "", trust_pilot$Date)

# Convert date into a date data type
trust_pilot$Date <- as.Date(trust_pilot$Date) 

## Site Jabber

# Load site jabber csv
site_jabber <- read_excel("sitejabber_reviews.xlsx")

# Strip the white space from usernames again 
site_jabber$usernames <- trimws(site_jabber$usernames)
site_jabber$contents <- trimws(site_jabber$contents)
site_jabber$times <- trimws(site_jabber$times)

# Populate the data frame with the source
site_jabber$source <- rep("Site Jabber", nrow(site_jabber))

# Rename columns appropriately
names(site_jabber)[names(site_jabber) == "usernames"]<-"Username"
names(site_jabber)[names(site_jabber) == "contents"]<-"Content"
names(site_jabber)[names(site_jabber) == "times"]<-"Date"
names(site_jabber)[names(site_jabber) == "source"]<-"Source"

# Remove the "th" from date
site_jabber$Date <- gsub("th", "", site_jabber$Date)

# Convert date into a date data type
site_jabber$Date <- as.Date(site_jabber$Date, format = "%B %d, %Y")

# Populate ratings column as NA
site_jabber$Rating <- rep(NA, nrow(site_jabber))


# Vertically integrate site jabber and trust pilot reviews
website_reviews <- rbind(trust_pilot, site_jabber)

# Add a 6 column for engagement for twitter API integration purposes
website_reviews$Engagement <- rep(NA, nrow(website_reviews))

## Twitter

# Set the working directory to the folder containing the CSV files
setwd("C:/Users/X")

# Create an empty list to store data frames
twitter_data <- list()

# Loop through each CSV file and read it into a data frame
for (i in 1:21) {
  filename <- paste0("twitter", i, ".csv")
  filepath <- file.path(getwd(), filename)
  
  # Read CSV into a data frame
  df <- read.csv(filepath)
  
  # Store the data frame in the list
  twitter_data[[i]] <- df
}

# Extract and rename specific columns from each data frame in the list
extracted_twitter_data <- lapply(twitter_data, function(df) {
  df %>%
    select(matches("instructions__entries__content__itemContent__tweet_results__result__legacy__full_text"),
           matches("instructions__entries__content__itemContent__tweet_results__result__legacy__created_at"),
           matches("instructions__entries__content__itemContent__tweet_results__result__core__user_results__result__legacy__screen_name"),
           matches("instructions__entries__content__itemContent__tweet_results__result__legacy__retweet_count"),
           matches("instructions__entries__content__itemContent__tweet_results__result__legacy__favorite_count"))
})

# Combine all data frames into one
twitter <- do.call(rbind, extracted_twitter_data)

# Rename columns
colnames(twitter) <- c("Content", "Date", "Username", "Retweets", "Likes")

# Remove rows with null values in Content column
twitter <- twitter %>% filter(!is.na(Content) & Content != "")

# Add a column for engagement (sum of Likes and Retweets)
twitter$Engagement <- twitter$Likes + twitter$Retweets

# Convert Twitter date to consistent format
twitter$Date <- as.POSIXct(twitter$Date, format = "%a %b %d %H:%M:%S %z %Y")
twitter$Date <- format(twitter$Date, "%B %d, %Y")

# Convert date into a date data type
twitter$Date <- as.Date(twitter$Date, format = "%B %d, %Y")

# Remove Retweets and Likes columns
twitter <- twitter[, !names(twitter) %in% c("Retweets", "Likes")]

# Populate ratings column as NA and source as Twitter
twitter$Source <- rep("Twitter", nrow(twitter))
twitter$Rating <- rep(NA, nrow(twitter))

# Eliminate advsertisements from twitter data frame
unwanted_ads_keywords <- c("#ad", "#advertisement", "#Sponsored", "Check this out!", "Apple", "FOMO", "LAST-CHANCE", "PlayTrade",
                           "Adidas Joggers", "BCHI", "NOW AVAILABLE", "ATTENTION", "BLACK FRIDAY SUPER SALE", "Challenge yourself",
                           "Cyber Savings", "Eczema", "EXCLUSIVE!", "DNA", "eBay", "Turbo Access", "Google gift", "Hey Cowboys", 
                           "America's Best Pillows", "eBook", "JBL Headphones", "Kohls", "Advance Trading", "Lock in", "Nitto Ridge",
                           "Advent devotionals", "MASSIVE Sale", "Massive cushioning", "Queen Sheets", "Respiratory viruses", "Daily Bite",
                           "Sakura Temple", "turf insects", "Travel safely", "We want fans to celebrate")

# Filter out rows containing unwanted ads
twitter <- twitter[!grepl(paste(unwanted_ads_keywords, collapse = "|"), twitter$Content, ignore.case = TRUE), ]

# Vertically integrate website reviews and tweets
all_source <- rbind(website_reviews, twitter)


# Set wd back to main folder to save all_source
setwd("C:/Users/X")

# Save final cleaned and integrated data frame
write.csv(all_source, "all_source.csv", row.names = FALSE)

