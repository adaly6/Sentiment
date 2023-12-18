# Clear the environment
rm(list=ls())

# Install xml2 library for web crawling
library(xml2)

# Set up emtpy vectors
usernames <- character(0) 
contents <- character(0) 
times <- character(0)
ratings <- character(0)

# Web crawl the page to get the data
for (i in 1:125){
  
  # Counter to know each time the bot moves to a new page
  print(i)
  
  # Set up a user agent and the page URL that will be scrapped
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36 Edg/117.0.2045.47"
  url <- paste("https://www.trustpilot.com/review/www.fanatics.com?page=", i, sep = "" )
  
  # Read the HTML on the website and use a user agent to follow best practices of web crawling
  page <- read_html(url, user_agent)
  
  # Sleep for 5 seconds to follow best practices of web crawling
  Sys.sleep(4)
  
  # Scrape the username and append it to the usernames vector
  username <- xml_text(xml_find_all(page, "//article/div/aside/div/a/span"))
  usernames <- c(usernames, username)
  
  # Scrap the content and append it to the contents vector
  content <- xml_text(xml_find_all(page, "//article/div/section/div/p[1]"))
  contents <- c(contents, content)
  
  # Scrape the time posted and append to the times vector - reviews may be updated in which they have a different Xpath
  time <- xml_attr(xml_find_all(page, "//section/div/div/time"), "datetime")
  time2 <- xml_attr(xml_find_all(page, "//section/div/div/span/time"), "datetime")
  times <- c(times, time, time2)

  # Scrape the time and append it to the ratings vector
  rating <- xml_attr(xml_find_all(page, "//article/div/section/div"), "data-service-review-rating")
  ratings <- c(ratings, rating)
  
  # Wait a random amount of time before moving to the next page
  random_value <- sample(4:12, 1)
  Sys.sleep(random_value)
}

# Ratings have NAs between them - HTML code set up that way - omitting them does not harm the data
ratings <- na.omit(ratings)

# Create date frame from web crawling
trustpilot_reviews <- data.frame(usernames, contents, times, ratings)

# Trim white space
trustpilot_reviews$usernames <- trimws(trustpilot_reviews$usernames)
trustpilot_reviews$contents <- trimws(trustpilot_reviews$contents)
trustpilot_reviews$times <- trimws(trustpilot_reviews$times)
trustpilot_reviews$ratings <- trimws(trustpilot_reviews$ratings)

# Populate the data frame with the source
trustpilot_reviews$source <- rep("Trust Pilot", nrow(trustpilot_reviews))

# Save the data frame to a csv
write.csv(trustpilot_reviews, "trustpilot_reviews.csv", row.names = FALSE)

