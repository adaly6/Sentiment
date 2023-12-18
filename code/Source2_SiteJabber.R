# Clear the environment
rm(list=ls())

# Install proper packages to execute the script
library(xml2)
install.packages("RSelenium")
library(RSelenium)
install.packages("wdman")
library(wdman)

# Create a remote driver instance using the remote URL
remDr <- remoteDriver(
  remoteServerAddr = "127.0.0.1",
  port = 4445L,
  browserName = "firefox",
  extraCapabilities = list("moz:firefoxOptions" = list(args = c("--headless" = FALSE)))
)

# Connect to the remote driver
remDr$open()

### Test

# Test that selenium Webdriver is open and working - navigate to a random page
remDr$navigate("http://www.google.com/ncr")

# Then navigate to a second page
remDr$navigate("https://github.com/ropensci/wdman")
remDr$getCurrentUrl()

# Go back and get Google URL
remDr$goBack()
remDr$getCurrentUrl()

###

# Set up review url to navigate to
url <- "https://www.sitejabber.com/reviews/fanatics.com"
remDr$navigate(url)
remDr$getCurrentUrl()

remDr$maxWindowSize()
remDr$screenshot(display = TRUE)

# Check if there are frames
webElems <- remDr$findElements(using = "tag name", "iframe")
if (length(webElems) > 0) {
  # If frames are found, print the src attribute of each frame
  frame_src <- sapply(webElems, function(x) x$getElementAttribute("src"))
  print(frame_src)
}

# Set up empty vectors
usernames <- character(0) 
contents <- character(0) 
times <- character(0)
ratings <- character(0)

# Web crawl the page to get the data
for (i in 1:3){
  
  # Sleep for 5 seconds to follow best practices of web crawling
  Sys.sleep(5)
  
  # Get page source to read xml
  page_source <- remDr$getPageSource()[[1]]
  
  # Read xml in site jabber
  page <- read_xml(page_source)
  
  # Scrape the username and append it to the usernames vector
  username <- xml_text(xml_find_all(page, "//div[@class='review__flex ']/div/div/div[@class='review__author__name']/a/span"))
  usernames <- c(usernames, username)
  
  # Scrap the content and append it to the contents vector
  content <- xml_text(xml_find_all(page, "//div[@class='review__flex ']/div/div[@class='review__content']/div/p"))
  contents <- c(contents, content)
  
  # Scrape the time and append it to the times vector
  time <- xml_text(xml_find_all(page, "//div[@class='review__flex ']/div/div[@class='review__content']/div/div[@class='review__date']"))
  times <- c(times, time)
  
  # Scrape the time and append it to the ratings vector
  rating <- xml_attr(xml_find_all(page, "//div[@class='review__flex ']/div/div[@class='review__content']/div/div/div"), "data-rating")
  ratings <- c(ratings, rating)
  
  # If frames are found, switch to the first frame (you may need to adjust this)
  #remDr$switchToFrame(webElems[[14]])
  
  # Wait a random amount of time before moving to the next page
  random_value <- sample(4:12, 1)
  Sys.sleep(random_value)
  
  # Find the next button within the HTML and simulate the trigger to load more data on the next page
  next_button <- remDr$findElement(using = "css selector", "#reviews > div.url-reviews__ajax > div.pagination > div.pagination__next > span > a")
  
  next_button$clickElement()
  
  
}

# Close the Selenium session
remDr$close()

# Create date frame from web crawling
sitejabber_reviews <- data.frame(usernames, contents, times, ratings)

# Trim white space
sitejabber_reviews$usernames <- trimws(sitejabber_reviews$usernames)
sitejabber_reviews$contents <- trimws(sitejabber_reviews$contents)
sitejabber_reviews$times <- trimws(sitejabber_reviews$times)
sitejabber_reviews$ratings <- trimws(sitejabber_reviews$ratings)

# Populate the data frame with the source
sitejabber_reviews$source <- rep("Site Jabber", nrow(sitejabber_reviews))

# Save the data frame to a csv
write.csv(sitejabber_reviews, "sitejabber_reviews.csv", row.names = FALSE)

