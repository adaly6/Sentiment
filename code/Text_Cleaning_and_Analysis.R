# Clear the environment
rm(list=ls())

# Install and load necessary packages for text mining techniques
install.packages(c("tidytext", "dplyr", "ggplot2", "tm", "wordcloud", "RColorBrewer", "stringr", "zoo"))
library(tidytext)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(zoo)

# Import all source csv
all_source <- read.csv("all_source.csv")

# Create a Corpus from the text column
corpus <- Corpus(VectorSource(all_source$Content))

# Text cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# corpus <- tm_map(corpus, stemDocument) Remove to to over truncating

# Extract the text from the Corpus
texts <- sapply(corpus, as.character)

# Create a data frame with the text
text_df <- data.frame(Content = texts, stringsAsFactors = FALSE)

# Add cleaned text to all_source
all_source$Content_Cleaned <- text_df$Content

# Sentiment analysis using a pre-built sentiment lexicon
bing_lexicon <- get_sentiments("bing")

# Function to calculate net sentiment score for each row
calculate_net_sentiment <- function(review, lexicon) {
  # Count positive and negative words
  positive_words <- sum(lexicon$word[lexicon$sentiment == "positive"] %in% unlist(strsplit(review, "\\s+")))
  negative_words <- sum(lexicon$word[lexicon$sentiment == "negative"] %in% unlist(strsplit(review, "\\s+")))
  
  # Calculate net sentiment score
  net_sentiment <- positive_words - negative_words
  return(net_sentiment)
}

# Calculate net sentiment score for each row in all_source
all_source$Sentiment <- sapply(all_source$Content_Cleaned, function(x) {
  calculate_net_sentiment(x, bing_lexicon)
})

# Fill missing sentiment scores with 0
all_source$Sentiment[is.na(all_source$Net_Sentiment)] <- 0



## Show frequency of words

# One word word cloud #

# Word Cloud with word frequency filtering
word_freq <- all_source %>%
  unnest_tokens(word, Content_Cleaned) %>%
  count(word, sort = TRUE)

# Set a threshold for word frequency
freq_threshold <- 50
word_freq_filtered <- word_freq %>%
  filter(n >= freq_threshold)

# Word Cloud
wordcloud(words = word_freq_filtered$word, freq = word_freq_filtered$n,
          min.freq = 1, scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"))


# Two word word cloud #

# Create bigrams from the cleaned content
bigrams <- all_source %>%
  unnest_tokens(bigram, Content_Cleaned, token = "ngrams", n = 2)

# Count the frequency of bigrams
bigram_freq <- bigrams %>%
  count(bigram, sort = TRUE)

# Set a threshold for bigram frequency
freq_threshold_bigram <- 10
bigram_freq_filtered <- bigram_freq %>%
  filter(n >= freq_threshold_bigram)

# Set the maximum number of words in the word cloud
max_words <- 100

# Create a vector of colors (you can customize this)
custom_colors <- c("red", "blue", "green", "purple", "orange")

# Word Cloud for bigrams with multicolored words
wordcloud(words = bigram_freq_filtered$bigram, freq = bigram_freq_filtered$n,
          min.freq = 1, max.words = max_words, scale = c(3, 0.5),
          colors = custom_colors)


# Word Frequency Bar Plot

all_source %>%
  unnest_tokens(word, Content_Cleaned) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Top 10 Most Frequent Words", x = "Word", y = "Frequency") +
  coord_flip()


# Word Frequency Bar Plot with removed common words

# Create a vector of words to filter out
common_words_to_remove <- c("order", "just", "ordered", "fanatics", "customer", "will", "get", "company", "item", "one", "now", "back", "jersey")

# Word Frequency Bar Plot with removed common words
all_source %>%
  unnest_tokens(word, Content_Cleaned) %>%
  count(word, sort = TRUE) %>%
  # Filter out common words
  anti_join(data.frame(word = common_words_to_remove), by = "word") %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Top 10 Most Frequent Words (Filtered)", x = "Word", y = "Frequency") +
  coord_flip()


## Words of interest 

# Identify specific words for exploration
words_of_interest <- c("service", "shipping", "return", "never", "time", "refund", "received") 

# Create bigrams from the cleaned content
bigrams <- all_source %>%
  unnest_tokens(bigram, Content_Cleaned, token = "ngrams", n = 2)

# Filter bigrams related to specific words of interest
filtered_bigrams <- bigrams %>%
  filter(str_detect(bigram, paste(words_of_interest, collapse = "|")))

# Count the frequency of filtered bigrams
filtered_bigram_freq <- filtered_bigrams %>%
  count(bigram, sort = TRUE)

# Set a threshold for filtered bigram frequency
freq_threshold_filtered_bigram <- 5
filtered_bigram_freq_filtered <- filtered_bigram_freq %>%
  filter(n >= freq_threshold_filtered_bigram)

# Create a vector of colors (you can customize this)
custom_colors <- c("red", "blue", "green", "purple", "orange")

# Word Cloud for filtered bigrams with multicolored words
wordcloud(words = filtered_bigram_freq_filtered$bigram, freq = filtered_bigram_freq_filtered$n,
          min.freq = 1, max.words = max_words, scale = c(3, 0.5),
          colors = custom_colors)


## Average Sentiment for Words of Interest

# Filter rows containing words of interest
filtered_rows <- all_source %>%
  filter(str_detect(Content_Cleaned, paste(words_of_interest, collapse = "|")))

# Create a data frame with words of interest
words_of_interest_df <- data.frame(Word = words_of_interest)

# Calculate average sentiment for each word in the vector words_of_interest
words_of_interest_df$Average_Sentiment <- sapply(words_of_interest_df$Word, function(word) {
  word_rows <- filtered_rows %>%
    filter(str_detect(Content_Cleaned, word))
  mean(word_rows$Sentiment, na.rm = TRUE)
})

words_of_interest_df



## Sentiment scores by source

# Sentiment by Source
sentiment_by_source <- all_source %>%
  group_by(Source) %>%
  summarise(AverageSentiment = mean(Sentiment))
sentiment_by_source

# Sentiment distribution by source
ggplot(all_source, aes(x = Sentiment, fill = Source)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Sentiment Distribution by Source", x = "Sentiment", y = "Count") +
  theme_minimal()

# Heatmap for Relationship between Engagement and Sentiment with larger dots
ggplot(all_source, aes(x = Engagement, y = Sentiment, fill = ..count..)) +
  geom_bin2d(binwidth = c(20, 0.7), drop = TRUE) +  # Adjust the binwidth for larger dots
  scale_fill_gradient(low = "blue", high = "red", guide = "legend") +
  labs(title = "Relationship between Engagement and Sentiment", x = "Engagement", y = "Sentiment") +
  theme_minimal()

# Sentiment by Trust Pilot with Rating
sentiment_for_trustpilot <- all_source %>%
  filter(Source == "Trust Pilot") %>%
  summarise(AverageSentiment = mean(Sentiment),
            AverageRating = mean(Rating))
sentiment_for_trustpilot



## Time Series

# Convert the "Date" column to a Date format
all_source$Date <- as.Date(all_source$Date, format = "%Y-%m-%d")

# Calculate average sentiment per month
average_sentiments <- all_source %>%
  group_by(YearMonth = format(Date, "%Y-%m")) %>%
  summarise(AverageSentiment = mean(Sentiment))

# Convert YearMonth to a date format
average_sentiments$Date <- as.Date(as.yearmon(average_sentiments$YearMonth))

# Plot using ggplot
ggplot(average_sentiments, aes(x = Date, y = AverageSentiment)) +
  geom_line() +
  labs(title = "Average Sentiment Over Time", x = "Date", y = "Average Sentiment") +
  theme_minimal()

