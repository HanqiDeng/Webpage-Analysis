# Import libraries
library(ggplot2)
library(googleVis)
library(tm)
library(rvest)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(SnowballC)
library(reshape2)

# Get a list of all the files in the folder
file_names <- list.files("~/Desktop/BU/688/project/data/", pattern = ".html")

# Create empty list to store text content of each file
text_content_list <- list()

# Read each file 
for (i in 1:length(file_names)) {
  html_file <- read_html(file.path("~/Desktop/BU/688/project/data/", file_names[i]))
  text_content <- html_nodes(html_file, "body") %>%
    html_text()
  text_content_list[[i]] <- text_content
}

# Create a data frame with one row per word
words_df <- data_frame(text_content_list) %>%
  unnest_tokens(word, text_content_list)

# Remove stop words and punctuation
stop_words <- tibble(word = stopwords("english"), stringsAsFactors = FALSE)
words_df_clean <- anti_join(words_df, stop_words, by = "word") %>%
  filter(!str_detect(word, "[^[:alpha:]]"))

top_75_words <- words_df_clean %>%
  count(word) %>%
  arrange(desc(n)) %>%
  top_n(75)

# Plot top 75 words in bar chart
ggplot(top_75_words, aes(x = n, y = reorder(word, n))) +
  geom_bar(stat = "identity") +
  labs(title = "Top 75 words", x = "Count", y = "Words")

# Create a corpus from text_content_list
corpus <- Corpus(VectorSource(text_content_list))

# Clean and preprocess the corpus
corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(tolower) %>%
  tm_map(stemDocument)

# Create a document term matrix
dtm <- DocumentTermMatrix(corpus)

# Calculate distances between documents and cluster them
dist_matrix <- dist(dtm)
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Plot a dendrogram
plot(hclust_result, hang = -1)

# convert dtm to a data frame 
doc_word_counts <- as.data.frame(as.matrix(dtm))
doc_word_counts <- doc_word_counts[, colnames(doc_word_counts) %>% nchar() <= 10000]

# Get the top 3 words for each document
top_words <- t(apply(doc_word_counts, 1, function(x) {
  names(x)[order(-x)][1:3]
}))

# Get the corresponding word counts for the top 3 words for each document
top_counts <- t(apply(doc_word_counts, 1, function(x) {
  sort(x, decreasing = TRUE)[1:3]
}))

# Create the index column
index_col <- rep(1:30)

# Create the word rank column
word_rank_col <- rep(1:3, 10)

# Create the word name column
word_name_col <- melt(t(top_words))["value"]

# Create the word count column
word_count_col <- melt(t(top_counts))["value"]

# Combine the columns into the top_words_df dataframe
top_words_df <- data.frame(index = index_col,
                           word_rank = word_rank_col,
                           word_name = word_name_col,
                           word_count = word_count_col)
colnames(top_words_df) <- c("index","wordrank","name","count")
top_words_df$wordrank <- as.character(top_words_df$wordrank)

# Create a bubble chart
bubble <- gvisBubbleChart(top_words_df, idvar = "name", xvar = "index", yvar = "count", colorvar = "wordrank", sizevar = "count", options = list(colorAxis = "{colors:['blue','red','yellow']}", hAxis.title = "Word Number", vAxis.title = "Word Frequency Count"))

plot(bubble)