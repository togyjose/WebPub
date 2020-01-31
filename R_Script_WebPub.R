# R_Scrip_WebPub

# Please refer the ReadMe file to see how the input file needs to be structured.

#Script for analyzing messages on an Enterprise Social Network

setwd(<'working directory'>)
install.packages("dplyr")
install.packages("tidytext")
install.packages("readxl")
install.packages("lda")
library(lda)
library(dplyr)
library(tidytext)
library(readxl)
library(tidytext)
library(tidyr)
library(reshape2)

# Extracts the sentiment dictionaries that given sentiment scores, 
# postive/negative tagging and emotion association like joy/curiosity/anxiety etc.
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


# Convert data from an .xlsx to a R dataframe
cons_org <- read_excel(<'Messages file in xls format'>)
cons_org

# Review the structure of the file
str(cons_org)

# Convert dataframe into tidy data format
tidy_messages <- cons_org %>% unnest_tokens(word, Message_Body)
tidy_messages

# Get stop words list
data(stop_words)

# Remove stop words from messages data
tidy_messages <- tidy_messages %>%
  anti_join(stop_words)

# Get wordwise count
tidy_messages %>%
  count(word, sort = TRUE) 

# Visualize represent word frequency 
library(ggplot2)
tidy_messages %>%
  count(word, sort = TRUE) %>%
  filter(n > 1500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Use the Bing dictionary to assign positive or negative sentiment and 
# generate a sentiment score for each message

library(tidyr)
tidy_messages_sentiment <- tidy_messages %>% #
  inner_join(get_sentiments("bing")) %>%
  count(Group_Name, index = Month_Index, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive - negative)

# View sentiment scores for each message
tidy_messages_sentiment


# By-group view of change in sentiment month-on-month
library(ggplot2)
ggplot(tidy_messages_sentiment, aes(index, sentiment, fill = Group_Name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group_Name, ncol = 3, scales = "free_x")

# We analyze data from a specfic group - "Book_Review"
Book_Review_messages <- tidy_messages %>%
  filter(Group_Name == 'Book_Review') %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
# Review how each word is contributing to overall sentiment
Book_Review_messages

# Convert Book_Review data into R tidydata format
Book_Review_sentiment <- tidy_messages_sentiment %>%
  filter(Group_Name == 'Book_Review')
Book_Review_sentiment

# Within the Book_Review community, analyze which terms are contributing 
# most to positive or negative sentiment.
Book_Review_tidy_messages %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Wordcloud for visualizing the key words used in the community
library(wordcloud)
tidy_messages %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 500))

# Use the wordcloud analysis to add new words to your Stop Words list.
# For example - in your anaylsis if you notice the following words are repeating
# often but dont add much value, then add them to your stop words using this code.
custom_stop_words <- bind_rows(tibble(word = c("user","cloud","nice", "ha", "tag", NA, "www.yammer.com", "cognizant.com", "https", "3", "6", "101"),
                                      lexicon = c("custom")),
                               stop_words)

## Identify truly relevant words using the 
## tf-idf (term frequency - inter document frequency construct). We use the 
## 'bind_tf_idf' to get clarity on words that truly relevant to a group.

library(dplyr)
library(janeaustenr)
library(tidytext)

# Convert individual words data in R tidydata format
group_words <- tidy_messages %>%
  unnest_tokens(word, Message_Body) %>%
  count(Group_Name, word, sort = TRUE)
group_words

# Get the total word count at a Group Level
total_words_m <- group_words %>%
  group_by (Group_Name) %>%
  summarize (total = sum(n))
total_words_m

# Merge the above two datasets to get see how often is a word is repeating (n) and
# what is the total word count of that Group (total). The term frequency will be n/total
group_words <- left_join(group_words,total_words_m) %>%
  anti_join(custom_stop_words) 
group_words

group_words_tfidf <- group_words %>%
  bind_tf_idf(word, Group_Name, n)

#Generates a tf-idf score for each word.
group_words_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
group_words

## Generating ngrams

# Scans the entire corpus and generates word pairs. For example - the phrase "I work here"
# will generate - "I work" and "work here"
library(dplyr)
library(tidytext)
group_bigrams <- group_words %>%
  unnest_tokens(bigram, Message_Body, token = "ngrams", n = 2) %>%
  select(-word, -n, -total)
group_bigrams

# Each bigram is seperated into two words for further analysis.
library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove bigrams which may contain stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Get Count of bigrams
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

# Join the bigrams back together
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united

# Generate trigrams
group_trigrams <- cons_org %>%
  unnest_tokens(trigram, Message_Body, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$word,
         !word2 %in% custom_stop_words$word,
         !word3 %in% custom_stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
group_trigrams

## Topic Modeling. For Topic modelling the data needs to modified from tidy data format 
# to the document-term-matrix format or dtm format. 
# This is done using the cast_dtm function

install.packages("reshape2")
library(reshape2)
library(topicmodels)

tidy_messages <- tidy_messages %>%
  count(Group_Name, Message_ID_int, word) 
tidy_messages

tidy_messages_dtm <- tidy_messages %>% 
  cast_dtm(Message_ID_int, word, n)
tidy_messages_dtm

# This is where the actual Topic are generated using the LDA function.
tidy_messages_dtm_lda <- LDA(tidy_messages_dtm, k=13, control = list(seed=1234))
tidy_messages_dtm_lda

# Get the per-topic-per-word probability i.e. for each topic 
# which words have the highest probability of being present 
# (also referred to as beta)
messages_topics <- tidy(tidy_messages_dtm_lda, matrix = "beta")
message_topics

# We investigate further to see which are the top 5 words in each topic
# This is give an idea of what the topic is
messages_top_terms <- message_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) 
messages_top_terms

# We can also visually inspect the top words in each topic
library(ggplot2)
message_top_terms %>%
  mutate(word = reorder_within(word, beta, topic)) %>%
  ggplot(aes(word, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Next we measure the probability of each topic belonging to each message 
# (also referred to as gamma).

messages_gamma <- tidy(tidy_messages_dtm_lda, matrix = "gamma")
messages_gamma

# By sorting the message by gamma value we can now effectively predict 
# which topic it belongs to.

####
# Now we have effectively classified our collection of messages into 
# topics / groups in an automated scalable manner. 
# Please reach out to togyjose@hrness.in if you have any queries.
####

