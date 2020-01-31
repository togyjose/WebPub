# R_Scrip_WebPub

#Script for analyzing messages on an Enterprise Social Network

setwd(<working directory>)
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
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


# Convert data from an .xlsx to a R dataframe
cons_org <- read_excel("Consolidated_Original.xlsx")

cons_org$Sender_ID_int <- as.integer(cons_org$`Sender ID`)
cons_org$Receiver_ID_int <- as.integer(cons_org$`Receiver ID`)
cons_org$Message_ID_int <- as.integer(cons_org$`Message ID`)
cons_org$Reply_ID_int <- as.integer(cons_org$`Reply ID`)
cons_org$Thread_ID_int <- as.integer(cons_org$`Thread ID`)
cons_org$Emp_ID_int <- as.integer(cons_org$`Emp ID`)

cons_org[c("Thread ID","Sender ID", "Receiver ID", "Message ID", "Reply ID", "Emp ID", 
           "Created At", "Original Post", "Grade Description", "Grade Code",
           "Region", "Gender", "Sender Email", "Sender_ID_int", "Receiver_ID_int", "Reply_ID_int", 
           "Thread_ID_int", "Emp_ID_int", "Trunc1", "Month", "Year")] <- list(NULL)

cons_org
str(cons_org)

tidy_messages <- cons_org %>% unnest_tokens(word, Message_Body)
tidy_messages

data(stop_words)

tidy_messages <- tidy_messages %>%
  anti_join(stop_words)

tidy_messages %>%
  count(word, sort = TRUE) 

library(ggplot2)

tidy_messages %>%
  count(word, sort = TRUE) %>%
  filter(n > 1500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_messages %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

str(tidy_messages)

library(tidyr)

tidy_messages_sentiment <- tidy_messages %>% #
  inner_join(get_sentiments("bing")) %>%
  count(Group_Name, index = Month_Index, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive - negative)

str(tidy_messages_sentiment)
tidy_messages_sentiment

# jane_austen_sentiment <- tidy_books %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(book, index = linenumber %/% 80, sentiment) %>%
#   spread(sentiment, n, fill = 0) %>%
#   mutate(sentiment = positive - negative)

# library(ggplot2)
# 
# ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~book, ncol = 2, scales = "free_x")


library(ggplot2)

ggplot(tidy_messages_sentiment, aes(index, sentiment, fill = Group_Name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group_Name, ncol = 3, scales = "free_x")

# pride_prejudice <- tidy_books %>% 
#   filter(book == "Pride & Prejudice")
# 
# pride_prejudice

movie_buff_messages <- tidy_messages %>%
  filter(Group_Name == 'Movie Buffs') %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
movie_buff_messages

movie_buffs_sentiment <- tidy_messages_sentiment %>%
  filter(Group_Name == 'Movie Buffs')
movie_buffs_sentiment

library(ggplot2)

ggplot(movie_buffs_messages, aes(index, sentiment, fill = Group_Name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group_Name, ncol = 3, scales = "free_x")

movie_buff_tidy_messages <- tidy_messages %>%
  inner_join(get_sentiments("bing")) %>%
  filter(Group_Name == 'Movie Buffs') %>%
  count(Group_Name, sentiment, word, sort = TRUE)
movie_buff_tidy_messages

movie_buff_tidy_messages %>%
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

# library(wordcloud)
# 
# tidy_messages %>%
#   anti_join(stop_words) %>%
#   count(word) %>%
#   with(wordcloud(word, n, max.words = 500))

#In the wordcloud we notice "user" is very visible - this is becuase "user"
#is part of every @mention in the message eg "@Richard" will be 
#printed as "User:123456" where 123456 is Richard's employee id. 
#So we will remove "user" from this corpus by adding it to the 
#Stop Words list

custom_stop_words <- bind_rows(tibble(word = c("user","cloud","nice", "ha", "tag", NA, "www.yammer.com", "cognizant.com", "https", "3", "6", "101"),
                                      lexicon = c("custom")),
                               stop_words)

custom_stop_words
# 
# tidy_messages %>%
#   anti_join(custom_stop_words) %>%
#   count(word) %>%
#   with(wordcloud(word, n, max.words = 750))
# 
# library(reshape2)
# 
# tidy_messages %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(word, sentiment, sort = TRUE) %>%
#   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
#   comparison.cloud(colors = c("gray20", "gray80"),
#                    max.words = 750)

# Identify truly relevant words using the tf-idf construct

library(dplyr)
library(tidytext)

library(dplyr)
library(janeaustenr)
library(tidytext)

group_words <- tidy_messages %>%
  #unnest_tokens(word, text) %>%
  count(Group_Name, word, sort = TRUE)

group_words

total_words_m <- group_words %>%
  group_by (Group_Name) %>%
  summarize (total = sum(n))

total_words_m

group_words <- inner_join(group_words,total_words_m) %>%
  anti_join(custom_stop_words) 

group_words

library(ggplot2)

ggplot(group_words, aes(n/total, fill = Group_Name)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~Group_Name, ncol = 2, scales = "free_y")


group_words_tfidf <- group_words %>%
  bind_tf_idf(word, Group_Name, n)
group_words

group_words_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
group_words

group_words_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Group_Name) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Group_Name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Group_Name, ncol = 2, scales = "free") +
  coord_flip()

# Generating ngrams
# 
library(dplyr)
library(tidytext)

group_bigrams <- group_words %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(-word, -n, -total)
group_bigrams


library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# austen_books() %>%
#   unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
#   filter(!word1 %in% stop_words$word,
#          !word2 %in% stop_words$word,
#          !word3 %in% stop_words$word) %>%
#   count(word1, word2, word3, sort = TRUE)
# 
group_trigrams <- cons_org %>%
  unnest_tokens(trigram, Message_Body, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$word,
         !word2 %in% custom_stop_words$word,
         !word3 %in% custom_stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
group_trigrams

###
###
# Topic Modeling
###
###

#install.packages("reshape2")
library(reshape2)
library(topicmodels)

tidy_messages <- tidy_messages %>%
  count(Group_Name, Message_ID_int, word) 
tidy_messages

tidy_messages_dtm <- tidy_messages %>% 
  cast_dtm(Message_ID_int, word, n)
tidy_messages_dtm

# set a seed so that the output of the model is predictable
#tidy_messages_dtm_lda <- LDA(tidy_messages_dtm, k=13, control = list(seed=1234))
#tidy_messages_dtm_lda


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

####
# Now we are effecitively classified our collection of messages into 
# topics / groups in an automated scalable manner.
####


