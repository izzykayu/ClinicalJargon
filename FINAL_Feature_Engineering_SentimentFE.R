setwd('/work')
library(packrat)
on()
library(readr)
library(tidyverse)
library(tidytext)
library(DT)
library(wordcloud)
library(igraph)
library(tm)
library(caret)
library(RColorBrewer)
library(NLP)
library(stringr)


df <- read_csv('<PATH-TO-THE-DATA-FRAME>')

# the get sentiments fxn is from the tidytext package

nrcFear <- get_sentiments("nrc") %>% filter(sentiment=="fear")
nrcTrust <- get_sentiments("nrc") %>% filter(sentiment=="trust")
get_sentiments("nrc") %>% group_by(sentiment) %>% tally()
nrcAnger <- get_sentiments("nrc") %>% filter(sentiment=="anger")
nrcAnticipation <-get_sentiments("nrc") %>% filter(sentiment=="anticipation") 
nrcDisgust <-get_sentiments("nrc") %>% filter(sentiment=="disgust") 
nrcJoy <-get_sentiments("nrc") %>% filter(sentiment=="joy") 
nrcSadness <-get_sentiments("nrc") %>% filter(sentiment=="sadness")
nrcSurprise <-get_sentiments("nrc") %>% filter(sentiment=="surprise") 
query_words <- c(':', '.', ';', ',', '?', '__num__')

getSentiScore = function(df)
{
  sentiment_lines  =  df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% 
  filter(!word %in% stopwords("en")) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(sentiment = mean(score))

sentiment_lines = sentiment_lines %>%
    right_join(df, by = "id") 

sentiment_lines = sentiment_lines %>% mutate(sentiment = ifelse(is.na(sentiment),0,sentiment))

return(sentiment_lines$sentiment)

}

# N_Anger words
 df<-df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% filter(!word %in% stopwords("en")) %>%
  group_by(id, word) %>% tally() %>% inner_join(nrcAnger, by="word") %>% group_by(id) %>% summarize(N_Anger = sum(n)) %>% right_join(df, by = "id")
# N_Disgust raw counts of words under "disgust" emotion according to NRC Lexicon after stop word and query word removal

df <- df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% filter(!word %in% stopwords("en")) %>%
  group_by(id, word) %>% tally() %>% inner_join(nrcDisgust, by="word") %>% group_by(id) %>% summarize(N_Disgust = sum(n)) %>% right_join(df, by = "id")
  
 df<-df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% filter(!word %in% stopwords("en")) %>%
  group_by(id, word) %>% tally() %>% inner_join(nrcJoy, by="word") %>% group_by(id) %>% summarize(N_Joy = sum(n)) %>% right_join(df, by = "id")
  
df <- df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% filter(!word %in% stopwords("en")) %>%
  group_by(id, word) %>% tally() %>% inner_join(nrcSadness, by="word") %>% group_by(id) %>% summarize(N_Sadness = sum(n)) %>% right_join(df, by = "id")
  
 df<-df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% filter(!word %in% stopwords("en")) %>%
  group_by(id, word) %>% tally() %>% inner_join(nrcFear, by="word") %>% group_by(id) %>% summarize(N_Fear = sum(n)) %>% right_join(df, by = "id")
  
df <- df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% filter(!word %in% stopwords("en")) %>%
  group_by(id, word) %>% tally() %>% inner_join(nrcTrust, by="word") %>% group_by(id) %>% summarize(N_Trust = sum(n)) %>% right_join(df, by = "id")
  
df <- df %>%
  unnest_tokens(word, text) %>% filter(!word %in% query_words) %>% filter(!word %in% stopwords("en")) %>%
  group_by(id, word) %>% tally() %>% inner_join(nrcAnticipation, by="word") %>% group_by(id) %>% summarize(N_Anticipation = sum(n)) %>% right_join(df, by = "id")
  
# recall id is a unique row identifier for patient notes

df$SentimentScore <- getSentiScore(df)
