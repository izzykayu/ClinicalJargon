setwd('/work')
library(packrat)
on()
library(readr)

replace_missings <- function(x, replacement) {
 # replaces for a vector (column for the df)
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  message(sum(is_miss), ' missings replaced by the value ', replacement)
  x
}

# wherever your df is
# these are inspired by the avati paper which utilized a deep learning model to extract features such as Number of codes assigned in a day, mean codes, variance in codes, and etc
df <- read_csv('/work/data/<DF-WITH-CLINICAL-TEXT>.csv')
library(tokenizers)

tokenized <- lapply(df$text, function(x) return(tokenize_words(as.character(x))[[1]]))

df$Nwords <- unlist(lapply(tokenized, function(x) length(x)))
# this feature gives the number of words in the note
df$Nwords <-replace_missings(df$Nwords, 0)
df$Nunique <- unlist(lapply(tokenized, function(x) length(unique(x))))
# this feature gives the number of words that are unique in the note
df$Nunique <-replace_missings(df$Nunique, 0)
df$Nrepeated <- df$Nwords-df$Nunique
# this feature gives the number of words that are repeated in the note
df$Nrepeated <-replace_missings(df$Nrepeated, 0)
df$UniquenessRatio <- df$Nunique/df$Nwords
# this feature gives the Uniqueness ratio based on the number of unique words and the total number of words
df$UniquenessRatio <-replace_missings(df$UniquenessRatio, 0)
df$Wmean <- unlist(lapply(tokenized, function(x) mean(unlist(lapply(x, function(y) nchar(y))))))
# this feature gives the mean character length of words in clinical note
df$Wmean <-replace_missings(df$Wmean, 0)
df$Wmedian <- unlist(lapply(tokenized, function(x) median(unlist(lapply(x, function(y) nchar(y))))))
# this feature gives the median character length of words in clinical note
df$Wmedian <-replace_missings(df$Wmedian, 0)
df$Wsd <- unlist(lapply(tokenized, function(x) sd(unlist(lapply(x, function(y) nchar(y))))))
# this feature gives the standard deviation in word length in clinical note
df$Wsd <-replace_missings(df$Wsd, 0)

write_csv('/work/data/<NEW-DF-WITH-FE>.csv', append=FALSE)

