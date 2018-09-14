setwd('/work')
library(packrat)
on()
library(stringr)
library(tidyverse)
library(readr)

df <- read_csv('<PATH-TO-DF>')
createFE = function(ds)
{
  ds = ds %>%
  mutate(Ncharacters= nchar(ds$NOTE_TEXT))
    mutate(Ncommas = stringr::str_count(ds$NOTE_TEXT, ',')) %>%
    mutate(Nsemicolumns = stringr::str_count(ds$NOTE_TEXT, ';')) %>%
    mutate(Ncapitalfirst = stringr::str_count(ds$NOTE_TEXT, '[A-Z][a-z]')) %>%
    mutate(Ncapital = stringr::str_count(ds$NOTE_TEXT, '[A-Z]')) %>%
    mutate(Ncolons = stringr::str_count(ds$NOTE_TEXT, ':')) %>%
    mutate(Nblank = stringr::str_count(ds$NOTE_TEXT, ' ')) %>%
    mutate(Nqmark = str_count(ds$NOTE_TEXT, '\\?')) %>%
    mutate(Nother = stringr::str_count(ds$NOTE_TEXT, '[\\.\\.]')) %>%
    mutate(Nnumber = stringr::str_count(ds$NOTE_TEXT,'[:digit:]')) %>%
    mutate(punctuation_count = stringr::str_count(ds$NOTE_TEXT,'[:punct:]'))
  return(ds)
}

newdf = createFE(df)
write_csv('/work/data/<NAME-OF-NEW-DF-WITH-FE>', append=FALSE)