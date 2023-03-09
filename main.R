library(tidyverse)
library(ngramr)
library(stringr)

#set working directory
setwd("C:/Users/cmg3/Documents/GitHub/anachronistic_text")

#read text file
text <- readLines("example.txt")

#concatenate lines
text <- paste(text, collapse = " ") %>% gsub(pattern = "[^[:alpha:]|’]", replacement = " ") %>% 
  str_squish %>% str_to_lower()

#get the unique words
text <- str_split_1(text, " ") %>% unique() %>% sort()

#remove all the common words 
common <- read_csv("common_words.csv") %>% select(Year,Phrase,Frequency,Corpus)
common.words <- common$Phrase %>% unique()
text.common <- text[text %in% common.words]
text <- text[!text %in% common.words]

#get rid of the ' 's ...
text <- text[!str_detect(text, "’")]

#turn the remaining words into chunks to be ngram'd
text.chunks <- chunk(text, len = 12)

#set target year
target.year <- 1875

#get the data for uncommon words
stats <- tibble()
for (i in 1:length(text.chunks)) {
  new <- ngram(text.chunks[[i]], year_start = target.year-1, year_end = target.year) %>%
    as_tibble()
  stats <- rbind(stats, new)
  print(i/length(text.chunks))
}
stats <- filter(stats, Year == target.year) %>% arrange(Frequency)

#get the data for common words
common.stats <- filter(common, Phrase %in% text.common & Year == target.year)

stats <- rbind(stats, common.stats) %>% arrange(Frequency) %>% distinct()

#suspicious words?
sus.words <- filter(stats, Frequency < 1)
sus.words

#compare to frequency of modern usage
sus.words$Phrase <- as.character(sus.words$Phrase)
sus.chunks <- arrange(sus.words, Phrase) %>% .$Phrase %>% chunk(len = 12)
sus.stats <- tibble()
for (i in 1:length(sus.chunks)) {
  new <- ngram(sus.chunks[[i]], year_start = target.year) %>%
    as_tibble()
  sus.stats <- rbind(sus.stats, new)
  print(i/length(sus.chunks))
}

sus.ratio <- sus.stats %>% group_by(Phrase) %>% filter(Year == target.year | Year == max(Year)) %>% 
  pivot_wider(names_from = "Year", values_from = Frequency)
sus.ratio$Ratio <- sus.ratio[3]/sus.ratio[4]
names(sus.ratio[5]) <- "Ratio"
