library(tidyverse)
library(ngramr)
library(stringr)

#set working directory
setwd("~/GitHub/anachronistic_text")

#read text file
text <- readLines("C:/Users/cmg3/Documents/example.txt")

#set target year
target.year <- 1875

#concatenate lines
text <- paste(text, collapse = " ") %>% gsub(pattern = "[^[:alpha:]|’]", replacement = " ") %>% 
  str_squish %>% str_to_lower()

#get the unique words
text <- str_split_1(text, " ") %>% unique() %>% sort()

#remove all the common words which we already have data for
common <- read_csv("common_words.csv") %>% select(Year,Phrase,Frequency,Corpus)
common.words <- common$Phrase %>% unique()
text.common <- text[text %in% common.words]
text <- text[!text %in% common.words]

#get rid of the ' 's ...
text <- text[!str_detect(text, "’")]

#turn the remaining uncommon words into chunks to be ngram'd
text.chunks <- chunk(text, len = 12)

#get the data for uncommon words
stats <- tibble()
for (i in 1:length(text.chunks)) {
  tryCatch({
    old <- ngram(text.chunks[[i]], year_start = target.year-1, year_end = target.year) %>% as_tibble()
    new <- ngram(text.chunks[[i]], year_start = 2018, year_end = 2019) %>% as_tibble()
    stats <- rbind(stats, new, old)
    }, error = function(e){
      print(paste("chunk error at chunk",i))
      for (n in 1:length(text.chunks[[i]])) {
        tryCatch({
          old <- ngram(text.chunks[[i]][n], year_start = target.year-1, year_end = target.year) %>% as_tibble()
          new <- ngram(text.chunks[[i]][n], year_start = 2018, year_end = 2019) %>% as_tibble()
          print(paste("chunk",i,"word",n))
          stats <- rbind(stats, new, old)
        }, error = function(e){
          print(paste("bad phrase at chunk",i,"phrase",n))
          print(paste(n,"again"))
          all <- ngram(text.chunks[[i]][n], year_start = target.year) %>% as_tibble()
          stats <- rbind(stats, all)
        })
      }
    })
  print(i/length(text.chunks))
}
stats <- filter(stats, Year == target.year | Year == max(Year)) %>% distinct() 

# #get the data for uncommon words
# stats <- tibble()
# for (i in 1:length(text.chunks)) {
#   old <- ngram(text.chunks[[i]], year_start = target.year-1, year_end = target.year) %>% as_tibble()
#   new <- ngram(text.chunks[[i]], year_start = 2018, year_end = 2019) %>% as_tibble()
#   stats <- rbind(stats, new, old)
#   print(i/length(text.chunks))
# }
# stats <- filter(stats, Year == target.year | Year == max(Year)) 

#get the data for common words
common.stats <- filter(common, Phrase %in% text.common & (Year == target.year | Year == max(Year)))

#combine data for common and uncommon words
stats <- rbind(stats, common.stats) %>% distinct()
stats$Phrase <- as.character(stats$Phrase)
stats <- arrange(stats, Phrase, Year)

#find words which are used at a much greater frequency in the modern year than in the target year
sus.ratio <- stats %>% group_by(Phrase) %>% filter(Year == target.year | Year == max(Year)) 
sus.ratio$Year_name <- as.character(sus.ratio$Year)
sus.ratio[sus.ratio$Year == min(sus.ratio$Year),"Year_name"] <- "Old"
sus.ratio[sus.ratio$Year == max(sus.ratio$Year),"Year_name"] <- "Modern"
sus.ratio <- sus.ratio %>% select(-Year) %>% pivot_wider(names_from = "Year_name", values_from = Frequency)
sus.ratio <- sus.ratio %>% mutate(Ratio = Old/Modern) %>% arrange(Ratio)
sus.ratio
