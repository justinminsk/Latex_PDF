library(tidytext)
library(tm)
library(wordcloud)
library(stringr)
library(dplyr)
library(knitr)

#--------------

lines <- scan("lyrics.txt", what = character(), sep = '\n')
#take all of the lyrics from the text doc and create an array by line

lines_df <- data_frame(line = 1:119, text = lines)
#make a data frame with na index and each line

words_df <- lines_df%>%
  unnest_tokens(word, text)
#store the words and maintain what line they where on

words_df <- words_df%>%
  filter(!(word %in% stop_words$word))
#takes out all of the common words form the list of words form the songs

words_free <- words_df%>%
  group_by(word)%>%
  summarise(count = n())
#get a count of the words

wordcloud(words_free$word, words_free$count, colors =  brewer.pal(5, "Dark2"))
#create a wordcloud

#-------------

library(janeaustenr)

sns <- austen_books()
#bring in all of jane austen's books

sns <- sns%>%
  filter(book == "Sense & Sensibility")
#filter to just get Sense and Sensibility text

sns$book <- as.character(sns$book)
#un factor the book column

str_detect(sns$text, "^CHAPTER")
#make a bool list with true for anyhting begining with CHAPTER

sns <- sns%>%
  filter(!str_detect(sns$text, "^CHAPTER"))
#filter out all of the CHAPTER lines

sns[1:11, ]
#see only row 1 -11 all columns

sns <- sns[12:12574, ]
#make sns start after the title 

sns <- sns[1:12560, ]
#get rid of the end

words_df <- sns%>%
  unnest_tokens(word, text)
#split the lines into words

words_df <- words_df%>%
  filter(!(word %in% stop_words$word))
#get rid of common words that are not unique (the, a, etc.)

words_free <- words_df%>%
  group_by(word)%>%
  summarise(count = n())
#make a count of the word

wordcloud(words_free$word, words_free$count, min.freq = 25)
#make a word cloud

#---------------

library(gutenbergr)

packages <- c('dplyr', 'stringr', 'tidytext', 'tm', 'wordcloud')
knitr::write_bib(packages, 'bib.bib')
#add packages to references 

gutenberg_works(author == str_extract(author, "Poe, Edgar Allan"))
#see Poes work's IDs

df <- gutenberg_download(10031)
#get the complete poetical works of Edgar Allan Poe

words_df <- df%>%
  unnest_tokens(word, text)
#split the lines into words

words_df <- words_df%>%
  filter(!(word %in% stop_words$word))
#get rid of common words that are not unique (the, a, etc.)

words_df <- words_df%>%
  filter(!word == "thy" & !word == "thou" & !word == "thee")
#some older words not in out stop_word list that are not useful

words_free <- words_df%>%
  group_by(word)%>%
  summarise(count = n())%>%
  arrange(-count)
#make a count of the word

wordcloud(words_free$word, words_free$count, min.freq = 25)
#make a word cloud

#-------------

sentiments
#list of dicts of sentiments with assinged to words

get_sentiments('nrc')
#gives words a sentiment like fear, or happy, etc.

gutenberg_works(title == "Dracula")
#get Dracula id

nrc <- get_sentiments('nrc')
#put the nrc into a dataframe

nrc_fear <- nrc%>%
  filter(sentiment == 'fear')
#get only the fear words from nrc

dracula <- gutenberg_download(345)
#get dracula text

dracula_words <- dracula%>%
  unnest_tokens(word, text)
#split the lines into words

dracula_fear_words <- inner_join(nrc_fear, dracula_words)
#find words in dracula that also are in nrc_fear

dfw_free <- dracula_fear_words%>%
  group_by(word)%>%
  summarise(count = n())
#make a count of the word

wordcloud(dfw_free$word, dfw_free$count, min.freq = 7)
#create a word cloud of the fear words
