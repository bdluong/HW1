library(tidyverse) #For data manuipulation
library(rtweet) #For scraping tweets
library(sentimentr) #For Financial Sentiment Engineering
library(tidytext) #For Ngram engineering
library(quantmod) #For scraping financial data

#Queries SPY and VIX market data 
getSymbols(c("SPY","^VIX"))
#Converts XTS to Dataframe 
SPY <- SPY %>% fortify()
VIX <- VIX %>% fortify()
#Renames Columns 
SPY <- SPY %>% rename(., date = "Index")
VIX <- VIX %>% rename(., date = "Index")
colnames(VIX) <- str_replace(colnames(VIX), "VIX.", "")
colnames(SPY) <- str_replace(colnames(SPY), "SPY.", "")
#Formats Dates 
SPY$date <- SPY$date %>% as.Date()
VIX$date <- VIX$date %>% as.Date()
#Generates Median Price Feature
SPY <- SPY %>% mutate(Median = (High + Low) / 2)
VIX <- VIX %>% mutate(Median = (High + Low) / 2)

#Reads in Twitter API token for scraping 
twitterToken <- readRDS("twitter_token.rds")
#Scrapes Twitter Data
df <- get_timeline("realdonaldtrump", n = 3200)
#Cleans Twitter text by removing numbers and links 
df$text <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", df$text)
df$text <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", df$text))
#Formats twitter columns 
df$date <- format(df$created_at, "%Y-%m-%d")
df$date <- as.Date(df$date, "%Y-%m-%d")
#Generates Day names for easier filtering 
df$day <- df$created_at %>% format("%A")
#Creates slack feature so tomorrow market data can be paired with current data 
df$marketDate <- df$date %>% as.Date("%Y-%m-%d") + 1


#Generates Bag of Words list
BOW <- df %>% 
  filter(is_retweet == FALSE) %>% 
  select(date, text) %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stopwordslangs %>% 
              filter(lang == "en") %>% 
              select(word)) %>% 
  group_by(date) %>% 
  count(word, word)

#Generates Bigram list
Bigram <- df %>% 
  filter(is_retweet == FALSE) %>% 
  select(date, text) %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1","word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  unite(bigram, c("word1", "word2"), sep = " ") %>% 
  group_by(date) %>% 
  count(bigram, bigram) %>% 
  arrange(-n)

#Generates Trigram list
triGram <- df %>% 
  filter(is_retweet == FALSE) %>% 
  select(date, text) %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1","word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  unite(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  group_by(date) %>% 
  count(trigram, trigram) %>% 
  arrange(-n)


#Generates Daily received Favorites, Daily received Retweets, Count of Daily Tweets, Daily Favs/Tweet, and Daily Rts/Tweet
Tweetdata <- df %>% 
  filter(is_retweet == FALSE) %>% 
  select(date, favorite_count, retweet_count) %>% 
  group_by(date) %>% 
  summarise(daily_fav = sum(favorite_count), 
            daily_rt = sum(retweet_count), 
            daily_tweet = n()) %>% 
  mutate(daily_fav_per_tweet = daily_fav / daily_tweet, 
         daily_rt_per_tweet = daily_rt / daily_tweet)


#Generates Daily Average Financial Tweet Sentiment 
Sentiment <- df %>% 
  select(date, text) %>% 
  get_sentences(text) %>% 
  sentiment(dpolarity_dt = lexicon::hash_sentiment_loughran_mcdonald) %>% 
  select(date, sentiment) %>% 
  group_by(date) %>% 
  summarise(sentiment = mean(sentiment))
Sentiment$date <- as.Date(Sentiment$date, "%Y-%m-%d")

#Combines tweet information ,sentiment, and adds next day market data to day of tweets
#Each row will contains the day's tweet data, market data, and the next day's market data for future models/analysis 
featureDf <- df %>% 
  filter(!day %in% c("Friday", "Saturday", "Sunday")) %>% 
  select(date, marketDate, day) %>%
  unique() %>% 
  left_join(Tweetdata, by = c("date" = "date")) %>% 
  left_join(Sentiment, by = c("date" = "date")) %>% 
  left_join(SPY %>% rename(.,"SPY_Current_Day_Open" = Open,
                           "SPY_Current_Day_High" = High,
                           "SPY_Current_Day_Low" = Low,
                           "SPY_Current_Day_Close" = Close,
                           "SPY_Current_Day_Volume"= Volume,
                           "SPY_Current_Day_Adjusted"= Adjusted,
                           "SPY_Current_Day_Median" = Median), by = c("date"="date")) %>% 
  left_join(VIX %>% rename(.,"VIX_Current_Day_Open" = Open,
                           "VIX_Current_Day_High" = High,
                           "VIX_Current_Day_Low" = Low,
                           "VIX_Current_Day_Close" = Close,
                           "VIX_Current_Day_Volume"= Volume,
                           "VIX_Current_Day_Adjusted"= Adjusted,
                           "VIX_Current_Day_Median" = Median), 
            by = c("date"="date")) %>% 
  left_join(SPY %>% rename(.,"SPY_Next_Day_Open" = Open,
                           "SPY_Next_Day_High" = High,
                           "SPY_Next_Day_Low" = Low,
                           "SPY_Next_Day_Close" = Close,
                           "SPY_Next_Day_Volume"= Volume,
                           "SPY_Next_Day_Adjusted"= Adjusted,
                           "SPY_Next_Day_Median" = Median), 
            by = c("marketDate" = "date")) %>%
  left_join(VIX %>% rename(.,"VIX_Next_Day_Open" = Open,
                           "VIX_Next_Day_High" = High,
                           "VIX_Next_Day_Low" = Low,
                           "VIX_Next_Day_Close" = Close,
                           "VIX_Next_Day_Volume"= Volume,
                           "VIX_Next_Day_Adjusted"= Adjusted,
                           "VIX_Next_Day_Median" = Median),
            by = c("marketDate" = "date"))