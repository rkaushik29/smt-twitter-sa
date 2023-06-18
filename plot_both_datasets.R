library(dplyr)
library(tidyverse)
library(lubridate)
library(syuzhet)
library(ggplot2)

riot_tweets_biden = read.csv("resources/riot_tweets_biden.csv")
riot_tweets_trump = read.csv("resources/riot_tweets_trump.csv")  

riot_posts_biden = read.csv("resources/riot_posts_biden.csv")
riot_posts_trump = read.csv("resources/riot_posts_trump.csv")  




# convert timestamp to datetime 
riot_tweets_trump$timestamp <- ymd_hms(riot_tweets_trump$date)
riot_tweets_biden$timestamp <- ymd_hms(riot_tweets_biden$date)

# convert timestamp to datetime 
riot_posts_trump$timestamp <- ymd_hms(riot_posts_trump$date)
riot_posts_biden$timestamp <- ymd_hms(riot_posts_biden$date)


# aggregate sentiment scores per minute
biden_sentiment_twit <- riot_tweets_biden %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

biden_sentiment_twit_syuzhet <- riot_tweets_biden %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment_syuzhet))

trump_sentiment_twit <- riot_tweets_trump %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

trump_sentiment_twit_syuzhet <- riot_tweets_trump %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment_syuzhet))


biden_sentiment_red <- riot_posts_biden %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

biden_sentiment_red_syuzhet <- riot_posts_biden %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment_syuzhet))

trump_sentiment_red <- riot_posts_trump %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment))

trump_sentiment_red_syuzhet <- riot_posts_trump %>%
  mutate(minute = floor_date(timestamp, "minute")) %>%
  group_by(minute) %>%
  summarize(avg_sentiment = mean(sentiment_syuzhet))



##### Analysis
########################


# adding rolling means (k=10 has been chosen arbitrarily)
biden_sentiment_twit$rollmean = zoo::rollmean(biden_sentiment_twit$avg_sentiment,10, fill = NA)
biden_sentiment_twit_syuzhet$rollmean = zoo::rollmean(biden_sentiment_twit_syuzhet$avg_sentiment,10, fill = NA)

trump_sentiment_twit$rollmean = zoo::rollmean(trump_sentiment_twit$avg_sentiment,10, fill = NA)
trump_sentiment_twit_syuzhet$rollmean = zoo::rollmean(trump_sentiment_twit_syuzhet$avg_sentiment,10, fill = NA)

biden_sentiment_red$rollmean = zoo::rollmean(biden_sentiment_red$avg_sentiment,10, fill = NA)
biden_sentiment_red_syuzhet$rollmean = zoo::rollmean(biden_sentiment_red_syuzhet$avg_sentiment,10, fill = NA)

trump_sentiment_red$rollmean = zoo::rollmean(trump_sentiment_red$avg_sentiment,10, fill = NA)
trump_sentiment_red_syuzhet$rollmean = zoo::rollmean(trump_sentiment_red_syuzhet$avg_sentiment,10, fill = NA)



# comparison

trump_sentiment_twit['dataset'] <- "Trump Twitter" 
biden_sentiment_twit['dataset'] <- "Biden Twitter"

trump_sentiment_red['dataset'] <- "Trump Reddit" 
biden_sentiment_red['dataset'] <- "Biden Reddit"

merged_data <- rbind(trump_sentiment_twit, biden_sentiment_twit, trump_sentiment_red, biden_sentiment_red)

trump_sentiment_twit_syuzhet['dataset'] <- "Trump Twitter" 
biden_sentiment_twit_syuzhet['dataset'] <- "Biden Twitter"

trump_sentiment_red_syuzhet['dataset'] <- "Trump Reddit" 
biden_sentiment_red_syuzhet['dataset'] <- "Biden Reddit"

merged_data_syuzhet <- rbind(trump_sentiment_twit_syuzhet, biden_sentiment_twit_syuzhet, trump_sentiment_red_syuzhet, biden_sentiment_red_syuzhet)

### method = "bing"
ggplot(merged_data, aes(x = minute, y = avg_sentiment, color = dataset)) +
  geom_smooth() +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiment Analysis Reddit/Twitter") +
  scale_color_manual(values = c("Trump Reddit" = "red", "Biden Reddit" = "blue", "Biden Twitter" = "cadetblue1", "Trump Twitter" = "orange"))
ggsave('plots/both_smooth.png', last_plot(), device = "png")

### method = "syuzhet"
ggplot(merged_data_syuzhet, aes(x = minute, y = avg_sentiment, color = dataset)) +
  geom_smooth() +
  labs(x = "Time", y = "Average Sentiment", title = "Sentiment Analysis Reddit/Twitter Syuzhet") +
  scale_color_manual(values = c("Trump Reddit" = "red", "Biden Reddit" = "blue", "Biden Twitter" = "cadetblue1", "Trump Twitter" = "orange"))
ggsave('plots/both_syuzhet_smooth.png', last_plot(), device = "png")

