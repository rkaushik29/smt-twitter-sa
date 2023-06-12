library("syuzhet")
library("ggplot2")

biden_dataset = read.csv("resources/filtered_biden.csv")  # path to joebiden dataset
trump_dataset = read.csv("resources/filtered_trump.csv") # path to donaldtrump dataset

biden_dataset = subset(biden_dataset, select = colnames(data_biden))
trump_dataset = subset(trump_dataset, select = colnames(data_trump))

total_unique = rbind(biden_dataset, trump_dataset)
total_unique = total_unique %>% distinct(tweet_id, .keep_all = TRUE)

biden_only = biden_dataset[!(biden_dataset$tweet_id %in% trump_dataset$tweet_id),]
biden_only$candidate = "biden"

trump_only = trump_dataset[!(trump_dataset$tweet_id %in% biden_dataset$tweet_id),]
trump_only$candidate = "trump"

both_only = semi_join(biden_dataset, trump_dataset, by = "tweet_id")
both_only$candidate = "both"

separated_data = rbind(biden_only, trump_only, both_only)

separated_data$candidate <- factor(separated_data$candidate, levels = c("trump", "biden", "both"), ordered = TRUE)

g = ggplot(data = separated_data, mapping = aes(x = candidate, fill = candidate)) + 
  geom_bar(mapping = aes(y = (..count..)/sum(..count..))) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Overlap Between #Biden and #Trump Data Sets") +
  ylab(label = "percentage of tweets")

print(g)


#####################################################################################################################################

# Sentiment over time
sample_idx = sample(nrow(separated_data), 1*nrow(separated_data))
sample_data = separated_data[sample_idx,]

tweet_sentiment = function(text)
{
  sentences = syuzhet::get_sentences(text)
  sentiments = syuzhet::get_sentiment(sentences, method = "syuzhet")
  mean(sentiments)
}

sample_data$sentiment = lapply(sample_data$tweet, tweet_sentiment)

sample_data$sentiment = as.numeric(sample_data$sentiment)

tmp_biden = sample_data[sample_data$candidate == "biden",]
tmp_trump = sample_data[sample_data$candidate == "trump",]
tmp_both = sample_data[sample_data$candidate == "both",]

tmp_biden = tmp_biden[order(tmp_biden$created_at),]
tmp_trump = tmp_trump[order(tmp_trump$created_at),]
tmp_both = tmp_both[order(tmp_both$created_at),]

avg_tph = function(df)
{
  temp_df = df
  temp_df$created_at_hour = as.POSIXct(temp_df$created_at, format = "%Y-%m-%d %H")
  temp_split = split(temp_df, temp_df$created_at_hour)
  
  mean(do.call(rbind, lapply(temp_split, function(x) nrow(x))))
}

biden_tph = avg_tph(tmp_biden)
both_tph = avg_tph(tmp_both)
trump_tph = avg_tph(tmp_trump)

tmp_biden$rollmean = zoo::rollmean(tmp_biden$sentiment, k=8*biden_tph, fill = NA)
tmp_trump$rollmean = zoo::rollmean(tmp_trump$sentiment, k=8*trump_tph, fill = NA)
tmp_both$rollmean = zoo::rollmean(tmp_both$sentiment, k=8*both_tph, fill = NA)

sample_data = rbind(tmp_biden, tmp_trump, tmp_both)

date_ranges = data.frame(
  from = as.Date(c("2020-11-03")),
  to = as.Date(c("2020-11-04"))
)

hour_ranges = data.frame(
  from = as.POSIXct(c("2020-11-03 00:00:00")),
  to = as.POSIXct(c("2020-11-04 00:00:00"))
)

g = ggplot() +
  geom_line(data = sample_data, mapping = aes(x = as.POSIXct(created_at, format = "%Y-%m-%d %H:%M:%S"), y = rollmean, color = candidate), size = 0.6) +
  geom_rect(data = hour_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Sentiment of Tweets Near Election Day") +
  ylab(label = "sentiment (rolling mean)") +
  xlab(label = "date")

print(g)

gs = ggplot() +
  geom_smooth(data = sample_data, mapping = aes(x = as.POSIXct(created_at, format = "%Y-%m-%d %H:%M:%S"), y = sentiment, color = candidate)) +
  geom_rect(data = hour_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Sentiment of Tweets Near Election Day") +
  ylab(label = "sentiment (smoothed)") +
  xlab(label = "date")

print(gs)

tmp_biden$rollmean = zoo::rollmean(tmp_biden$sentiment, k=6*biden_tph, fill = NA)
tmp_trump$rollmean = zoo::rollmean(tmp_trump$sentiment, k=6*trump_tph, fill = NA)
tmp_both$rollmean = zoo::rollmean(tmp_both$sentiment, k=6*both_tph, fill = NA)

sample_data_range = rbind(tmp_biden, tmp_trump, tmp_both)
sample_data_range = sample_data_range[sample_data_range$created_at >= "2020-11-01 00:00:00" & sample_data_range$created_at < "2020-11-06 00:00:00",]

g = ggplot() +
  geom_line(data = sample_data_range, mapping = aes(x = as.POSIXct(created_at, format = "%Y-%m-%d %H:%M:%S"), y = rollmean, color = candidate), size = 0.6) +
  geom_rect(data = hour_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Sentiment of Tweets Near Election Day") +
  ylab(label = "sentiment (rolling mean)") +
  xlab(label = "date")

print(g)

gs = ggplot() +
  geom_smooth(data = sample_data_range, mapping = aes(x = as.POSIXct(created_at, format = "%Y-%m-%d %H:%M:%S"), y = sentiment, color = candidate)) +
  geom_rect(data = hour_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Sentiment of Tweets Near Election Day") +
  ylab(label = "sentiment (smoothed)") +
  xlab(label = "date")

print(gs)
