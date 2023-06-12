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