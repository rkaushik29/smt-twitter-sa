biden_dataset = read.csv("resources/joebiden.csv")  # path to joebiden dataset
trump_dataset = read.csv("resources/donaldtrump.csv") # path to donaldtrump dataset

# Removing rows with garbage values
biden_dataset = biden_dataset[!is.na(as.POSIXct(as.character(biden_dataset$created_at), format = "%Y-%m-%d %H:%M:%S")),]
trump_dataset = trump_dataset[!is.na(as.POSIXct(as.character(trump_dataset$created_at), format = "%Y-%m-%d %H:%M:%S")),]

# Duplicate removal
biden_dataset = biden_dataset %>% distinct(tweet_id, .keep_all = TRUE)
trump_dataset = trump_dataset %>% distinct(tweet_id, .keep_all = TRUE)

# Write back to dataset
write.csv(data_biden, file = "filtered_biden.csv", row.names = FALSE)
write.csv(data_trump, file = "filtered_trump.csv", row.names = FALSE)
