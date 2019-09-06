knapply_tweets <- rtweet::get_timeline("knapply_")
knapply_tweets$timestamp_ms <- Sys.time()
knapply_tweets <- data.table::as.data.table(knapply_tweets)

hashtag_rstats <- rtweet::search_tweets("#rstats")
hashtag_rstats$timestamp_ms <- Sys.time()

usethis::use_data(knapply_tweets, hashtag_rstats, internal = TRUE, overwrite=T)
