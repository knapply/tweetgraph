knapply_tweets <- rtweet::get_timeline("knapply_")
knapply_tweets$timestamp_ms <- Sys.time()

hashtag_rstats <- rtweet::search_tweets("#rstats")
hashtag_rstats$timestamp_ms <- Sys.time()

usethis::use_data(knapply_tweets, hashtag_rstats, internal = TRUE)
