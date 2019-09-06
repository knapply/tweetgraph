test_that("extract_all_users() works", {
  users <- extract_all_users(tweetgraph:::knapply_tweets)
  
  expect_equal(
    dim(users),
    c(105, 17)
  )
  
  expect_equal(
    names(users),
    c("user_id", "timestamp_ms", "name", "screen_name", "location", 
      "description", "url", "protected", "followers_count", "friends_count", 
      "listed_count", "statuses_count", "favourites_count", "account_created_at", 
      "verified", "profile_url", "account_lang")
  )
  
  expect_equal(
    .map_chr(users, typeof),
    c(user_id = "character", timestamp_ms = "double", name = "character", 
      screen_name = "character", location = "character", description = "character", 
      url = "logical", protected = "logical", followers_count = "integer", 
      friends_count = "integer", listed_count = "integer", statuses_count = "integer", 
      favourites_count = "integer", account_created_at = "double", 
      verified = "logical", profile_url = "character", account_lang = "logical"
    )
  )  
})

test_that("extract_all_statuses() works", {
  statuses <- extract_all_statuses(tweetgraph:::knapply_tweets)
  
  expect_equal(
    dim(statuses),
    c(190, 6)
  )
  
  expect_equal(
    names(statuses),
    c("timestamp_ms", "status_id", "created_at", "text", "source", "lang")
  )
  
  expect_equal(
    .map_chr(statuses, typeof),
    c(timestamp_ms = "double", status_id = "character", created_at = "double", 
      text = "character", source = "character", lang = "character")
  )  
})


test_that("as_knowledge_graph() works", {
  hashtag_g <- as_knowledge_graph(hashtag_rstats)
  
  expect_equal(
    igraph::vcount(hashtag_g),
    329
  )
  
  expect_equal(
    igraph::ecount(hashtag_g),
    812
  )
  
  expect_equal(
    igraph::vertex_attr_names(hashtag_g),
    c("timestamp_ms", "name", "created_at", "text", "source", "lang", 
      "node_class", "TWITTER_NAME", "screen_name", "location", "description", 
      "url", "protected", "followers_count", "friends_count", "listed_count", 
      "statuses_count", "favourites_count", "account_created_at", "verified", 
      "profile_url", "account_lang")
  )
  
  expect_equal(
    igraph::edge_attr_names(hashtag_g),
    c("time", "source_class", "action", "target_class")
  )
})
