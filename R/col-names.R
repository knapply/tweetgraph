#' @importFrom data.table %chin%
status_col_names <- function(tweet_df, timestamp_col = "timestamp_ms") {
  col_templates <- c(
    "status_id",
    "created_at",
    "text", 
    "source",
    "lang"
    # "is_quote",
    # "is_retweet",
    # "favorite_count", 
    # "retweet_count",
    # "quote_count",
    # "reply_count", 
    # "place_url", 
    # "place_name", 
    # "place_full_name",
    # "place_type",
    # "country",
    # "country_code",
    # "geo_coords",
    # "bbox_coords",
    # "project_title",
    # "campaign_title"
  )
  
  with_context <- list(
    main = c(timestamp_col, col_templates),
    retweet = c(timestamp_col, paste0("retweet_", col_templates)),
    reply_to = c(timestamp_col, paste0("reply_to_", col_templates)),
    quoted = c(timestamp_col, paste0("quoted_", col_templates)),
    mentions = c(timestamp_col, paste0("mentions_", col_templates))
  )
  
  out <- lapply(with_context,
                function(.x) unique(.x[.x %chin% names(tweet_df)]))
  Filter(function(.x) length(.x) > 1L, out)
}


#' @importFrom data.table %chin%
user_col_names <- function(tweet_df, timestamp_col = "timestamp_ms") {
  col_templates <- c(
    "user_id",
    "name",
    "screen_name",
    "location",
    "description",
    "url",
    "protected",
    "followers_count",
    "friends_count",
    "listed_count",
    "statuses_count",
    "favourites_count",
    "account_created_at",
    "verified",
    "profile_url",
    # "profile_expanded_url",
    "account_lang"#,
    # "profile_banner_url",
    # "profile_background_url",
    # "profile_image_url"
  )
  
  with_context <- list(
    main = c(timestamp_col, col_templates),
    retweet = c(timestamp_col, paste0("retweet_", col_templates)),
    reply_to = c(timestamp_col, paste0("reply_to", col_templates)),
    quoted = c(timestamp_col, paste0("quoted_", col_templates)),
    mentions = c(timestamp_col, paste0("mentions_", col_templates))
  )
  
  out <- lapply(with_context, 
                function(.x) unique(.x[.x %chin% names(tweet_df)]))
  Filter(function(.x) length(.x) > 1L, out)
}

#' @importFrom stringi stri_replace_first_regex
standardize_cols <- function(df) {
  prototype_regex <- "^(retweet|reply_to|quoted|mentions)_"
  setnames(df, old = names(df), new =  stri_replace_first_regex(
    names(df), pattern = prototype_regex, replacement = ""
  )
  )[]
}



status_user_edge_cols <- function(tweet_df, .edge_type) {
  if (.edge_type == "info_flow") {
    status_user_cols <- list(
      # status to status, reversed
      c("retweet_status_id", "status_id", "created_at"), 
      c("reply_to_status_id", "status_id", "created_at"), 
      c("quoted_status_id", "status_id", "created_at"),
      # user to status
      c("user_id", "status_id", "created_at"),
      c("reply_to_user_id", "reply_to_status_id"),
      c("retweet_user_id", "retweet_status_id", "retweet_created_at"),
      c("quoted_user_id", "quoted_status_id", "quoted_created_at")
    )
  } else if (.edge_type == "interaction") {
    status_user_cols <- list(
      # # status to status
      c("status_id", "retweet_status_id", "created_at"),
      c("status_id", "reply_to_status_id", "created_at"),
      c("status_id", "quoted_status_id", "created_at"),
      # # status to user
      c("status_id", "user_id", "created_at"),
      c("reply_to_status_id", "reply_to_user_id"),
      c("retweet_status_id", "retweet_user_id", "retweet_created_at"),
      c("quoted_status_id", "quoted_user_id", "quoted_created_at")
    )
  } else {
    stop("Unknown `edge_type`: ", .edge_type)
  }
  
  .keep(status_user_cols, function(.x) all(.x %chin% names(tweet_df)))
}



