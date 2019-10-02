#' @importFrom data.table %chin% rbindlist
extract_all_users <- function(tweet_df) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # row_id <- NULL
  # user_id <- NULL
  # timestamp_ms <- NULL
  # ..x <- NULL
  # .I <- NULL
  # .SD <- NULL
  #
  
  user_cols <- user_col_names(tweet_df)
  
  user_dfs <- lapply(user_cols, function(x) {
    standardize_cols(
      tweet_df[, ..x]
    )[!is.na(user_id)]
  })
  
  if ("mentions" %chin% names(user_dfs)) {
    user_dfs$mentions[, row_id := .I]
    user_dfs$mentions <- suppressWarnings(
      rbindlist(
        lapply(split(user_dfs$mentions, by = "row_id"),
               unlist, recursive = FALSE)
      )
    )
    user_dfs$mentions[, timestamp_ms := as.POSIXct(
      timestamp_ms, 
      origin = structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC")
      )
      ][, row_id := NULL
        ]
  }
  
  out <- rbindlist(user_dfs, use.names = TRUE, fill = TRUE)
  
  out[, lapply(.SD, function(.x) .x[which.min(is.na(.x))]),
      by = user_id]
}


#' @importFrom data.table rbindlist
extract_all_statuses <- function(tweet_df) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # status_id <- NULL
  # ..x <- NULL
  #
  
  status_cols <- status_col_names(tweet_df)
  
  status_dfs <- lapply(status_cols, function(x) {
    standardize_cols(
      tweet_df[, ..x]
    )[!is.na(status_id)]
  })
  
  out <- rbindlist(status_dfs, use.names = TRUE, fill = TRUE)
  
  unique(out, by = "status_id")
}