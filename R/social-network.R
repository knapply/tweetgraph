#' Build a graph ready for proper social network analysis.
#' 
#' @template param-tweet_df
#'
#' @return `igraph`
#' 
#' @template author-bk
#' 
#' @examples 
#' \dontrun{
#' 
#' 
#' hashtag_rstats <- rtweet::search_tweets("#rstats")
#' 
#' tweet_graph <- as_social_network(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @importFrom data.table := %chin% as.data.table data.table is.data.table melt setnames setcolorder setorder
#' @importFrom igraph graph_from_data_frame V vertex_attr vertex.attributes<-
#' 
#' @export
as_social_network <- function(tweet_df) {
  if (!is.data.table(tweet_df)) {
    tweet_df <- as.data.table(tweet_df)
  }
  if (!"timestamp_ms" %chin% names(tweet_df)) {
    warning("`timestamp_ms` column is missing. Setting column to `Sys.time()`.",
            call. = FALSE)
    tweet_df[, timestamp_ms := Sys.time()]
  }
  
  targets <- intersect(
    names(tweet_df), c("mentions_user_id", "quoted_user_id", 
                       "reply_to_user_id", "retweet_user_id")
  )
  
  col_to_keep <- c("user_id", "status_id", targets)
  
  init <- tweet_df[
    , ..col_to_keep
    ][, lapply(.SD, unlist, use.names = FALSE), 
      .SDcols = targets,
      by = .(user_id, status_id)
      ]
  setnames(init, old = "user_id", new = "from")
  
  edge_df <- melt(
    init, id.vars = c("from", "status_id"), 
    variable.name = "action", value.name = "to",
    variable.factor = FALSE
    )[!is.na(to)
      ]
  setcolorder(edge_df, neworder = c("from", "to", "action", "status_id"))
  
  users <- extract_all_users(
    tweet_df
    )[user_id %chin% unique(c(edge_df$from, edge_df$to))
      ][, c("timestamp_ms", "account_created_at") := lapply(.SD, as.double),
        .SDcols = c("timestamp_ms", "account_created_at")
        ][, node_class := "user"]
  setnames(users,
           old = c("name", "user_id"), 
           new = c("TWITTER_NAME", "name"))
  
  statuses <- extract_all_statuses(
    tweet_df
    )[status_id %chin% edge_df$status_id
      ][, created_at := NULL
        ]
  
  edge_df <- edge_df[statuses, on = "status_id"
                     ][, timestamp_ms := NULL
                       ]
  
  out <- graph_from_data_frame(d = edge_df)
  
  missing_nodes <- V(out)[!vertex_attr(out, "name") %chin% users$name]
  if (length(missing_nodes) != 0L) {
    users <- rbindlist(
      list(users, data.table(name = missing_nodes$name)),
      fill = TRUE
    )
  }
  
  users[, node_order := match(users$name, vertex_attr(out, "name"))]
  setorder(users, node_order)
  users[, node_order := NULL]
  
  vertex.attributes(out) <- as.list(users)
  
  out
}
