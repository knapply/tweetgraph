#' Build a social network `<tweetgraph_primitive>`.
#' 
#' @template param-tweet_df
#' @param action `<character>`
#' * `"all"`, `"mentions"`, `"retweet"`, `"quoted"`, and/or `"reply_to"`
#'
#' @return `<tweetgraph_primitive>`
#' 
#' @template author-bk
#' 
#' @examples 
#' \dontrun{
#' 
#' 
#' hashtag_rstats <- rtweet::search_tweets("#rstats")
#' 
#' as_social_network_primitive(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @importFrom data.table := %chin% as.data.table data.table is.data.table melt
#' @importFrom data.table setnames setcolorder setorder
#' 
#' @export
as_socnet_primitive <- function(tweet_df, action = c("all", "mentions",
                                                     "retweet","quoted", 
                                                     "reply_to")) {
  if (!is.data.table(tweet_df)) {
    tweet_df <- as.data.table(tweet_df)
  }
  if (!"timestamp_ms" %chin% names(tweet_df)) {
    message("`timestamp_ms` column is missing. Setting column to `Sys.time()`.")
    tweet_df[, timestamp_ms := Sys.time()]
  }
  
  action <- match.arg(action, 
                      c("all", "mentions", "retweet", "quoted", "reply_to"),
                      several.ok = TRUE)
  if ("all" %chin% action) {
    action <- c("mentions", "retweet", "quoted", "reply_to")
  }
  targets <- paste0(action, "_user_id")
  
  cols_to_keep <- unique(c("user_id", "status_id", targets))
  
  init <- tweet_df[, ..cols_to_keep]
  atomic_targets <- setdiff(names(init)[.map_lgl(init, is.atomic)],
                            c("user_id", "status_id"))
  init[, (atomic_targets) := lapply(.SD, as.list),
       .SDcols = atomic_targets]
  
  edge_df <- melt(init, id.vars = c("user_id", "status_id"), 
                  variable.name = "action", value.name = "to")
  
  edge_df <- edge_df[, .(to = unlist(to, use.names = FALSE)),
      by = setdiff(names(edge_df), "to")
      ][!is.na(to)
        ][, action := sub("_user_id$", "", action)]
  setnames(edge_df, old = "user_id", new = "from")
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
  
  init_node_names <- unique(c(edge_df[["from"]], edge_df[["to"]]))
  missing_nodes <- init_node_names[!init_node_names %chin% users[["name"]]]
  if (length(missing_nodes) != 0L) {
    users <- rbindlist(
      list(users, data.table(name = missing_nodes)),
      fill = TRUE
    )
  }
  
  structure(
    list(edges = edge_df, nodes = users),
    class = "tweetgraph_primitive",
    actions = unique(edge_df$action)
  )
}
















