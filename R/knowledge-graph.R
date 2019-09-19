#' @importFrom data.table %chin%
decide_edge_action <- function(col_name, edge_type) {
  if (edge_type %chin% c("interaction", "entity")) {
    retweet_edge <- "retweet"
    reply_to_edge <- "reply_to"
    quoted_edge <- "quoted"
  } else if (edge_type == "info_flow") {
    retweet_edge <- "was_retweeted_by"
    reply_to_edge <- "was_replied_to_by"
    quoted_edge <- "was_quoted_by"
  } else {
    stop("Unknown `edge_type`: ", edge_type)
  }
  
  switch (col_name,
          status_id = ,
          user_id = "posts",
          
          retweet_status_id = ,
          retweet_user_id = retweet_edge,
          
          reply_to_status_id = ,
          reply_to_user_id = reply_to_edge,
          
          quoted_status_id = ,
          quoted_user_id = quoted_edge,
          
          hashtags = ,
          media_expanded_url = ,
          urls_expanded_url = "contains",
          
          mentions_user_id = "mentions",
          
          stop("Unknown `col_name`: ", col_name)
  )
}


decide_node_class <- function(col_name) {
  switch (col_name,
          status_id = ,
          retweet_status_id = ,
          reply_to_status_id = ,
          quoted_status_id = "status",
          
          user_id = ,
          retweet_user_id = ,
          reply_to_user_id = ,
          mentions_user_id = ,
          quoted_user_id = "user",
          
          hashtags = "hashtag",
          media_expanded_url = "media",
          urls_expanded_url = "url",
          
          stop("Unknown `col_name`: ", col_name)
  )
}


#' @importFrom data.table setnames
set_edge_col_names <- function(edge_df, columns) {
  new_col_names <- c(
    c("source", "target", "time")[seq_along(columns)],
    "source_class", "action", "target_class"
  )
  setnames(edge_df, new_col_names)[]
}


#' @importFrom data.table :=
#' @importFrom stats na.omit
.get_status_user_edges <- function(columns, tweet_df, edge_direction) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # ..columns <- NULL
  #
  
  edge_action_col <- switch (edge_direction,
                             interaction = columns[[2L]],
                             info_flow = columns[[1L]],
                             stop("Unknown `edge_type`: ", edge_direction)
  )
  
  out <- na.omit(tweet_df, cols = columns
                 )[, ..columns
                   ][, c("source_class", "action", "target_class") := list(
                     decide_node_class(columns[[1L]]),
                     decide_edge_action(edge_action_col, edge_direction),
                     decide_node_class(columns[[2L]])
                     )]
  
  set_edge_col_names(out, columns)
}


#' @importFrom data.table rbindlist
get_status_user_edges <- function(tweet_df, edge_direction) {
  target_cols <- status_user_edge_cols(tweet_df, edge_direction)
  
  df_list <- lapply(target_cols, .get_status_user_edges, 
                    tweet_df = tweet_df, 
                    edge_direction = edge_direction)
  
  rbindlist(df_list, use.names = TRUE, fill = TRUE)
}


#' @importFrom data.table :=
#' @importFrom stats na.omit
.get_entity_edges <- function(columns, tweet_df) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # ..columns <- NULL
  #
  
  out <- na.omit(tweet_df, cols = columns
                 )[, ..columns
                   ][, c("source_class", "action", "target_class") := list(
                     decide_node_class(columns[[1L]]),
                     decide_edge_action(columns[[2L]], edge_type = "entity"),
                     decide_node_class(columns[[2L]])
                     )]
  set_edge_col_names(out, columns)
}



#' @importFrom data.table rbindlist setcolorder
get_entity_edges <- function(tweet_df) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # target <- NULL
  # . <- NULL
  #
  target_cols <- entity_edge_cols(tweet_df)
  
  init <- rbindlist(
    lapply(target_cols, .get_entity_edges, tweet_df),
    use.names = TRUE, fill = TRUE
  )
  
  out <- init[, .(target = unlist_na_rm(target)),
              by = c("source", "source_class", 
                     "action", "target_class", "time")]
  
  setcolorder(
    out, c("source", "target", "time", "source_class", "action", "target_class")
  )
}



#' @importFrom igraph induced_subgraph neighborhood V V<-
extract_ego <- function(tweet_graph, node_name, .order = 3L) {
  target_nodes <- which(V(tweet_graph)$name %chin% node_name)
  
  hood <- neighborhood(graph = tweet_graph,
                       order = .order,
                       nodes = target_nodes)
  
  out <- induced_subgraph(graph = tweet_graph,
                          vids = unlist(hood, use.names = FALSE))
  V(out)$title <- V(out)$name
  
  attr(out, "edge_type") <- attr(tweet_graph, "edge_type")
  
  out
}






#' Build a knowledge graph `<tweetgraph_primitive>`.
#' 
#' @template param-tweet_df
#' @param edge_direction
#' * `character(1L)`
#'   + `"info_flow"` or `"interaction"`
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
#' as_kg_primitive(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @importFrom data.table := %chin% as.data.table is.data.table 
#' @importFrom data.table setcolorder setnames setorder
#' 
#' @export
as_kg_primitive <- function(tweet_df, edge_direction = c("info_flow",
                                                         "interaction")) {
  if (!is.data.table(tweet_df)) {
    tweet_df <- as.data.table(tweet_df)
  }
  if (!"timestamp_ms" %chin% names(tweet_df)) {
    message("`timestamp_ms` column is missing. Setting column to `Sys.time()`.")
    tweet_df[, timestamp_ms := Sys.time()]
    
    HAS_VALID_TIMESTAMP <- FALSE
  } else {
    HAS_VALID_TIMESTAMP <- TRUE
  }
  
  edge_direction <- match.arg(edge_direction, c("info_flow", "interaction"))

  status_user_edges <- get_status_user_edges(tweet_df, edge_direction)
  
  entity_edges <- get_entity_edges(tweet_df
                                   )[, target := tolower(target)
                                     ]
  
  edge_df <- rbindlist(
    lapply(list(status_user_edges, entity_edges),
           unique),
    use.names = TRUE
    )[, time := as.double(time) # igraph mangles POSIXct
      ]
  
  names_in_edge_df <- unique(c(edge_df$source, edge_df$target))
  
  status_attrs <- extract_all_statuses(tweet_df
                                       )[status_id %chin% names_in_edge_df]
  status_attrs[, node_class := "status"]
  setnames(status_attrs, old = "status_id", new = "name")
  
  user_attrs <- extract_all_users(tweet_df
                                  )[user_id %chin% names_in_edge_df
                                    ][, node_class := "user"
                                      ]
  setnames(user_attrs, 
           old = c("name", "user_id"), 
           new = c("TWITTER_NAME", "name"))
  
  entity_attrs <- entity_edges[target_class %chin% c("hashtag", "media", "url"),
                               .(name = target, node_class = target_class)]
  
  all_attrs <- rbindlist(
    list(status_attrs, user_attrs, entity_attrs),
    use.names = TRUE, fill = TRUE
  )
  
  missing_nodes <- names_in_edge_df[!names_in_edge_df %chin% all_attrs$names]
  if (length(missing_nodes) != 0L) {
    missing_attrs <- rbindlist(
      list(edge_df[source %chin% missing_nodes, 
                   .(name = source, node_class = source_class)],
           edge_df[target %chin% missing_nodes,
                   .(name = target, node_class = target_class)]
      )
    )
    
    all_attrs <- rbindlist(list(all_attrs, missing_attrs),
                           use.names = TRUE, fill = TRUE)
  }
  
  all_attrs <- unique(all_attrs, by = "name")
  dttm_cols <- names(all_attrs)[.map_lgl(all_attrs, inherits, "POSIXct")]
  all_attrs[, (dttm_cols) := lapply(.SD, as.double),
            .SDcols = dttm_cols]
  setcolorder(all_attrs, c("name", setdiff(names(all_attrs), "name")))
  
  structure(
    list(edges = edge_df, nodes = all_attrs),
    class = "tweetgraph_primitive",
    edge_type = edge_direction
  )
}
