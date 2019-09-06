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
    mention_cols <- intersect(c("user_id", "screen_name"), 
                              names(user_dfs$mentions))
    user_dfs$mentions[, row_id := .I]
    user_dfs$mentions <- rbindlist(
      lapply(split(user_dfs$mentions, by = "row_id"),
             unlist, recursive = FALSE)
    )
    user_dfs$mentions[, timestamp_ms := as.POSIXct(
      timestamp_ms, 
      origin = structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )][, row_id := NULL]
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
  
  status_dfs <- lapply(status_cols, function(x){
    standardize_cols(
      tweet_df[, ..x]
    )[!is.na(status_id)]
  })
  
  out <- rbindlist(status_dfs, use.names = TRUE, fill = TRUE)
  
  unique(out, by = "status_id")
}


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
.get_status_user_edges <- function(columns, .tweet_df, edge_type) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # ..columns <- NULL
  #
  
  edge_action_col <- switch (edge_type,
                             interaction = columns[[2L]],
                             info_flow = columns[[1L]],
                             stop("Unknown `edge_type`: ", edge_type)
  )
  
  out <- na.omit(.tweet_df, cols = columns
                 )[, ..columns
                   ][, c("source_class", "action", "target_class") := list(
                     decide_node_class(columns[[1L]]),
                     decide_edge_action(edge_action_col, edge_type),
                     decide_node_class(columns[[2L]])
                     )]
  
  set_edge_col_names(out, columns)
}


#' @importFrom data.table rbindlist
get_status_user_edges <- function(.tweet_df, .col_list, edge_type) {
  df_list <- lapply(.col_list, .get_status_user_edges, .tweet_df, 
                    edge_type = edge_type)
  rbindlist(df_list, use.names = TRUE, fill = TRUE)
}


#' @importFrom data.table :=
#' @importFrom stats na.omit
.get_entity_edges <- function(columns, .tweet_df) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # ..columns <- NULL
  #
  
  out <- na.omit(.tweet_df, cols = columns
                 )[, ..columns
                   ][, c("source_class", "action", "target_class") := list(
                     decide_node_class(columns[[1L]]),
                     decide_edge_action(columns[[2L]], edge_type = "entity"),
                     decide_node_class(columns[[2L]])
                     )]
  set_edge_col_names(out, columns)
}

unlist_na_rm <- function(x) {
  init <- unlist(x, use.names = FALSE)
  init[!is.na(init)]
}

#' @importFrom data.table rbindlist setcolorder
get_entity_edges <- function(.tweet_df, .col_list) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # target <- NULL
  # . <- NULL
  #
  
  init <- rbindlist(
    lapply(.col_list, .get_entity_edges, .tweet_df),
    use.names = TRUE, fill = TRUE
  )
  out <- init[, .(target = unlist_na_rm(target)),
              by = c("source", "source_class", 
                     "action", "target_class", "time")]
  
  setcolorder(
    out, c("source", "target", "time", "source_class", "action", "target_class")
  )
}

#' @importFrom data.table := %chin% as.data.table is.data.table setnames setorder
#' @importFrom igraph as_ids graph_from_data_frame V vertex.attributes<- edge.attributes<-
#' @importFrom stringi stri_trans_tolower
as_knowledge_graph <- function(tweet_df,
                               .edge_type = c("info_flow", "interaction")) {
  # handle R CMD check NOTES about NSE {data.table} vars
  # source <- NULL
  # source_class <- NULL
  # target <- NULL
  # target_class <- NULL
  # time <- NULL
  # status_id <- NULL
  # node_class <- NULL
  # user_id <- NULL
  # timestamp_ms <- NULL
  # node_order <- NULL
  # . <- NULL
  # ..columns <- NULL
  # .I <- NULL
  # .SD <- NULL
  #
  
  if (!is.data.table(tweet_df)) {
    tweet_df <- as.data.table(tweet_df)
  }
  if (!"timestamp_ms" %chin% names(tweet_df)) {
    warning("`timestamp_ms` column is missing. Setting column to `Sys.time()`.",
            call. = FALSE)
    tweet_df[, timestamp_ms := Sys.time()]
  }
  
  .edge_type <- match.arg(.edge_type, c("info_flow", "interaction"))
  
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
  
  good_status_user_cols <- vapply(status_user_cols, 
                                  function(.x) all(.x %chin% names(tweet_df)),
                                  FUN.VALUE = logical(1L))
  status_user_cols <- status_user_cols[good_status_user_cols]
  
  status_user_edges <- get_status_user_edges(
    .tweet_df = tweet_df,
    .col_list = status_user_cols,
    edge_type = .edge_type
  )
  
  
  entity_cols <- list(
    # status to hashtag
    c("status_id", "hashtags", "created_at"),
    c("retweet_status_id", "hashtags", "retweet_created_at"),
    c("quoted_status_id", "hashtags", "quoted_created_at"),
    # status to media
    c("status_id", "media_expanded_url", "created_at"),
    c("retweet_status_id", "media_expanded_url", "retweet_created_at"),
    c("quoted_status_id", "media_expanded_url", "quoted_created_at"),
    # status to url
    c("status_id", "urls_expanded_url", "created_at"),
    c("retweet_status_id", "urls_expanded_url", "retweet_created_at"),
    c("quoted_status_id", "urls_expanded_url", "quoted_created_at"),
    # status to mention
    c("status_id", "mentions_user_id", "created_at"),
    c("retweet_status_id", "mentions_user_id", "retweet_created_at"),
    c("quoted_status_id", "mentions_user_id", "quoted_created_at")
  )
  good_entity_cols <- vapply(entity_cols, 
                             function(.x) all(.x %chin% names(tweet_df)),
                             FUN.VALUE = logical(1L))
  entity_cols <- entity_cols[good_entity_cols]
  
  entity_edges <- get_entity_edges(
    .tweet_df = tweet_df,
    .col_list = entity_cols
  )[, target := stri_trans_tolower(target)]
  
  edges <- rbindlist(lapply(list(status_user_edges, entity_edges), 
                            unique),
                     use.names = TRUE)
  edges[, time := as.double(time)]                      # igraph mangles POSIXct
  
  g <- graph_from_data_frame(d = edges)
  
  status_attrs <- extract_all_statuses(tweet_df)[status_id %chin% V(g)$name]
  status_attrs[, node_class := "status"]
  setnames(status_attrs, old = "status_id", new = "name")
  
  user_attrs <- extract_all_users(tweet_df)[user_id %chin% V(g)$name]
  user_attrs[, node_class := "user"]
  setnames(user_attrs, 
           old = c("name", "user_id"), 
           new = c("TWITTER_NAME", "name"))
  
  entity_attrs <- entity_edges[target_class %chin% c("hashtag", "media", "url"),
                               .(name = target, node_class = target_class)]
  
  all_attrs <- rbindlist(
    list(status_attrs, user_attrs, entity_attrs),
    use.names = TRUE, fill = TRUE
  )
  
  missing_nodes <- V(g)[!V(g)$name %chin% all_attrs$name]
  if (length(missing_nodes) != 0L) {
    missing_attrs <- rbindlist(
      list(edges[source %chin% missing_nodes$name, 
                 .(name = source, node_class = source_class)],
           edges[target %chin% missing_nodes$name,
                 .(name = target, node_class = target_class)]
      )
    )
    
    all_attrs <- rbindlist(list(all_attrs, missing_attrs),
                           use.names = TRUE, fill = TRUE)
  }
  
  all_attrs <- unique(all_attrs, by = "name")
  
  all_attrs[, node_order := match(all_attrs$name, V(g)$name)]
  
  setorder(all_attrs, node_order)
  all_attrs[, node_order := NULL]
  
  vertex.attributes(g) <- as.list(all_attrs)
  
  
  attr(g, "edge_type") <- .edge_type
  
  g
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


