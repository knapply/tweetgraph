#' Build an `<igraph>` for social network analysis from a
#' `<tweetgraph_primitive>` or tweet `<data.frame>`.
#' 
#' @param x `<tweetgraph_primitive>` or tweet `<data.frame>`
#' @param action `<character>`
#' * `"all"`, `"mentions"`, `"retweet"`, `"quoted"`, and/or `"reply_to"`
#'
#' @return `<igraph>`
#' 
#' @template author-bk
#' 
#' @examples 
#' \dontrun{
#' 
#' 
#' hashtag_rstats <- rtweet::search_tweets("#rstats")
#' 
#' as_socnet_igraph(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @export
as_socnet_igraph <- function(x, ...) {
  UseMethod("as_socnet_igraph")
}

#' @rdname as_socnet_igraph
#' @export
as_socnet_igraph.data.frame <- function(x, action = c("all", "mentions", 
                                                      "retweet", "quoted",
                                                      "reply_to")) {
  req_missing_cols <- setdiff(c("user_id", "status_id"), names(x))
  if (!.is_empty(req_missing_cols)) {
    stop("`x` has an unrecognized strucuture. It's missing the following
         required columns:", paste0('\n\t-"', req_missing_cols, '"'))
  }
  
  socnet_prim <- as_socnet_primitive(tweet_df = x, action = action)
  
  as_socnet_igraph(socnet_prim)
}

#' @rdname as_socnet_igraph
#' @importFrom igraph graph_from_data_frame
#' @export
as_socnet_igraph.tweetgraph_primitive <- function(x, ...) {
  graph_from_data_frame(d = x$edges, directed = TRUE,
                        vertices = x$nodes)
}



#' Build a knowledge `<igraph>` from a `<tweetgraph_primitive>` or tweet 
#' `<data.frame>`.
#' 
#' @param x `<tweetgraph_primitive>` or tweet `<data.frame>`
#' @param .edge_type `<character>`
#' * One of `"info_flow"` or `"interaction"`
#' @param ... Arguments passed to or from other methods.
#'
#' @return `<igraph>`
#' 
#' @template author-bk
#' 
#' @examples 
#' \dontrun{
#' 
#' 
#' hashtag_rstats <- rtweet::search_tweets("#rstats")
#' 
#' as_knowledge_igraph(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @export
as_knowledge_igraph <- function(x, ...) {
  UseMethod("as_knowledge_igraph")
}

#' @rdname as_knowledge_igraph
#' @export
as_knowledge_igraph.data.frame <- function(x, ...) {
  req_missing_cols <- setdiff(c("user_id", "status_id"), names(x))
  if (!.is_empty(req_missing_cols)) {
    stop("`x` has an unrecognized strucuture. It's missing the following
         required columns:", paste0('\n\t-"', req_missing_cols, '"'))
  }
  
  kg_prim <- as_knowledge_graph_primitive(tweet_df = x, ...)
  
  as_knowledge_igraph(kg_prim)
}

#' @rdname as_knowledge_igraph
#' @importFrom igraph graph_from_data_frame
#' @export
as_knowledge_igraph.tweetgraph_primitive <- function(x, ...) {
  graph_from_data_frame(d = x$edges, directed = TRUE,
                        vertices = x$nodes)
}

