#' Build an `<igraph>` for social network analysis from a
#' `<tweetgraph_primitive>` or tweet `<data.frame>`.
#' 
#' @param x `<tweetgraph_primitive>` or tweet `<data.frame>`
#' @template param-dots
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
#' as_sna_igraph(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @export
as_sna_igraph <- function(x, ...) {
  UseMethod("as_sna_igraph")
}

#' @rdname as_sna_igraph
#' @export
as_sna_igraph.data.frame <- function(x, ...) {
  req_missing_cols <- setdiff(c("user_id", "status_id"), names(x))
  if (!.is_empty(req_missing_cols)) {
    stop("`x` has an unrecognized strucuture. It's missing the following
         required columns:", paste0('\n\t-"', req_missing_cols, '"'))
  }
  
  socnet_prim <- as_sna_primitive(tweet_df = x, ...)
  
  as_sna_igraph(socnet_prim)
}

#' @rdname as_sna_igraph
#' @importFrom igraph graph_from_data_frame
#' @export
as_sna_igraph.tweetgraph_primitive <- function(x, ...) {
  graph_from_data_frame(d = x$edges, directed = TRUE,
                        vertices = x$nodes)
}



#' Build a knowledge `<igraph>` from a `<tweetgraph_primitive>` or tweet 
#' `<data.frame>`.
#' 
#' @param x `<tweetgraph_primitive>` or tweet `<data.frame>`
#' @template param-dots
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
#' as_kg_igraph(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @export
as_kg_igraph <- function(x, ...) {
  UseMethod("as_kg_igraph")
}

#' @rdname as_kg_igraph
#' @export
as_kg_igraph.data.frame <- function(x, ...) {
  req_missing_cols <- setdiff(c("user_id", "status_id"), names(x))
  if (!.is_empty(req_missing_cols)) {
    stop("`x` has an unrecognized strucuture. It's missing the following
         required columns:", paste0('\n\t-"', req_missing_cols, '"'))
  }
  
  kg_prim <- as_kg_primitive(tweet_df = x, ...)
  
  as_kg_igraph(kg_prim)
}

#' @rdname as_kg_igraph
#' @importFrom igraph graph_from_data_frame
#' @export
as_kg_igraph.tweetgraph_primitive <- function(x, ...) {
  graph_from_data_frame(d = x$edges, directed = TRUE,
                        vertices = x$nodes)
}

