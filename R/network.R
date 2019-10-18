#' Build a knowledge `<network>` from a `<tweetgraph_primitive>` or tweet 
#' `<data.frame>`.
#' 
#' @param x `<tweetgraph_primitive>` or tweet `<data.frame>`
#' @param ... Arguments passed to or from other methods.
#'
#' @return `<network>`
#' 
#' @template author-bk
#' 
#' @examples 
#' \dontrun{
#' 
#' 
#' hashtag_rstats <- rtweet::search_tweets("#rstats")
#' 
#' as_kg_network(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @export
as_kg_network <- function(x, ...) {
  UseMethod("as_kg_network")
}

#' @rdname as_kg_network
#' @export
as_kg_network.data.frame <- function(x, ...) {
  req_missing_cols <- setdiff(c("user_id", "status_id"), names(x))
  if (!.is_empty(req_missing_cols)) {
    stop("`x` has an unrecognized strucuture. It's missing the following
         required columns:", paste0('\n\t-"', req_missing_cols, '"'))
  }
  
  kg_prim <- as_kg_primitive(tweet_df = x, ...)
  
  as_kg_network(kg_prim)
}

#' @rdname as_kg_network
#' @importFrom igraph graph_from_data_frame
#' @export
as_kg_network.tweetgraph_primitive <- function(x, ...) {
  .network_from_tweetgraph_primitive(x)
}



#' Build a `<network>` a for social network analysis from a
#' `<tweetgraph_primitive>` or tweet `<data.frame>`.
#' 
#' @param x `<tweetgraph_primitive>` or tweet `<data.frame>`
#' @template param-dots
#' 
#' @return `<network>`
#' 
#' @template author-bk
#' 
#' @examples 
#' \dontrun{
#' 
#' 
#' hashtag_rstats <- rtweet::search_tweets("#rstats")
#' 
#' as_sna_network(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @export
as_sna_network <- function(x, ...) {
  UseMethod("as_sna_network")
}

#' @rdname as_sna_network
#' @export
as_sna_network.data.frame <- function(x, ...) {
  socnet_prim <- as_sna_primitive(x, ...)
  
  as_sna_network(socnet_prim)
}

#' @rdname as_sna_network
#' 
#' @export
as_sna_network.tweetgraph_primitive <- function(x, ...) {
  .network_from_tweetgraph_primitive(x)
}

#' @importFrom data.table := copy
#' @importFrom network add.edges network.initialize set.vertex.attribute
.network_from_tweetgraph_primitive <- function(x) {
  out <- network.initialize(
    n = nrow(x$nodes), 
    directed = TRUE,
    hyper = FALSE,
    loops = TRUE,
    multiple = length(attr(x, "actions")) > 1L,
    bipartite = FALSE
  )
  
  
  nw_edge_attr_names <- tg_edge_attr_names(x)
  nw_edges <- copy(x$edges)
  nw_edges[, c(1L, 2L) := lapply(
    .SD, function(.x) as.integer(factor(.x, levels = x$nodes$name))
   ),
  .SDcols = c(1L, 2L)]
  
  names_eval <- rep(
    list(as.list(nw_edge_attr_names)),
    nrow(x$edges)
  )
  
  vals_eval <- unname(
    lapply(
      split(nw_edges[, ..nw_edge_attr_names
                     ][, row_index := .I
                       ]
            , 
            by = "row_index"),
      function(.x) as.list(.x[, row_index := NULL])
    )
  )

  out <- add.edges(
    x = out,
    tail = nw_edges[[1L]],
    head = nw_edges[[2L]],
    names.eval = names_eval,
    vals.eval = vals_eval
  )
  
  for (i in seq_along(x$nodes)) {
    target_col <- names(x$nodes)[[i]]
    if (target_col == "name") {
      target_col <- "vertex.names"
    }
    
    out <- set.vertex.attribute(
      x = out,
      attrname = target_col,
      value = x$nodes[[i]]
    )
  }
  
  out
}
