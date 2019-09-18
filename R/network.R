#' Build a `<network>` a for social network analysis from a
#' `<tweetgraph_primitive>` or tweet `<data.frame>`.
#' 
#' @param x `<tweetgraph_primitive>` or tweet `<data.frame>`
#' @param action `<character>`
#' * `"all"`, `"mentions"`, `"retweet"`, `"quoted"`, and/or `"reply_to"`
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
#' as_socnet_network(hashtag_rstats)
#' 
#' 
#' }
#' 
#' @export
as_socnet_network <- function(x, ...) {
  UseMethod("as_socnet_network")
}

#' @rdname as_socnet_network
#' @export
as_socnet_network.data.frame <- function(x, ...) {
  socnet_prim <- as_socnet_primitive(x, ...)
  
  as_socnet_network(socnet_prim)
}

#' @rdname as_socnet_network
#' 
#' @export
as_socnet_network.tweetgraph_primitive <- function(x, ...) {
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
  nw_edges[, c("from", "to") := lapply(
    .SD, function(.x) as.integer(factor(.x, levels = x$nodes$name))
   ), 
  .SDcols = c("from", "to")]
  
  names_eval <- rep(
    list(as.list(nw_edge_attr_names)),
    nrow(x$edges)
  )
  
  vals_eval <- unname(
    lapply(
      split(nw_edges[, ..nw_edge_attr_names][, row_index := .I], 
            by = "row_index"),
      function(.x) as.list(.x[,row_index := NULL])
    )
  )
  
  out <- add.edges(
    x = out,
    tail = nw_edges$from,
    head = nw_edges$to,
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
