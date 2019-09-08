#' @importFrom data.table %chin%
#' @importFrom igraph edge_attr edge_attr<- vertex_attr vertex_attr<-
set_graph_appearance <- function(tweet_graph, egos = NULL) {
  vertex_attr(tweet_graph, "shape") <- "icon"
  
  fa_icons <- list(status = "f075",
                   hashtag = "f292",
                   url    = "f0c1",
                   user    = "f007",
                   media   = "f03e")
  
  node_classes <- vertex_attr(tweet_graph, "node_class")
  
  user_i <- which(node_classes == "user")
  status_i <- which(node_classes == "status")
  hashtag_i <- which(node_classes == "hashtag")
  url_i <- which(node_classes == "url")
  media_i <- which(node_classes == "media")
  
  if (!.is_empty(user_i)) {
    vertex_attr(tweet_graph, "icon.code", index = user_i) <- fa_icons$user
    vertex_attr(tweet_graph, "icon.color", index = user_i) <- "orange"
    vertex_attr(tweet_graph, "title", 
                index = user_i) <- vertex_attr(tweet_graph, "screen_name", 
                                               index = user_i)
    
  }
  if (!.is_empty(status_i)) {
    vertex_attr(tweet_graph, "icon.code", index = status_i) <- fa_icons$status
    vertex_attr(tweet_graph, "icon.color", index = status_i) <- "blue"
    vertex_attr(tweet_graph, "title", 
                index = status_i) <- vertex_attr(tweet_graph, "text", 
                                                 index = status_i)
  }
  if (!.is_empty(hashtag_i)) {
    vertex_attr(tweet_graph, "icon.code", index = hashtag_i) <- fa_icons$hashtag
    vertex_attr(tweet_graph, "icon.color", index = hashtag_i) <- "gray"
    vertex_attr(tweet_graph, "title", 
                index = hashtag_i) <- vertex_attr(tweet_graph, "name", 
                                                  index = hashtag_i)
  }
  if (!.is_empty(url_i)) {
    vertex_attr(tweet_graph, "icon.code", index = url_i) <- fa_icons$url
    vertex_attr(tweet_graph, "icon.color", index = url_i) <- "purple" 
    vertex_attr(tweet_graph, "title", 
                index = url_i) <- vertex_attr(tweet_graph, "name", 
                                              index = url_i)
  }
  if (!.is_empty(media_i)) {
    vertex_attr(tweet_graph, "icon.code", index = media_i) <- fa_icons$media
    vertex_attr(tweet_graph, "icon.color", index = media_i) <- "green"
    vertex_attr(tweet_graph, "title", 
                index = media_i) <- vertex_attr(tweet_graph, "name", 
                                                index = media_i)
  }
  
  
  actions <- edge_attr(tweet_graph, "action")
  
  posts_i <- which(actions == "posts")
  contains_i <- which(actions == "contains")
  mentions_i <- which(actions == "mentions")
  retweet_i <- which(actions %chin% c("retweet", "was_retweeted_by",
                                      "retweet_user_id"))
  reply_to_i <- which(actions %chin% c("reply_to", "was_replied_to_by",
                                       "reply_to_user_id"))
  quoted_i <- which(actions %chin% c("quoted", "was_quoted_by",
                                     "quoted_user_id"))
  
  if (!.is_empty(posts_i)) {
    edge_attr(tweet_graph, "color", index = posts_i) <- "lightblue"
  }
  if (!.is_empty(contains_i)) {
    edge_attr(tweet_graph, "color", index = contains_i) <- "lightgreen"
  }
  if (!.is_empty(mentions_i)) {
    edge_attr(tweet_graph, "color", index = mentions_i) <- "lightgray"
  }
  if (!.is_empty(retweet_i)) {
    edge_attr(tweet_graph, "color", index = retweet_i) <- "darkbrown"
  }
  if (!.is_empty(reply_to_i)) {
    edge_attr(tweet_graph, "color", index = reply_to_i) <- "purple"
  }
  if (!.is_empty(quoted_i)) {
    edge_attr(tweet_graph, "color", index = quoted_i) <- "red"
  }
  
  tweet_graph
}


#' @importFrom data.table data.table
#' @importFrom igraph edge_attr vertex_attr
add_vis_legend <- function(vis_net, tweet_graph) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("{visNetwork} required for this functionality", call. = FALSE)
  }
  
  lnodes <- unique(
    data.table(
      label = vertex_attr(tweet_graph, "node_class"),
      icon.code = vertex_attr(tweet_graph, "icon.code"),
      icon.color = vertex_attr(tweet_graph, "icon.color")
    )
  )[, shape := "icon"]
  
  ledges <- unique(
    data.table(
      label = edge_attr(tweet_graph, "action"),
      color = edge_attr(tweet_graph, "color")
    )
  )
  
  visNetwork::visLegend(vis_net, addNodes = lnodes, addEdges = ledges,
                        zoom = FALSE, useGroups = FALSE)
}


#' Interactively visualize a graph of twitter data
#' 
#' @template param-tweet_df
#' @param tweet_graph 
#' * Currently objects obtained via `as_knowledge_graph()` and
#' `as_social_network()`
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
#' hashtag_rstats %>% 
#'   as_social_network() %>% 
#'   plot_vis_net()
#' 
#' 
#' }
#' 
#' @export
plot_vis_net <- function(tweet_graph) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("{visNetwork} required for this functionality", call. = FALSE)
  }
  
  tweet_graph <- set_graph_appearance(tweet_graph)
  
  tweet_graph %>% 
    visNetwork::visIgraph(idToLabel = FALSE) %>%
    visNetwork::addFontAwesome(name = "font-awesome-visNetwork") %>%
    add_vis_legend(tweet_graph)
}

