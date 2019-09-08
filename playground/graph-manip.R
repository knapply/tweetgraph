library(tweetgraph)

# tweet_graph <- hongkongio::hkio_all_tweets() %>% 
  # as_knowledge_graph()


# test <- igraph::bfs(tweet_graph, igraph::V(tweet_graph), neimode = "in")
# # test$order[[1]]
# test <- lapply(1:igraph::vcount(tweet_graph),
#                function(x) {
#   init <- igraph::bfs(tweet_graph, x, neimode = "in", unreachable = FALSE,)$order
#   # init <- init[!is.na(init)]
#   init[length(init[!is.na(init)])]
# })
# 
# igraph::V(tweet_graph)



tweet_graph <- tweetgraph:::knapply_tweets %>% 
  tweetgraph::as_knowledge_graph()


knapply_df <- tweetgraph:::knapply_tweets %>% 
  as.data.table()


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
    variable.name = "action", value.name = "to"
    )[!is.na(to)
      ][, action := as.character(action)]
  setcolorder(edge_df, neworder = c("from", "to", "action", "status_id"))
  
  users <- tweetgraph:::extract_all_users(
    tweet_df
    )[user_id %chin% unique(c(edge_df$from, edge_df$to))
      ][, c("timestamp_ms", "account_created_at") := lapply(.SD, as.double),
        .SDcols = c("timestamp_ms", "account_created_at")
        ][, node_class := "user"]
  setnames(users,
           old = c("name", "user_id"), 
           new = c("TWITTER_NAME", "name"))
  
  statuses <- tweetgraph:::extract_all_statuses(
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

as_social_network(knapply_df)



# hk_df <- hongkongio::hkio_all_tweets()



hk_net <- as_social_network(hk_df)

# hk_net <- rtweet::network_graph(hk_df)

































tweet_df[
  , .(source = user_id, status = status_id,
      mentions_user_id, quoted_user_id, reply_to_user_id, retweet_user_id)
  ][, lapply(.SD, unlist, use.names = FALSE), 
    .SDcols = c("mentions_user_id", "quoted_user_id",
                "reply_to_user_id", "retweet_user_id"),
    by = .(source, status)
    ] %>% 
  melt(id.vars = c("source", "status"), 
       variable.name = "action",
       value.name = "target")
















edges <- as_data_frame(tweet_graph) %>% 
  as.data.table()

author_status <- edges[action == "posts", .(from, to)]

retweets <- edges[
  action == "was_retweeted_by"
  ][, from := author_status$from[match(author_status$from, from)]
    ]
                    # author_status, nomatch = 0L, on = c("from", "to")]


tweet_graph %>% 
  delete_vertices(!vertex_attr(tweet_graph, "node_class") %in% c("user", "status")) %>% 
  contract(1:vcount(.), vertex.attr.comb = c)

igraph::contract(
  tweet_graph, 
  1:vcount(tweet_graph),
  vertex.attr.comb = c
  # rep(which(vertex_attr(tweet_graph, "node_class") == "status"), each = 2)
) %>% 
  vertex_attr("node_class")

igraph::connect(tweet_graph, order = 2, ) %>% 
  as_data_frame() %>% 
  as.data.table()


lapply(
  igraph::make_ego_graph(tweet_graph, order = 2, 
                       nodes = igraph::vertex_attr(tweet_graph, "node_class") == "user"),
  function(x) delete_vertices(x, vertex_attr(x, "node_class") != "user")
  # function(x) {
    # x[x$node_class == "user"]
  # }
)


ends(tweet_graph, es = E(tweet_graph))


igraph::incident_edges(tweet_graph, v = 1)


tweet_df <- as.data.table(tweetgraph:::knapply_tweets)


tweet_df[, mentions_user_id2 := rep(unlist(mentions_user_id), .N),
         # .SDcols = "mentions_user_id",
         by = .I]

tweet_df[, ROW_ID := .I]

list_cols <- names(which(.map_lgl(tweet_df, is.list)))

rbindlist(
  lapply(split(tweet_df, by = "ROW_ID"), function(x) {
    as.data.table(lapply(x, unlist))
  })
)


rbindlist(
  lapply(list_cols, function(x) {
    to_keep <- c("ROW_ID", x)
    lapply(as.list(tweet_df[, ..to_keep]), unlist)
    # as.data.table(unlist(tweet_df[, ..x], recursive = FALSE))
    # tweet_df[, #..to_keep
             # ][, (x) := unlist(.SD, use.names = FALSE),
               # .SDcols = x,
               # by = ROW_ID]
  }), fill = TRUE
)



unnested <- split(tweet_df, by = "status_id")
to_unnest <- c("retweet_user_id", "quoted_user_id", "mentions_user_id")

unnested <- rbindlist(
  lapply(unnested, function(.x) {
    for (i in seq_along(to_unnest)) {
      .x[, (to_unnest[[i]]) := rep(unlist(.SD, use.names = FALSE), .N),
         .SDcols = to_unnest[[i]],
         by = .I]
    }
    .x
  })
)




for (i in seq_along(to_unnest)) {
  unnested[, (to_unnest[[i]]) := unlist(.SD, use.names = FALSE),
           .SDcols = to_unnest[[i]],
           by = .I]
}

unnested <- copy(tweet_df)[
  , mentions_user_id := unlist(mentions_user_id, use.names = FALSE),
  by = .I
  ][
    retweet_screen_name := unlist(retweet_screen_name, use.names = FALSE)
    ][, c("screen")]


# 
# 
# as_social_network <- function(tweet_df) {
#   
#   rt_cols <- setdiff(names(tweet_df), 
#   
#   retweets <- copy(tweet_df)[
#     , retweet_user_id := unlist(retweet_user_id, use.names = FALSE),
#     by = .I
#     ][!is.na(retweet_user_id), 
#       screen_name = unlist(retweet_screen_name, use.names = FALSE)
#       ]
#   
#   
#   
#   
#   
#   [, retweet_screen_name := unlist()]
#   
#   temp <- copy(tweet_df)[, row_id := .I]
#   statuses <- split(temp, by = "row_id")
#   
#   rbindlist(
#     lapply(statuses, function(.x) {
#       unlist(.x, recursive = FALSE)
#       # .x[, lapply(.SD, unlist, use.names = FALSE),
#          # by = row_id]
#       # lapply(.x, unlist, recursive = FALSE)
#     })
#   )
# }
# 
# 
# 
# 
# all_nodes_bfs <- function(.adj_mat) {
#   n_nodes   <- nrow(.adj_mat)
#   seq_nodes <- seq_len(n_nodes)
#   
#   frontier <- Matrix(0, nrow = n_nodes, ncol = n_nodes, sparse = FALSE, doDiag = FALSE)
#   diag(frontier) <- 1
#   mask           <- frontier
#   
#   out <- as.list(seq_nodes)
#   
#   for (depth in seq_nodes) {
#     frontier <- crossprod(.adj_mat, frontier) & !mask
#     
#     if ( !any(frontier) ) break
#     
#     # out <- mapply(c, out, apply(frontier, 2L, which), SIMPLIFY = FALSE)
#     out <- apply(frontier, 2L, which)
#     
#     mask[frontier] <- 1
#   }
#   
#   out
# }
# 
# 
# step_bfs <- function(.adj_mat) {
#   n_nodes   <- nrow(.adj_mat)
#   seq_nodes <- seq_len(n_nodes)
#   
#   frontier <- Matrix(0, nrow = n_nodes, ncol = n_nodes, sparse = FALSE, doDiag = FALSE)
#   diag(frontier) <- 1
#   # mask           <- frontier
#   crossprod(.adj_mat, frontier)
#   # out <- apply(frontier, 2L, which)
#   # Filter(length, out)
# }
# 
# 
# 
# tweet_graph %>% 
#   as_adjacency_matrix() %>% 
#   step_bfs()
# 
# 
# as_adj_tensor <- function(tweet_graph) {
#   slices <- setNames(
#     nm = c("was_retweeted_by", "was_quoted_by", 
#            "was_replied_to_by", "mentions")
#   )
#   
#   lapply(slices, function(x) {
#     sub_graph <- subgraph.edges(
#       tweet_graph, eids = which(
#         edge_attr(tweet_graph, "action") == c(x, "posts")
#       ),
#       delete.vertices = FALSE
#     )
#     
#     as_adjacency_matrix(sub_graph, sparse = TRUE)
#   })
# }
# 
# 
# adj_tensor <- tweet_graph %>% as_adj_tensor()
# 
# 
# crossprod(adj_tensor$was_retweeted_by[status_i, user_i]) %>% 
#   graph_from_adjacency_matrix()
#   colnames()
# 
# adj_tensor$was_retweeted_by[user_i, status_i] %*% 
#   adj_tensor$was_retweeted_by[user_i, status_i] %>% 
#   graph_from_adjacency_matrix() %>% 
#   `vertex_attr<-`(value = vertex_attr(tweet_graph, index = user_i))
# 
# 
# 
# adj_tensor$
# 
# 
# from_step <- lapply(adj_tensor, step_bfs)
# to_step <- lapply(adj_tensor, function(x) step_bfs(t(x)))
# 
# 
# from_step$posts
# 
# 
# status_status <- el[source_class == "status" & target_class == "status"]
# 
# el[action == "posts", .(from, to)]
# 
# status_status[el[action == "posts", .(from, to)], on = .(from = to)]
# 
# 
# user_i <- which(V(tweet_graph)$node_class == "user")
# status_i <- which(V(tweet_graph)$node_class == "status")
# user_status_i <- c(user_i, status_i)
# 
# library(data.table)
# el <- igraph::as_data_frame(tweet_graph) %>% as.data.table()
# el[source_class %in% c("user", "status") & target_class %in% c("user", "status")
#    ]
# 
# el[source_class == "user" & target_class == "status"]
# el[source_class == "status" & target_class == "user"]
# 
# el[, .(from = el$frommatch())]
# 
# 
# el[el$source_class %in% c("user", "status") & el$target_class %in% c("user", "status"), ]
# 
# el <- el[el[, 1] %in% user_status_i & el[, 2] %in% user_status_i, ]
# 
# el[ , ]
# 
# 
# 
# adj_mat <- as_adjacency_matrix(tweet_graph)
# 
# diag_mat <- adj_mat
# diag_mat[] <- 0
# diag(diag_mat) <- 1
# 
# user_to_status <- crossprod(adj_mat, diag_mat, boolArith = TRUE)[user_i, status_i]
# 
# 
# status_to_status <- crossprod(user_to_status, boolArith = TRUE)[status_i, status_i]
# 
# Filter(length, apply(user_to_status, 2L, which))
# Filter(length, apply(status_to_status, 2L, which))
# 
# 
# crossprod(crossprod(adj_mat, diag_mat), crossprod(adj_mat, diag_mat))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# all_nodes_bfs(as_adjacency_matrix(tweet_graph) %>% t()) %>% 
#   unlist() %>% 
#   table() %>% 
#   sort()
# 
# 
# 
# slices <- setNames(
#   nm = c("posts", "was_retweeted_by", "was_quoted_by", 
#          "was_replied_to_by", "mentions")
# )
# 
# rhs_slices <- slices[!names(slices) == "posts"]
# 
# 
# adj_tensor <- lapply(slices, function(x) {
#   sub_graph <- subgraph.edges(
#     tweet_graph, eids = which(
#       edge_attr(tweet_graph, "action") == x
#     ),
#     delete.vertices = FALSE
#   )
#   
#   as_adjacency_matrix(as.undirected(sub_graph))
# })
# 
# bfs <- all_nodes_bfs(adj_tensor$posts,
#               target_nodes = which(V(tweet_graph)$node_class == "status"))
# 
# user_i <-  which(V(tweet_graph)$node_class == "user")
# 
# lapply(bfs, .discard, function(x) !x %in% user_i)
# 
# 
# bfs()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(igraph)
# library(Matrix)
# 
# tweet_graph <- tweetgraph:::knapply_tweets %>%
#   tweetgraph::as_knowledge_graph()
# 
# action <- "was_retweeted_by"
# lhs_class <- "user"
# rhs_class <- "status"
# 
# unique(E(tweet_graph)$action)
# 
# fold_graph <- function(tweet_graph, lhs_class, rhs_class) {
#   # edges_to_ignore <- which(!edge_attr(tweet_graph, "action") %in% action)
#   # temp_g <- delete_edges(tweet_graph, edges_to_ignore)
#   
#   user_i <- which(vertex_attr(tweet_graph, "node_class") == "user")
#   status_i <- which(vertex_attr(tweet_graph, "node_class") == "status")
#   
#   user_diag <- Matrix(FALSE, nrow = vcount(tweet_graph), ncol = vcount(tweet_graph))
#   status_diag <- user_diag
#   diag(user_diag)[user_i] <- TRUE
#   diag(status_diag)[status_i] <- TRUE
#   
#   
#   slices_present <- setNames(nm = unique(edge_attr(tweet_graph, "action")))
#   # slices_present <- slices_present[slices_present != "posts"]
#   
#   slices <- setNames(
#     nm = c("posts", "was_retweeted_by", "was_quoted_by", 
#            "was_replied_to_by", "mentions")
#   )
#   rhs_slices <- slices[!names(slices) == "posts"]
#   
#   adj_tensor <- lapply(slices, function(x) {
#     sub_graph <- subgraph.edges(
#       tweet_graph, eids = which(
#         edge_attr(tweet_graph, "action") == x
#       ),
#       delete.vertices = FALSE
#     )
#     
#     as_adjacency_matrix(as.undirected(sub_graph))
#   })
#   # lhs <- crossprod(adj_tensor$posts, status_diag)
#   
#   
#   # crossprod(adj_tensor$posts, user_diag) +
#   # t(adj_tensor$posts) %*% user_diag
#   
#   mxm_res <- lapply(adj_tensor[rhs_slices], function(.x) {
#     # a <- crossprod(.x, status_diag)
#     # b <- crossprod(a, user_diag)
#     # c <- b & crossprod(.x, user_diag)
#     c <- .x %*% .x %*% t(adj_tensor$posts)
#     # a <- t(adj_tensor$posts) %*% user_diag
#     # b <- t(.x) %*% user_diag
#     # c <- a + b
#     
#     
#     # crossprod(.x[user_i, status_i])
#     
#     # diag(c) <- FALSE
#     # d <- t(c) %*% user_diag
#     
#     # c <- t(.x) %*% status_diag %*% status_diag %*% user_diag
#     
#     # a <- adj_tensor$posts %*% status_diag
#     # b <- t(a) %*% .x
#     # c <- t(b) %*% status_diag
#     # d <- t(c) %*% user_diag
#     
#     # a <- crossprod(adj_tensor$posts, user_diag)
#     # b <- crossprod(a, .x)
#     # c <- crossprod(b, user_diag)
#     
#     # c <- c[c[, 1] != c[, 2], ]
#     
#     out <- c %>%
#       summary() %>% 
#       .[, 1:2]
#     
#     out <- out[out[, 1] != out[, 2], ]
#     
#     out %>% t()
#   })
#   mxm_res <- .keep(mxm_res, function(.x) sum(.x) != 0)
#   
#   out <- make_empty_graph(vcount(tweet_graph))
#   for (i in seq_along(mxm_res)) {
#     out <- add_edges(
#       graph = out,
#       edges = mxm_res[[i]], #t(summary(mxm_res[[i]])[, 1:2]),
#       attr = list(action = names(mxm_res)[[i]])
#     )
#   }
#   
#   vertex_attr(out) <- vertex_attr(tweet_graph)
#   out <- delete_vertices(out, vertex_attr(out, "node_class") != "user")
#   # out <- delete_edges(out, E(out)[edge_attr(out, "action") == "mentions"]) 
#   out
#   
#   out %>% 
#     # delete_edges(which(edge_attr(., "action") == "mention")) %>% edge_attr("action") 
#     set_vertex_attr("name", value = V(.)$screen_name) %>%
#     set_edge_attr("label", value = E(.)$action) %>%
#     visNetwork::visIgraph() %>%
#     # visNetwork::visEdges(label = ) %>%
#     visNetwork::visOptions(nodesIdSelection = TRUE, selectedBy = "node_class")
#   
#   
#   
#   
#   
#   
#   
# }
# #   
# #   
# #   E(t(
# #     crossprod(adj_tensor$posts, status_diag) %*%
# #     crossprod(adj_tensor$was_replied_to_by, status_diag) 
# #   ) %>% #%*% user_diag %>% #sum()
# #     graph_from_adjacency_matrix() %>% 
# #     `vertex_attr<-`(value = vertex_attr(tweet_graph)) %>%
# #     delete_vertices(v = which(degree(.) == 0)) %>%
# #     plot(layout = layout_with_kk, vertex.label = V(.)$screen_name,
# #          asp = 0.5, vertex.size = 1)
# #     # delete_vertices(v = which(degree(.) == 0)) %>%
# #     set_vertex_attr("name", value = V(.)$screen_name) %>% 
# #     set_edge_attr("label", value = E(.)$action) %>% 
# #     visNetwork::visIgraph() %>% 
# #     # visNetwork::visEdges(label = ) %>% 
# #     visNetwork::visOptions(nodesIdSelection = TRUE, selectedBy = "node_class")
# #     
# #   
# #   
# #   
# #   
# #   
# #   .[user_i, user_i] %>% sum()
# #   
# #   %&%
# #     tcrossprod(adj_tensor$posts, user_diag, boolArith = TRUE) %>% sum()
# #   
# #   
# #   
# #   (t(adj_tensor$posts) %&% status_diag) %&% (t(adj_tensor$was_retweeted_by) %&% status_diag) %>% sum()
# #   all.equal(
# #     crossprod(adj_tensor$posts, status_diag, boolArith = TRUE),
# #     t(adj_tensor$posts) %&% status_diag
# #   )
# #   
# #   # sum(status_diag)
# #   
# #   
# #   
# #   mxm_res <- ( 
# #     t(adj_tensor$posts) %&% (t(adj_tensor$posts) %&% adj_tensor$was_retweeted_by)
# #   )[user_i, user_i]
# #     # tcrossprod(
# #     # tcrossprod(adj_tensor$posts, adj_tensor$was_retweeted_by, boolArith = TRUE),
# #     # adj_tensor$posts, boolArith = TRUE
# #     # )[user_i, user_i]
# #     # t(adj_tensor$posts) %&% adj_tensor$was_retweeted_by %&% t(adj_tensor$posts)
# #     # adj_tensor$posts[, user_i] %&% adj_tensor$was_retweeted_by[status_i, ]  #%&% t(adj_tensor$posts)
# #     # )
# #   
# #   sum(mxm_res)
# #   
# #   
# #   
# #   
# #   out <- make_empty_graph(n = length(lhs_indices))
# #   out <- add_edges(
# #     out, edges = t(summary(mxm_res))
# #   )
# #   
# #   
# #   
# #   
# #   mxm_res <- lapply(slices_present, function(x) {
# #     sub_graph <- subgraph.edges(
# #       tweet_graph, eids = which(
# #         edge_attr(tweet_graph, "action") %in% c(x, "posts")
# #       ),
# #       delete.vertices = FALSE
# #     )
# #     adj_mat <- as_adjacency_matrix(sub_graph)[rhs_indices, lhs_indices]
# #     res <- t(adj_mat) %&% adj_mat
# #     diag(res) <- FALSE
# #     res
# #     # tcrossprod(
# #       # as_adjacency_matrix(sub_graph)[lhs_indices, rhs_indices],
# #       # boolArith = TRUE
# #     # )
# #   })
# #   
# #   
# #   out <- make_empty_graph(n = length(lhs_indices))
# # 
# #   for (i in seq_along(mxm_res)) {
# #     out <- add_edges(out, t(summary(mxm_res[[i]])), 
# #                      attr = list(action = names(mxm_res)[[i]]))
# #   }
# #   
# #   out_attrs <- .discard(vertex_attr(tweet_graph, index = lhs_indices),
# #                         function(.x) all(is.na(.x)))
# #   
# #   vertex_attr(out) <- out_attrs
# #   
# #   
# #   out
# # }
# # 
# # 
# # V(tweet_graph)[betweenness(tweet_graph) == max(betweenness(tweet_graph))]$screen_name
# # 
# # fold_graph(tweet_graph, "user", "status") %>%  # igraph::as_data_frame() %>% tibble::as_tibble() %>% dplyr::distinct(action)
# #   set_vertex_attr("name", value = V(.)$screen_name) %>% 
# #   set_edge_attr("label", value = E(.)$action) %>% 
# #   visNetwork::visIgraph() %>% 
# #   # visNetwork::visEdges(label = ) %>% 
# #   visNetwork::visOptions(nodesIdSelection = TRUE, selectedBy = "node_class")
# # 
# # 
# # 
# # 
# # # rstats <- rtweet::search_tweets("#rstats")
# # # tweet_graph <- tweetgraph::as_knowledge_graph(rstats)
# # 
# # lhs_indices <- which(vertex_attr(tweet_graph, "node_class") == "user")
# # rhs_indices <- which(vertex_attr(tweet_graph, "node_class") == "status")
# # adj_mat <- as_adjacency_matrix(tweet_graph, edges = T)[rhs_indices, lhs_indices]
# # 
# # drop_diag <- function(x) {
# #   diag(x) <- 0
# #   x
# # }
# # 
# # t(adj_mat) %&% adj_mat %>%
# #   drop_diag() %>% 
# #   graph_from_adjacency_matrix() %>% 
# #   set_vertex_attr("name", value = V(tweet_graph)$screen_name[lhs_indices])
# # 
# # 
# # 
# # 
# # 
# # 
# # rtweet::network_graph(tweetgraph:::knapply_tweets)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # tweet_graph
# # 
# # tweets_only <- delete_vertices(
# #   tweet_graph, v = vertex_attr(tweet_graph, name = "node_class") != "status"
# # )
# # 
# # 
# # tweets_only %>% 
# #   ggraph("nicely") +
# #   geom_edge_link() +
# #   geom_node_point()
# # 
# # 
# # plot(tweets_only, vertex.label = NA)
# # 
# # 
# # library(igraph)
# # library(Matrix)
# # adj_mat <- as_adjacency_matrix(tweet_graph)
# # status_nodes <- which(vertex_attr(tweet_graph, "node_class") == "status")
# # 
# # 
# # 
# # 
# # fold_graph <- function(tweet_graph, target_class) {
# #   target_nodes <- which(
# #     vertex_attr(tweet_graph, name = node_class) == target_class
# #   )
# #   adj_mat <- as_adjacency_matrix(tweet_graph)
# #   
# #   
# #   
# #   out <- graph_from_adjacency_matrix(adj_mat)
# #   vertex_attr(out) <- vertex_attr(tweet_graph, index = target_class)
# #   
# #   out
# # }
# # 
# # 
# # 
# # 
# # 
# # graph_from_adjacency_matrix(
# #   tweet_graph[status_nodes, ] %*% t(tweet_graph[status_nodes, ])
# # ) %>% 
# #   
# #   
# # 
# # crossprod(tweet_graph[, status_nodes],
# #           tweet_graph[, status_nodes])
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # library(ggraph)
# # tweet_graph %>% 
# #   igraph::mst() %>% 
# #   ggraph(layout = "fabric",  sort.by = node_rank_fabric()) +
# #   geom_node_range() +
# #   geom_edge_span()
# #   geom_edge_elbow()
# #   geom_edge_diagonal()
# #   geom_edge_link()
# # 
# # tweet_graph %>%
# #   igraph::mst() %>%
# #   ggraph( 'fabric', sort.by = node_rank_fabric()) + 
# #   geom_node_point(aes(colour = node_class)) + 
# #   geom_edge_span(aes(colour = action), end_shape = 'square', 
# #                  show.legend = FALSE) + 
# #   coord_fixed() +
# #   scale_edge_color_viridis(discrete = TRUE) +
# #   facet_edges(~ action)
# # 
# # ggraph(tweet_graph %>% igraph::mst(), layout = 'linear') + 
# #   geom_node_point(aes(fill = node_class), 
# #                   color = "transparent", shape = 21) +
# #   geom_edge_arc(aes(colour = action),
# #                 show.legend = FALSE) + 
# #   coord_fixed() +
# #   theme(legend.title = element_blank()) +
# #   facet_edges(~ action, nrow = 1) +
# #   guides(fill = guide_legend(override.aes = list(size = 10))) +
# #   # scale_fill_viridis_d() +
# #   scale_edge_color_viridis(discrete = TRUE) +
# #   coord_flip()
# #  
# # 
# # as.dendrogram(tweet_graph)
# # 
# # ggraph(tweet_graph %>% igraph::mst(), 'dendrogram', circular = TRUE) +
# #   geom_edge_diagonal() 
# # +
# #   geom_node_point()
# #   geom_node_circle(aes(fill = depth), size = 0.25, n = 50) + 
# #   coord_fixed()
# # 
# # 
# # 
# # 
# # V(tweet_graph)[degree(tweet_graph, mode = "out") == 1]
# # 
# # 
# # 
# # adj_mat <- igraph::as_adjacency_matrix(tweet_graph)
# # 
# # 
# # 
# # library(igraph)
# # library(Matrix)
# # fold_graph <- function(tweet_graph, source_class, action, target_class, ...) {
# #   temp_g <- subgraph.edges(
# #     graph = tweet_graph, 
# #     eids = which(edge_attr(tweet_graph, name = "action") %in% action),
# #     delete.vertices = FALSE
# #   )
# #   
# #   source_nodes <- which(
# #     vertex_attr(tweet_graph, name = "node_class") %in% source_class
# #   )
# #   target_nodes <- which(
# #     vertex_attr(tweet_graph, name = "node_class") %in% target_class
# #   )
# #   
# #   adj_mat <- as_adjacency_matrix(temp_g)
# #   
# #   crossprod(tweet_graph[source_nodes, target_nodes])
# # }
# # 
# 
# fold_graph(tweet_graph, "user", "posts", "status")
# 
# 
# 
# 
# 
# 
# 
# 
# el <- head(igraph::as_edgelist(tweet_graph, names = FALSE))
# 
# el - 1
# head(tweet_graph[7])
# head(unclass(tweet_graph)[[3]])
# head(unclass(tweet_graph)[[4]])
# 
# .as_edgelist <- function(g) {
#   unclassed <- unclass(g)
#   cbind(unclassed[[4L]], unclassed[[3L]])
# }
# 
# 
# zac <- igraph::graph("Zachary")
# zac_el <- igraph::as_edgelist(zac) - 1
# 
# identical(zac_el, .as_edgelist(zac))
# 
# 
# 
# test_el <- cbind(
#   unclass(zac)[[3L]],
#   unclass(zac)[[4L]]
# )
# 
# 
# 
# hk_graph <- hongkongio::hkio_all_tweets() %>% 
#   as_knowledge_graph()
# 
# adj_mat <- igraph::as_adjacency_matrix(
#   graph = hk_graph
# )
# 
# dim(adj_mat)
# 
