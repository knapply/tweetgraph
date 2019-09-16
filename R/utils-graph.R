tg_edge_attr_names <- function(x, ...) {
  UseMethod("tg_edge_attr_names")
}
tg_edge_attr_names.tweetgraph_primitive <- function(x, ...) {
  intersect(
    names(x$edges), 
    c("action","status_id","text", "source", "lang",
      "time", "source_class", "target_class")
  )
}