#' Write a tweet `<igraph>` to a .graphml file
#' 
#' @param g `<igraph>` object
#' @param path `<character>` scalar
#' @param na_numbers_as `<numeric>` scalar indicating the value to which
#'  `NA_real_`s and `NA_integer_` are re-coded Default: `-1`.
#' @param na_characters_as `<character>` scalar indicating the value to which 
#' `NA_character_`s are re-coded. Default: `""`.
#' @param drop_na_attrs `<logical>` scalar indicating whether 
#' 
#' @examples
#' \dontrun{
#' 
#' hashtag_rstats <- rtweet::search_tweets("#rstats")
#'
#' tweet_graph <- as_sna_igraph(tweet_df)
#'
#' target_file_path <- tempfile(fileext = "graphml")
#'
#' write_graphml(g = tweet_graph, path = target_file_path)
#'
#' }
#' 
#' @template author-bk
#' @importFrom igraph vertex_attr vertex_attr<- delete_vertex_attr write_graph
#' @export
write_graphml <- function(g, path, na_numbers_as = -1, 
                          na_characters_as = "", drop_na_attrs = TRUE) {
  if (!inherits(g, "igraph")) {
    stop("`g` must be an <igraph>.", call. = FALSE)
  }
  if (!is.character(path) | length(path) != 1L) {
    stop("`path` must be a <character> scalar.", call. = FALSE)
  }
  if (!is.null(na_numbers_as)) {
    if (!is.numeric(na_numbers_as) | length(na_numbers_as) != 1L) {
      stop("`na_numbers_as` must be `NULL` or a scalar <numeric>.", 
           call. = FALSE)
    }
  }
  if (!is.null(na_characters_as)) {
    if (!is.character(na_characters_as) | length(na_characters_as) != 1L) {
      stop("`na_characters_as` must be `NULL` or a scalar <character>.", 
           call. = FALSE)
    }
  }

  if (drop_na_attrs) {
    node_all_na_attrs <- which(
      .map_lgl(vertex_attr(g), function(.x) all(is.na(.x)))
    )
    if (!.is_empty(node_all_na_attrs)) {
      g <- delete_vertex_attr(g, name = names(node_all_na_attrs))
    }
  }
  
  if (!is.null(na_numbers_as)) {
    vertex_attr(g) <- lapply(vertex_attr(g), function(.x) {
      if (!is.numeric(.x)) return(.x)
      .x[is.na(.x)] <- na_numbers_as
      .x
    })
  }
  
  if (!is.null(na_characters_as)) {
    vertex_attr(g) <- lapply(vertex_attr(g), function(.x) {
      if (!is.character(.x)) return(.x)
      .x[is.na(.x)] <- na_characters_as
      
      gsub("[[:cntrl:]]", "", .x)
    })
  }
  
  write_graph(g, file = path, format = "graphml")
}


