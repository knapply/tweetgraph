% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphml.R
\name{write_graphml}
\alias{write_graphml}
\title{Write a tweet \code{<igraph>} to a .graphml file}
\usage{
write_graphml(g, path, na_numbers_as = -1, na_characters_as = "",
  drop_na_attrs = TRUE)
}
\arguments{
\item{g}{\code{<igraph>} object}

\item{path}{\code{<character>} scalar}

\item{na_numbers_as}{\code{<numeric>} scalar indicating the value to which
\code{NA_real_}s and \code{NA_integer_} are re-coded Default: \code{-1}.}

\item{na_characters_as}{\code{<character>} scalar indicating the value to which
\code{NA_character_}s are re-coded. Default: \code{""}.}

\item{drop_na_attrs}{\code{<logical>} scalar indicating whether}
}
\description{
Write a tweet \code{<igraph>} to a .graphml file
}
\examples{
\dontrun{

hashtag_rstats <- rtweet::search_tweets("#rstats")

tweet_graph <- as_sna_igraph(tweet_df)

target_file_path <- tempfile(fileext = "graphml")

write_graphml(g = tweet_graph, path = target_file_path)

}

}
\author{
Brendan Knapp \email{brendan.g.knapp@gmail.com}
}
