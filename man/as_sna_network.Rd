% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network.R
\name{as_sna_network}
\alias{as_sna_network}
\alias{as_sna_network.data.frame}
\alias{as_sna_network.tweetgraph_primitive}
\title{Build a \code{<network>} a for social network analysis from a
\code{<tweetgraph_primitive>} or tweet \code{<data.frame>}.}
\usage{
as_sna_network(x, ...)

\method{as_sna_network}{data.frame}(x, ...)

\method{as_sna_network}{tweetgraph_primitive}(x, ...)
}
\arguments{
\item{x}{\code{<tweetgraph_primitive>} or tweet \code{<data.frame>}}

\item{...}{Arguments passed to or from other methods.}
}
\value{
\code{<network>}
}
\description{
Build a \code{<network>} a for social network analysis from a
\code{<tweetgraph_primitive>} or tweet \code{<data.frame>}.
}
\examples{
\dontrun{


hashtag_rstats <- rtweet::search_tweets("#rstats")

as_sna_network(hashtag_rstats)


}

}
\author{
Brendan Knapp \email{brendan.g.knapp@gmail.com}
}
