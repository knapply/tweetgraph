
<!-- README.Rmd generates README.md. -->

# tweetgraph

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![GitHub last
commit](https://img.shields.io/github/last-commit/knapply/tweetgraph.svg)](https://github.com/knapply/tweetgraph/commits/master)
[![Codecov test
coverage](https://codecov.io/gh/knapply/tweetgraph/branch/master/graph/badge.svg)](https://codecov.io/gh/knapply/tweetgraph?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/knapply/tweetgraph?branch=master&svg=true)](https://ci.appveyor.com/project/knapply/tweetgraph)
[![Travis-CI Build
Status](https://travis-ci.org/knapply/tweetgraph.svg?branch=master)](https://travis-ci.org/knapply/tweetgraph)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Depends](https://img.shields.io/badge/Depends-GNU_R%3E=3.6-blue.svg)](https://www.r-project.org/)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/knapply/tweetgraph.svg)](https://github.com/knapply/tweetgraph)
[![HitCount](http://hits.dwyl.io/knapply/tweetgraph.svg)](http://hits.dwyl.io/knapply/tweetgraph)
<!-- badges: end -->

# Overview

# Installation

``` r
# install.packages("remotes")
remotes::install_github("knapply/tweetgraph")
```

# Usage

``` r
library(tweetgraph)

hashtag_rstats <- rtweet::search_tweets("#rstats")

tweet_graph <- as_knowledge_graph(hashtag_rstats)

tweet_graph
```

    #> IGRAPH ea52eb1 DN-- 331 739 -- 
    #> + attr: timestamp_ms (v/n), name (v/c), created_at (v/n), text
    #> | (v/c), source (v/c), lang (v/c), node_class (v/c), TWITTER_NAME
    #> | (v/c), screen_name (v/c), location (v/c), description (v/c), url
    #> | (v/c), protected (v/l), followers_count (v/n), friends_count
    #> | (v/n), listed_count (v/n), statuses_count (v/n),
    #> | favourites_count (v/n), account_created_at (v/n), verified
    #> | (v/l), profile_url (v/c), account_lang (v/l), time (e/n),
    #> | source_class (e/c), action (e/c), target_class (e/c)
    #> + edges from ea52eb1 (vertex names):
    #> [1] 1165301524412526593->1169993733611429889
    #> + ... omitted several edges
