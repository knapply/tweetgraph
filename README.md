
<!-- README.Rmd generates README.md. -->

# `{tweetgraph}`

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

# Installation

``` r
# install.packages("remotes")
remotes::install_github("knapply/tweetgraph")
```

# Usage

``` r
library(tweetgraph)

hashtag_rstats <- rtweet::search_tweets("#rstats")
```

## Knowledge Graph

### `as_kg_primitive()`

``` r
kg_data <- as_kg_primitive(hashtag_rstats)

lapply(kg_data, names)
```

    #> $edges
    #> [1] "source"       "target"       "time"         "source_class"
    #> [5] "action"       "target_class"
    #> 
    #> $nodes
    #>  [1] "name"               "timestamp_ms"       "created_at"        
    #>  [4] "text"               "source"             "lang"              
    #>  [7] "node_class"         "TWITTER_NAME"       "screen_name"       
    #> [10] "location"           "description"        "url"               
    #> [13] "protected"          "followers_count"    "friends_count"     
    #> [16] "listed_count"       "statuses_count"     "favourites_count"  
    #> [19] "account_created_at" "verified"           "profile_url"       
    #> [22] "account_lang"

### Using `{igraph}`

``` r
as_kg_igraph(hashtag_rstats)
```

    #> IGRAPH b7b6111 DN-- 314 853 -- 
    #> + attr: name (v/c), timestamp_ms (v/n), created_at (v/n), text
    #> | (v/c), source (v/c), lang (v/c), node_class (v/c), TWITTER_NAME
    #> | (v/c), screen_name (v/c), location (v/c), description (v/c), url
    #> | (v/c), protected (v/l), followers_count (v/n), friends_count
    #> | (v/n), listed_count (v/n), statuses_count (v/n),
    #> | favourites_count (v/n), account_created_at (v/n), verified
    #> | (v/l), profile_url (v/c), account_lang (v/l), time (e/n),
    #> | source_class (e/c), action (e/c), target_class (e/c)
    #> + edges from b7b6111 (vertex names):
    #> [1] 1174409462960021509->1174496989859368960
    #> + ... omitted several edges

``` r
as_kg_igraph(kg_data)
```

    #> IGRAPH b7b6fc9 DN-- 314 853 -- 
    #> + attr: name (v/c), timestamp_ms (v/n), created_at (v/n), text
    #> | (v/c), source (v/c), lang (v/c), node_class (v/c), TWITTER_NAME
    #> | (v/c), screen_name (v/c), location (v/c), description (v/c), url
    #> | (v/c), protected (v/l), followers_count (v/n), friends_count
    #> | (v/n), listed_count (v/n), statuses_count (v/n),
    #> | favourites_count (v/n), account_created_at (v/n), verified
    #> | (v/l), profile_url (v/c), account_lang (v/l), time (e/n),
    #> | source_class (e/c), action (e/c), target_class (e/c)
    #> + edges from b7b6fc9 (vertex names):
    #> [1] 1174409462960021509->1174496989859368960
    #> + ... omitted several edges

``` r
kg_data %>% 
  as_kg_igraph() %>% 
  plot_vis_net()
```

<img src="man/figures/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

### Using `{network}`

``` r
as_kg_network(hashtag_rstats)
```

    #>  Network attributes:
    #>   vertices = 314 
    #>   directed = TRUE 
    #>   hyper = FALSE 
    #>   loops = TRUE 
    #>   multiple = FALSE 
    #>   bipartite = FALSE 
    #>   total edges= 853 
    #>     missing edges= 0 
    #>     non-missing edges= 853 
    #> 
    #>  Vertex attribute names: 
    #>     account_created_at account_lang created_at description favourites_count followers_count friends_count lang listed_count location node_class profile_url protected screen_name source statuses_count text timestamp_ms TWITTER_NAME url verified vertex.names 
    #> 
    #>  Edge attribute names: 
    #>     action source source_class target_class time

``` r
as_kg_network(kg_data)
```

    #>  Network attributes:
    #>   vertices = 314 
    #>   directed = TRUE 
    #>   hyper = FALSE 
    #>   loops = TRUE 
    #>   multiple = FALSE 
    #>   bipartite = FALSE 
    #>   total edges= 853 
    #>     missing edges= 0 
    #>     non-missing edges= 853 
    #> 
    #>  Vertex attribute names: 
    #>     account_created_at account_lang created_at description favourites_count followers_count friends_count lang listed_count location node_class profile_url protected screen_name source statuses_count text timestamp_ms TWITTER_NAME url verified vertex.names 
    #> 
    #>  Edge attribute names: 
    #>     action source source_class target_class time

## Social Network Analysis

### `as_sna_primitive()`

``` r
sna_data <- as_sna_primitive(hashtag_rstats)

lapply(sna_data, names)
```

    #> $edges
    #> [1] "from"      "to"        "action"    "status_id" "text"      "source"   
    #> [7] "lang"     
    #> 
    #> $nodes
    #>  [1] "name"               "timestamp_ms"       "TWITTER_NAME"      
    #>  [4] "screen_name"        "location"           "description"       
    #>  [7] "url"                "protected"          "followers_count"   
    #> [10] "friends_count"      "listed_count"       "statuses_count"    
    #> [13] "favourites_count"   "account_created_at" "verified"          
    #> [16] "profile_url"        "account_lang"       "node_class"

### Using `{igraph}`

``` r
as_sna_igraph(hashtag_rstats)
```

    #> IGRAPH ba437d8 DN-- 100 221 -- 
    #> + attr: name (v/c), timestamp_ms (v/n), TWITTER_NAME (v/c),
    #> | screen_name (v/c), location (v/c), description (v/c), url (v/c),
    #> | protected (v/l), followers_count (v/n), friends_count (v/n),
    #> | listed_count (v/n), statuses_count (v/n), favourites_count
    #> | (v/n), account_created_at (v/n), verified (v/l), profile_url
    #> | (v/c), account_lang (v/l), node_class (v/c), action (e/c),
    #> | status_id (e/c), text (e/c), source (e/c), lang (e/c)
    #> + edges from ba437d8 (vertex names):
    #> [1] 2563455493->20444825 2563455493->20444825 1340110171->90938499
    #> [4] 1340110171->90938499
    #> + ... omitted several edges

``` r
as_sna_igraph(sna_data)
```

    #> IGRAPH ba4419e DN-- 100 221 -- 
    #> + attr: name (v/c), timestamp_ms (v/n), TWITTER_NAME (v/c),
    #> | screen_name (v/c), location (v/c), description (v/c), url (v/c),
    #> | protected (v/l), followers_count (v/n), friends_count (v/n),
    #> | listed_count (v/n), statuses_count (v/n), favourites_count
    #> | (v/n), account_created_at (v/n), verified (v/l), profile_url
    #> | (v/c), account_lang (v/l), node_class (v/c), action (e/c),
    #> | status_id (e/c), text (e/c), source (e/c), lang (e/c)
    #> + edges from ba4419e (vertex names):
    #> [1] 2563455493->20444825 2563455493->20444825 1340110171->90938499
    #> [4] 1340110171->90938499
    #> + ... omitted several edges

``` r
sna_data %>% 
  as_sna_igraph() %>% 
  plot_vis_net()
```

<img src="man/figures/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

### Using `{network}`

``` r
as_sna_network(hashtag_rstats)
```

    #>  Network attributes:
    #>   vertices = 100 
    #>   directed = TRUE 
    #>   hyper = FALSE 
    #>   loops = TRUE 
    #>   multiple = TRUE 
    #>   bipartite = FALSE 
    #>   total edges= 221 
    #>     missing edges= 0 
    #>     non-missing edges= 221 
    #> 
    #>  Vertex attribute names: 
    #>     account_created_at account_lang description favourites_count followers_count friends_count listed_count location node_class profile_url protected screen_name statuses_count timestamp_ms TWITTER_NAME url verified vertex.names 
    #> 
    #>  Edge attribute names: 
    #>     action lang source status_id text

``` r
as_sna_network(sna_data)
```

    #>  Network attributes:
    #>   vertices = 100 
    #>   directed = TRUE 
    #>   hyper = FALSE 
    #>   loops = TRUE 
    #>   multiple = TRUE 
    #>   bipartite = FALSE 
    #>   total edges= 221 
    #>     missing edges= 0 
    #>     non-missing edges= 221 
    #> 
    #>  Vertex attribute names: 
    #>     account_created_at account_lang description favourites_count followers_count friends_count listed_count location node_class profile_url protected screen_name statuses_count timestamp_ms TWITTER_NAME url verified vertex.names 
    #> 
    #>  Edge attribute names: 
    #>     action lang source status_id text

<!-- # `R CMD Check` -->

<!-- ```{r} -->

<!-- devtools::check(quiet = TRUE) -->

<!-- ``` -->

# Cite

``` r
citation("tweetgraph")
```

    #> 
    #> To cite tweetgraph use:
    #> 
    #>   Knapp, B. G. (2019). tweetgraph: Tweet Graph Structures for
    #>   Network Analysis Data. R package version 0.0.0.9 Retrieved from
    #>   https://knapply.github.io/tweetgraph
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{tweetgraph-package,
    #>     title = {tweetgraph: Tweet Graph Structures for Network Analysis},
    #>     author = {Brendan Knapp},
    #>     year = {2019},
    #>     note = {R package version 0.0.0.9},
    #>     url = {https://knapply.github.io/tweetgraph},
    #>   }
