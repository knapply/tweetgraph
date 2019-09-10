
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

library(rtweet)
hashtag_rstats <- search_tweets("#rstats")
```

## Knowledge Graph

``` r
kg <- as_knowledge_graph(hashtag_rstats)
kg
```

    #> IGRAPH 6682ee0 DN-- 304 925 -- 
    #> + attr: timestamp_ms (v/n), name (v/c), created_at (v/n), text
    #> | (v/c), source (v/c), lang (v/c), node_class (v/c), TWITTER_NAME
    #> | (v/c), screen_name (v/c), location (v/c), description (v/c), url
    #> | (v/c), protected (v/l), followers_count (v/n), friends_count
    #> | (v/n), listed_count (v/n), statuses_count (v/n),
    #> | favourites_count (v/n), account_created_at (v/n), verified
    #> | (v/l), profile_url (v/c), account_lang (v/l), time (e/n),
    #> | source_class (e/c), action (e/c), target_class (e/c)
    #> + edges from 6682ee0 (vertex names):
    #> [1] 1171386846666010624->1171396055512080384
    #> + ... omitted several edges

``` r
plot_vis_net(kg)
```

<img src="man/figures/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## Social Network

``` r
sn <- as_social_network(hashtag_rstats)
sn
```

    #> IGRAPH 6b1d76e DN-- 88 255 -- 
    #> + attr: name (v/c), timestamp_ms (v/n), TWITTER_NAME (v/c),
    #> | screen_name (v/c), location (v/c), description (v/c), url (v/c),
    #> | protected (v/l), followers_count (v/n), friends_count (v/n),
    #> | listed_count (v/n), statuses_count (v/n), favourites_count
    #> | (v/n), account_created_at (v/n), verified (v/l), profile_url
    #> | (v/c), account_lang (v/l), node_class (v/c), action (e/c),
    #> | status_id (e/c), text (e/c), source (e/c), lang (e/c)
    #> + edges from 6b1d76e (vertex names):
    #> [1] 821105693948264448->16741059   821105693948264448->2879642556
    #> [3] 821105693948264448->352684272  821105693948264448->16741059  
    #> + ... omitted several edges

``` r
plot_vis_net(sn)
```

<img src="man/figures/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

# So What?

<table class="table" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

var

</th>

<th style="text-align:right;">

n\_nodes

</th>

<th style="text-align:right;">

n\_edges

</th>

<th style="text-align:left;">

node\_types

</th>

<th style="text-align:left;">

node\_attrs

</th>

<th style="text-align:left;">

edge\_attrs

</th>

<th style="text-align:left;">

edge\_types

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

tweetgraph::as\_knowledge\_graph

</td>

<td style="text-align:right;">

304

</td>

<td style="text-align:right;">

925

</td>

<td style="text-align:left;">

c(“status”, “user”, “hashtag”, “media”, “url”)

</td>

<td style="text-align:left;">

c(“timestamp\_ms”, “name”, “created\_at”, “text”, “source”, “lang”,
“node\_class”, “TWITTER\_NAME”, “screen\_name”, “location”,
“description”, “url”, “protected”, “followers\_count”,
“friends\_count”, “listed\_count”, “statuses\_count”,
“favourites\_count”, “account\_created\_at”, “verified”,
“profile\_url”, “account\_lang”)

</td>

<td style="text-align:left;">

c(“time”, “source\_class”, “action”, “target\_class”)

</td>

<td style="text-align:left;">

c(“was\_retweeted\_by”, “was\_replied\_to\_by”, “posts”, “contains”,
“mentions”)

</td>

</tr>

<tr>

<td style="text-align:left;">

tweetgraph::as\_social\_network

</td>

<td style="text-align:right;">

88

</td>

<td style="text-align:right;">

255

</td>

<td style="text-align:left;">

user

</td>

<td style="text-align:left;">

c(“name”, “timestamp\_ms”, “TWITTER\_NAME”, “screen\_name”, “location”,
“description”, “url”, “protected”, “followers\_count”,
“friends\_count”, “listed\_count”, “statuses\_count”,
“favourites\_count”, “account\_created\_at”, “verified”,
“profile\_url”, “account\_lang”, “node\_class”)

</td>

<td style="text-align:left;">

c(“action”, “status\_id”, “text”, “source”, “lang”)

</td>

<td style="text-align:left;">

c(“mentions”, “reply\_to”, “retweet”)

</td>

</tr>

</tbody>

</table>
