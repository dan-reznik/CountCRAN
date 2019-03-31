How many packages are there on CRAN?
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
This shows one how to scrape data directly from a website 🕸 (an html table of CRAN packages, structure it as a dataframe and plot it).

> Loads R packages 📦

``` r
library(tidyverse)
library(ggthemes)
library(lubridate)
```

> Loads functionality to decode and plot CRAN data 💻

``` r
source("count_cran.R")
```

> Reads package table (as html) from CRAN and decode it into a dataframe: 👨‍💻

``` r
url_cran <- "http://cran.r-project.org/web/packages/available_packages_by_date.html"
df_cran <- read_file(url_cran) %>% cran_html_to_df
nrow(df_cran)
```

    ## [1] 11175

What's in it?

``` r
glimpse(df_cran)
```

    ## Observations: 11,175
    ## Variables: 3
    ## $ Date    <date> 2019-03-31, 2019-03-31, 2019-03-31, 2019-03-31, 2019-03…
    ## $ Package <chr> "AmigaFFH", "AzureGraph", "bnviewer", "bysykkel", "fastN…
    ## $ Title   <chr> "Commodore Amiga File Format Handler", "Simple Interface…

> What are the top 10 words used in package titles?

``` r
df_cran %>%
  get_top_words(10) %>%
  knitr::kable()
```

| word       |     n|
|:-----------|-----:|
| data       |  1584|
| analysis   |  1141|
| models     |   683|
| functions  |   446|
| tools      |   434|
| regression |   426|
| using      |   389|
| estimation |   364|
| model      |   354|
| interface  |   330|

> Plots it! 💹

``` r
df_cran %>%
  plot_cran_df +
  theme_economist()
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

> There you have it! R is growing exponentially!😄
