How many packages are there on CRAN?
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
This shows one how to scrape data directly from a website 🕸 (an html table of CRAN packages, structure it as a dataframe and plot it).

> Loads packages 📦

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

> Plots it! 💹

``` r
df_cran %>%
  plot_cran_df +
  theme_economist()
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

> There you have it! R is growing exponentially!😄
