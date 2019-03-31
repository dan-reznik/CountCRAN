# <tr> <th> Date </th> <th> Package </th> <th> Title </th> </tr>\n<tr> <td> 2019-03-26 </td> <td> <a href=\"../../web/packages/BIOMASS/index.html\">BIOMASS</a> </td> <td> Estimating Aboveground Biomass and Its Uncertainty in Tropical\nForests </td> </tr>

cran_html_to_df <- function(file_html) {
  file_csv <- file_html %>%
    str_extract_all("<tr>.*?</tr>") %>%
    first %>%
    str_remove_all("</?tr>") %>%
    str_remove_all("</t[hd]>") %>%
    str_squish() %>%
    str_remove_all("^<t[hd]> ") %>%
    str_remove_all("<a href=.*?>") %>%
    str_remove_all("</a>") %>%
    str_remove_all(fixed('"')) %>%
    str_replace_all(" <t[dh]> ","|")
  file_csv %>% read_delim(delim=fixed("|"))
}

plot_cran_df <- function(df_cran,brks=1000) {
  df_cran2 <- df_cran %>%
    mutate(ym=sprintf("%s-%02d-01",year(Date),month(Date))) %>%
    count(ym) %>%
    arrange(ym) %>%
    mutate(ym=ymd(ym),
           total=cumsum(n))
  total_max <- max(df_cran2$total)%>%as.integer
  total_ceil <- ceiling(total_max/brks)*brks
  df_cran2 %>%
    ggplot(aes(ym,total)) +
    geom_line(color="blue") + #+geom_smooth()
    scale_x_date(date_breaks="1 year",date_labels = "%m/%y") +
    scale_y_continuous(breaks=seq(0,total_ceil,brks),
                       minor_breaks=seq(0,total_ceil,brks)) +
    labs(title=sprintf("%d CRAN packages",total_max),
         subtitle=today()%>%as.character)
}

stop_words <- c("for","and","the","with","from","using")

get_top_words <- function(df,how_many) df %>%
  pull(Title) %>%
  str_squish() %>%
  str_to_lower() %>%
  str_remove_all("[^[:alpha:] ]") %>%
  str_split(" ") %>%
  unlist %>%
  keep(~str_length(.x)>2&!(.x%in%stop_words)) %>%
  tibble(word=.) %>%
  count(word,sort=T) %>%
  head(how_many)
