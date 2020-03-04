library(xml2)
library(rvest)
library(tidyverse)


# URL <- "https://bjgp.org/content/70/692"
# 
# pg <- read_html(URL)
# 
# page_nodes <- html_nodes(pg, "span, div h2")
# 
# start <- which(html_text(page_nodes) == "Research")+1
# end <- which(html_text(page_nodes) == "Life & Times")-1
# 
# page_nodes <- page_nodes[start:end] 
# page_nodes_attrs <- page_nodes %>% html_attrs()


## interesting function from https://gist.github.com/paulrougieux/e1ee769577b40cd9ed9db7f75e9a2cc2, modified slightly to include h2 div headers for referencing which links are related to research. Compare against my own stuff from above.

scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a, div h2") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a, div h2") %>%
    rvest::html_text()
  
  merge <- tibble(link = link_, url = url_)
  
  start <- which(merge$link == "Research" & is.na(merge$url))+1
  end <- which(merge$link == "Life & Times"| merge$link == "Out of Hours" & is.na(merge$url))-1

  merge <- merge[start:end,]
  merge$url <- paste0("https://bjgp.org", merge$url)
  
  return(merge)
}

use_this <-
  tibble(issue_no = 630:692) %>% mutate(
    volume_no = case_when(
      issue_no %in% 630:641 ~ 65,
      issue_no %in% 642:653 ~ 66,
      issue_no %in% 654:665 ~ 67,
      issue_no %in% 666:677 ~ 68,
      issue_no %in% 678:689 ~ 69,
      issue_no %in% 690:692 ~ 70
    )
  )

zoom <-
  map2_dfr(use_this$issue_no %>% set_names(nm = as.character(use_this$issue_no)), use_this$volume_no, function(issue_no, volume_no) {
    scraplinks(paste0("https://bjgp.org/content/", volume_no, "/", issue_no, "/"))
  }, .id = "issue_no")

relevant_dates <- map_dfr(zoom$url, ~ {
  webpage <- xml2::read_html(.x)
  # Extract the URLs
  text <- webpage %>%
    rvest::html_nodes("div ul") %>%
    rvest::html_text()
  text <- keep(text, function(x) str_detect(x, "Received"))
  text <- text %>% str_split_fixed("\\.", 3) %>% as_tibble()
  return(text)
})
