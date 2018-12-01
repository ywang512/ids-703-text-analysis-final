library(rvest)
library(tidytext)
library(dplyr)
library(SnowballC)
library(stringr)
library(lubridate)
library(RCurl)


### depression army  -----  26 secs

start_time <- Sys.time()

page5 <-'http://www.depressionarmy.com'
da_page <- read_html(paste0(page5,"/ourblog"))

# All pages
da_all_pages <- c(paste0(page5, "/ourblog"))
da_older_page_link <- paste0(page5, html_attr(html_nodes(da_page,'.BlogList-pagination-link'), "href"))
da_all_pages <- c(da_all_pages, da_older_page_link)
next_page <- read_html(da_older_page_link)
current_navigator <- html_attr(html_nodes(next_page, '.BlogList-pagination-link'), "href")
while(length(current_navigator) == 2){
  da_older_page_link <- paste0(page5, current_navigator[2])
  da_all_pages <- c(da_all_pages, da_older_page_link)
  next_page <- read_html(da_older_page_link)
  current_navigator <- html_attr(html_nodes(next_page, '.BlogList-pagination-link'), "href")
}
# da_all_pages


da_text <- list()
for (j in seq_along(da_all_pages)){
  # All topics in one page
  current_page <- read_html(da_all_pages[j])
  da_threads <- html_nodes(current_page, '.BlogList-item-title')
  da_topics <- html_attr(da_threads,"href")

  # Each topic
  for(i in seq_along(da_topics)){
    da_page <- read_html(paste0(page5,da_topics[i]))
    #posts <- html_nodes(da_page, xpath='.sqs-block-html')
    
    post_title <- da_page %>% 
      html_nodes("h1.BlogItem-title") %>% 
      html_text
    
    post_date <- da_page %>% 
      html_nodes(xpath="//time[@class='Blog-meta-item Blog-meta-item--date']") %>% 
      html_text %>%
      mdy()
    
    post_text <- da_page %>% 
      html_nodes(xpath="//div[@data-layout-label='Post Body']") %>% 
      html_text 
    
    da_text[[(j-1)*20+i]] <- tibble(post_title, post_date, post_text)
    
  }
  # names(da_text) <- topics
}

end_time <- Sys.time()
print(end_time - start_time)

#================================Try scraping comments==========================

# test <- da_topics[20]
# test_page <- read_html("http://www.depressionarmy.com/ourblog/2017/4/7/depression-and-social-media")
# 
# post_comments <- test_page %>%
#   html_nodes("p")
# 
# post_comments <- test_page %>%
#   html_nodes(xpath = '//*[@id="comments-5b9a61922b6a288509f8546c"]')


da_text_df <- lapply(da_text,function(x) as.data.frame(x))
big_df <- do.call("rbind", da_text_df)

library(jsonlite)
output_json <- toJSON(big_df)
write_json(output_json, "depression_army.json")
write.csv(big_df, "depression_army.csv")

test <- read.csv("depression_army.csv")
View(test)
test2 <- fromJSON(unlist(fromJSON("depression_army.json")))
View(test2)

