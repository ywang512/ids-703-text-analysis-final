library(rvest)
library(tidytext)
library(dplyr)
library(SnowballC)
library(stringr)
library(lubridate)
library(RCurl)

start_time <- Sys.time()

### Beyond and Blues
page <-'https://www.beyondblue.org.au/get-support/online-forums/'
beyondblue_page <- read_html(paste0(page,"depression"))
pages_names <- html_nodes(beyondblue_page, 
                          xpath = '//*[@id="MainContentPlaceholder_C006_forumsFrontendThreadsList_ctl00_ctl00_wsPager_myPager_ctl00_ctl00_cmdLast"]')
final_page_n <- html_attr(pages_names, "href") %>%
  gsub("[^0-9]","",.) %>%
  as.numeric

bb_text <- list(); k <- 1; topics_11 <- c(); beyondblue_pagep <- paste0(page, "depression")
for(p in 1:final_page_n){
  beyondblue_pagep <- paste0(page,"depression/page/",p)
  bb_threads <- read_html(beyondblue_pagep) %>%
    html_nodes(.,'.sfforumThreadTitle')
  if(p==1){ 
    topics <- html_attr(bb_threads,"href") %>% 
      .[4:length(.)] %>%
      gsub("^..","",.)
    topics_names <- html_attr(bb_threads,"title")  %>% 
      .[4:length(.)]
  }else{
    topics <- html_attr(bb_threads,"href") %>%
      gsub("^..","",.)
    topics_names <- html_attr(bb_threads,"title") 
  }
  for(i in seq_along(topics)){
    posts_page <- paste0(page,"depression",topics[i]) %>%
      read_html(.)
    posts_pages_all <- posts_page %>%
      html_node(.,".inlineItems .sf_pagerNumeric") %>%
      html_children(.) %>%
      html_attr(., "href") %>%
      .[-1]
    n <- ifelse(length(posts_pages_all)==0, 1, length(posts_pages_all))
    
    if (topics_names[i] == "Here I am again!! Life is great - NOT !!"){
      n <- 1
    }
    # if (topics_names[i] == "If you could decribe your depression in one word , what would it be ?"){
    #   n <- 16
    # }
    
    if (n == 11){
      topics_11 <- c(topics_names[i], topics_11)
    }
    
    for(j in 1:n){
      print(paste0("Currently working on page ", p, ", topic ", i, ", pages scraped ", k))
      
      posts <- posts_page %>%
        html_nodes(".media")
      
      post_author <- posts %>% 
        html_nodes(".sfforumThreadPostUser .sfforumUser") %>% 
        html_text
      
      post_date <- posts %>% 
        html_nodes(".sfforumPostAge") %>% 
        html_text %>% 
        str_extract("\\d{1,2}?\\s\\w{3,9}?\\s\\d{4}?") %>% 
        dmy()
      
      post_text <- posts %>% 
        html_nodes(".sfContentBlock") %>% 
        html_text %>%
        gsub("[\r\n\t]"," ",.) 
      
      if (topics[i] == "/feeling-empty"){
        post_text <- post_text[-5]
      }
      if (topics[i] == "/had-enough-of-everything"){
        post_text <- post_text[-3]
      }

      bb_text[[k]] <- tibble(post_author, post_date, post_text, post_thread = topics_names[i])
      k <- k + 1
      
      if (j != n){
        bb_pagej <- posts_pages_all[j]
        posts_page <- read_html(bb_pagej)
      }
    }
  }
}


end_time <- Sys.time()
print(end_time - start_time)




bb_text_df <- lapply(bb_text, function(x) as.data.frame(x))
big_df <- do.call("rbind", bb_text_df)

library(jsonlite)
output_json <- toJSON(big_df)
write_json(output_json, "beyondblue.json")
write.csv(big_df, "beyondblue.csv")

test <- read.csv("beyondblue.csv")
View(test)
test2 <- fromJSON(unlist(fromJSON("beyondblue.json")))
View(test2)




# bb_tidy <- bb_text %>%
#   unlist() %>%
#   data_frame(text = .) %>%
#   unnest_tokens(word,text) %>%
#   anti_join(stop_words) %>%
#   count(word, sort = TRUE)
# 
# bb_stemm <- bb_tidy %>%
#   mutate_at("word", funs(wordStem((.), language="en")))
# 
# bb_DTM <- bb_tidy %>%
#   cast_dtm("word")
# 
# head(posts_bb)
# posts_bb <- html_nodes(beyondblue_page,xpath='//*[@id="MainContentPlaceholder_C006_forumsFrontendPostsList"]/ol')
# 
# head(section_of_wiki)
# health_rankings <- html_table(posts_bb)
# head(health_rankings[,(1:2)])





