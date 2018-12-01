library(rvest)
library(tidytext)
library(dplyr)
library(SnowballC)


### Beyond Blue
page <-'https://www.beyondblue.org.au/get-support/online-forums/'

## topics
beyondblue_page <- read_html(paste0(page, "depression"))

## threads
path <- '.sfforumThreadTitle'
bb_threads <- html_nodes(beyondblue_page, path)
topics <- html_attr(bb_threads, "href")  # one page with 50 threads
topic <- topics[1]

## posts and replies
beyondblue_posts_page <- read_html(paste0(page, topic))
posts_pager_path <- ".inlineItems .sf_pagerNumeric"
bb_posts <- html_node(beyondblue_posts_page, posts_pager_path)
all_posts_pages <- html_attr(html_children(bb_posts), "href")[-1] # the first is always NA

## one post
# user
users_path <- '.sfforumUser'
bb_users <- html_nodes(beyondblue_posts_page, users_path)
html_text(bb_users)
bb_text[[i]] <- html_text(bb_posts) %>% 
  tolower() %>%
  gsub("(\\n)|(\\s{2,})","",.) %>%
  gsub("(.*underwood)|(.*reply)","",.) %>%
  gsub("[a-z]\\.[a-z]","\\1 \\3",.)
  
posts_path <- '.sfforumThreadPostContentWrp'
bb_posts <- html_nodes(beyondblue_posts_page, posts_path)

## topics pager
test <- html_node(beyondblue_page, xpath = '//*[@id="MainContentPlaceholder_C006_forumsFrontendThreadsList_ctl00_ctl00_wsPager_myPager_ctl00_ctl00_cmdLast"]')
final_thread_page <- html_attr(test, "href")



# bb_text <-list()
# for(i in seq_along(topics)){
#   beyondblue_page <- read_html(paste0(page,topics[i]))
#   patht <- '.sfforumThreadPostContentWrp'
#   bb_posts <- html_nodes(beyondblue_page,patht)
#   bb_text[[i]] <- html_text(bb_posts) %>% 
#     tolower() %>%
#     gsub("(\\n)|(\\s{2,})","",.) %>%
#     gsub("(.*underwood)|(.*reply)","",.) %>%
#     gsub("[a-z]\\.[a-z]","\\1 \\3",.)
# }
names(bb_text) <- topics

data(stop_words)

bb_tidy <- bb_text %>%
  unlist() %>%
  data_frame(text = .) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

bb_stemm <- bb_tidy %>%
  mutate_at("word", funs(wordStem((.), language="en")))

bb_DTM<- bb_tidy %>%
  cast_dtm("word")

head(posts_bb)
posts_bb <- html_nodes(beyondblue_page,xpath='//*[@id="MainContentPlaceholder_C006_forumsFrontendPostsList"]/ol')

head(section_of_wiki)
health_rankings <- html_table(posts_bb)
head(health_rankings[,(1:2)])






### psychcentral
page2 <-'https://forums.psychcentral.com/'
psychcentral_page <- read_html(paste0(page2,"anxiety-panic-and-phobias"))
path2 <- '#threadbits_forum_9 a'
pc_threads <- html_nodes(psychcentral_page,path2)
pc_topics <- html_attr(pc_threads,"href") %>% 
  grep("anxiety-panic-and-phobias",.,value=TRUE) %>%
  grep("top-anxiety-resources-online",.,value=TRUE,invert=TRUE) %>%
  subset(.,1:length(.) %% 2 ==1)

pc_text <-list()
for(i in seq_along(pc_topics)){
  pc_page <- read_html(pc_topics[i])
  patht <- '#posts hr+ div'
  pc_posts <- html_nodes(pc_page,patht)
  pc_text[[i]] <- html_text(pc_posts) %>% 
    tolower() %>%
    gsub("(\\n)|(\\s{2,})","",.) %>%
    gsub("^.* posted by [a-z]* ","",.)
}
names(pc_text) <- topics


### sane foums
page3 <-'https://saneforums.org/'
sane_page <- read_html(paste0(page3,"t5/Something-s-not-right/bd-p/le-forum-0004"))
path3 <- '.page-link'
sane_threads <- html_nodes(sane_page,path3)
sane_topics <- html_attr(sane_threads,"href") 


sane_text <-list()
for(i in seq_along(sane_topics)){
  sane_page <- read_html(paste0(page3,sane_topics[i]))
  patht <- '.lia-message-body-content'
  sane_posts <- html_nodes(sane_page,patht)
  sane_text[[i]] <- html_text(sane_posts) %>% 
    tolower() %>%
    gsub("(\\n)|(\\s{2,})","",.) 
}
names(sane_text) <- topics

### time to change
page4 <-'https://www.time-to-change.org.uk/'
change_page <- read_html(paste0(page4,"category/blog/depression"))
path4 <- '.node-header'
change_threads <- html_nodes(change_page,path4)
change_topics <- html_name(change_threads) #stuck here


sane_text <-list()
for(i in seq_along(sane_topics)){
  sane_page <- read_html(paste0(page3,sane_topics[i]))
  patht <- '.lia-message-body-content'
  sane_posts <- html_nodes(sane_page,patht)
  sane_text[[i]] <- html_text(sane_posts) %>% 
    tolower() %>%
    gsub("(\\n)|(\\s{2,})","",.) 
}
names(sane_text) <- topics


### depression army
page5 <-'http://www.depressionarmy.com'
da_page <- read_html(page0(page5,"/ourblog"))
path5 <- '.BlogList-item-title'
da_threads <- html_nodes(da_page,path5)
da_topics <- html_attr(da_threads,"href")  


da_text <-list()
for(i in seq_along(da_topics)){
  da_page <- read_html(paste0(page5,da_topics[i]))
  patht <- '.sqs-block-html'
  da_posts <- html_nodes(da_page,patht)
  da_text[[i]] <- html_text(da_posts) %>% 
    .[1:(length(.)-1)] %>%
    tolower() %>%
    gsub("(\\n)|(\\s{2,})","",.) 
}
names(da_text) <- topics


G3HP3N
