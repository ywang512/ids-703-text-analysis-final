library(tidyverse)
library(tidytext)
library(lubridate)
library(textnets)
library(SnowballC)
library(jsonlite)
library(htmlwidgets)


# Pre-processing
# General functions

# Removing NA's in text DF
# df is your dataframe with text
# text_column is the name of your column that contains all text
dropNA <-function(df,text_column){
  df %>%
    drop_na(!!sym(text_column)) %>%
    return()
}
# Tokenization
# df is your dataframe with text
# token for default is word, 
# text_column is the name of your column that contains all text
tokenization <-function(df,text_column,token = "word"){
  df %>%
    unnest_tokens(!!sym(token),!!sym(text_column)) %>%
    return()
}

# Removing stopwords
# df is your dataframe with a token for each row
# df_stop dataframe with words you want to remove, 
# default is stop_words, you need to uplad it first
data("stop_words")
rem_stopwords <-function(df,df_stop = stop_words){
  df %>%
    anti_join(df_stop) %>%
    return()
}
# Removing numbers
# df is your dataframe with a token for each row
# token for default is word, 
rem_numbers <- function(df,token="word"){
  df %>%
    .[-grep("\\d",.[[token]]),] %>%
    return()
}
# Removing whitespaces
# df is your dataframe with a token for each row
# token for default is word, 
rem_whitespaces <-function(df,token="word"){
  df %>%
    mutate(!!sym(token) := gsub("\\s+","",.[[token]] ) ) %>%
    return()
}
# Stemming
# df is your dataframe with a token for each row
# token for default is word, 
stemming <- function(df,token = "word"){
  df %>%
    mutate_at(token, funs(wordStem((.), language="en"))) %>%
    return()
}


#===================== Exploration ============================================================

beyondblue <- read_csv("dataset/beyondblue.csv") %>% 
  select(post_author, post_date, post_text) %>% 
  mutate(data_source = "beyondblue") %>% 
  mutate(platform = "forums")
depression_army <- read_csv("dataset/depression_army.csv") %>%
  select(post_title, post_date, post_text) %>% 
  rename(post_author = post_title) %>% 
  mutate(data_source = "depression_army") %>% 
  mutate(platform = "blogs")
psychcentral <- read_csv("dataset/psychcentral_forum_2.csv") %>% 
  select(post_author, post_date, post_text) %>% 
  mutate(data_source = "psychcentral") %>% 
  mutate(platform = "forums")
straightdope <- read_csv("dataset/straightdope_forum.csv") %>% 
  select(post_author, post_date, post_text) %>% 
  mutate(data_source = "straightdope") %>% 
  mutate(platform = "forums")
ER0 <- read_csv("dataset/ER_0.csv") %>% 
  select(ID, DATE, TEXT) %>% 
  rename(post_author = ID, post_date = DATE, post_text = TEXT) %>% 
  mutate(data_source = "ER_0") %>% 
  mutate(platform = "reddit")
ER1 <- read_csv("dataset/ER_1.csv") %>% 
  select(ID, DATE, TEXT) %>% 
  rename(post_author = ID, post_date = DATE, post_text = TEXT) %>% 
  mutate(data_source = "ER_1") %>% 
  mutate(platform = "reddit")
tumblr <- read_csv("dataset/tumblr-data.csv") %>% 
  select(Author, Created_time, Content, Keyword) %>% 
  rename(post_author = Author, post_date = Created_time, post_text = Content) %>% 
  mutate(data_source = paste0("tumblr_", as.character(Keyword))) %>% 
  mutate(platform = "tumblr") %>% 
  select(post_author, post_date, post_text, data_source, platform)
tumblr$post_date <- as.Date(tumblr$post_date, origin =  "1970-01-01")
reddit_comment <- read_csv("dataset/SubredditDepression.csv") %>% 
  select(user, comm_date, comment) %>% 
  rename(post_author = user, post_date = comm_date, post_text = comment) %>% 
  mutate(data_source = "reddit_comment") %>% 
  mutate(platform = "reddit")
reddit_post <- read_csv("dataset/SubredditDepression.csv") %>% 
  select(author, post_date, post_text) %>% 
  rename(post_author = author) %>% 
  unique() %>% 
  mutate(data_source = "reddit_post") %>% 
  mutate(platform = "reddit")
twitter <- fromJSON("dataset/depression_tweets_all.json") %>% 
  tbl_df() %>% 
  select(screen_name, created_at, text) %>% 
  rename(post_author = screen_name, post_date = created_at, post_text = text) %>% 
  mutate(data_source = "twitter") %>% 
  mutate(platform = "twitter")


combined <- rbind(beyondblue, depression_army, psychcentral, straightdope, ER0, ER1, tumblr,
                  reddit_comment, reddit_post, twitter)

tumblr_d <- tumblr %>% 
  filter(data_source != "tumblr_Trending") %>% 
  mutate(data_source = "tumblr")
reddit_d <- rbind(ER1, reddit_comment, reddit_post) %>% 
  mutate(data_source = "reddit")
combined <- rbind(beyondblue, depression_army, psychcentral, tumblr_d, reddit_d, twitter)

# write.csv(combined, "combined_data.csv")

tidy_combined <- combined %>% 
  dropNA(., "post_text") %>%
  tokenization(., "post_text", token = "word") %>%
  rem_stopwords(., df_stop = stop_words) %>%
  rem_numbers() %>%
  rem_whitespaces() %>%
  stemming()

# ## top-n words for each source
top_n_words_combined <- tidy_combined %>%
  group_by(data_source) %>%
  count(word) %>%
  arrange(., desc(n)) %>% 
  slice(1:200) %>%
  ungroup()

tidy_combined_topnwords <- tidy_combined %>%
  filter(word %in% top_n_words_combined$word)

top_n_words_platform <- tidy_combined %>%
  group_by(platform) %>%
  count(word) %>%
  arrange(., desc(n)) %>% 
  slice(1:200) %>%
  ungroup()
top_words <- unique(top_n_words_platform$word)
temp <- c(1:7, 9:13, 15:16, 19, 21, 23, 25:27, 29, 31, 33, 34, 36, 37, 39, 44, 45, 48, 49, 52, 53, 55, 60:62,
          66, 68, 77, 80,82, 84, 88,  89:91, 93, 103:106, 112:113, 118:119, 125, 127, 129, 131, 133:135, 139, 
          142:146, 152, 155, 158:160, 164, 167, 204, 216, 221:223, 225, 227:228, 232, 238, 247, 255:256, 259, 
          265, 276, 278, 293, 323, 328, 348, 364, 385:386, 393, 400:402, 407, 410:411, 415, 422, 430, 432, 451,
          454, 456, 457)
top_n_words_platform <- top_words[temp]
tidy_platform_topnwords <- tidy_combined %>%
  filter(word %in% top_n_words_platform)

# tidy_platform_topnwords <- tidy_combined %>%
#   filter(word %in% top_n_words_platform$word)




#===================== TextNet: node as post ===================================================

# combined_prepprd <- PrepText(tidy_combined_sample, groupvar = "post_author", textvar = "word",
#                        node_type = "groups", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)

# combined_prepprd <- PrepText(tidy_combined_sample, groupvar = "post_author", textvar = "word",
#                        node_type = "words", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)

## Preparing Texts
combined_prepprd_groups <- PrepText(tidy_combined_topnwords, groupvar = "data_source", textvar = "word",
                                    node_type = "groups", tokenizer = "words", pos = "nouns",
                                    remove_stop_words = FALSE, compound_nouns = FALSE)

platform_prepprd_groups <- PrepText(tidy_platform_topnwords, groupvar = "platform", textvar = "word",
                                    node_type = "groups", tokenizer = "words", pos = "nouns",
                                    remove_stop_words = FALSE, compound_nouns = FALSE)

platform_prepprd_words <- PrepText(tidy_platform_topnwords, groupvar = "platform", textvar = "word",
                                    node_type = "words", tokenizer = "words", pos = "nouns",
                                    remove_stop_words = FALSE, compound_nouns = FALSE)

# combined_prepprd_words <- PrepText(tidy_combined_topnwords, groupvar = "data_source", textvar = "word",
#                        node_type = "words", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)

## Creating Text Networks
combined_text_network_groups <- CreateTextnet(combined_prepprd_groups)
platform_text_network_groups <- CreateTextnet(platform_prepprd_groups)
platform_text_network_words <- CreateTextnet(platform_prepprd_words)

## Visualization
VisTextNet(combined_text_network_groups, alpha = 0.25, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(platform_text_network_groups, alpha = 0.25, label_degree_cut = 0, betweenness = FALSE)
temp = VisTextNet(platform_text_network_words, alpha = 0.125, label_degree_cut = 0, betweenness = FALSE)

# VisTextNetD3(combined_text_network)
setwd("./plots")
vis1 <- VisTextNetD3(combined_text_network_groups,
                     height=1000,
                     width=1400,
                     bound=FALSE,
                     zoom=FALSE,
                     charge=-30)
vis2 <- VisTextNetD3(platform_text_network_groups,
                     height=1000,
                     width=1400,
                     bound=FALSE,
                     zoom=FALSE,
                     charge=-30)
vis3 <- VisTextNetD3(platform_text_network_words,
                     height=1000,
                     width=1400,
                     bound=FALSE,
                     zoom=FALSE,
                     charge=-30)
saveWidget(vis1, "combined_TextNetwork_groups.html")
saveWidget(vis2, "platform_TextNetwork_groups.html")
saveWidget(vis3, "platform_TextNetwork_words.html")
setwd("../")

## Analyzing
combined_communities <- TextCommunities(combined_text_network_groups) %>% 
  arrange(., modularity_class)
combined_communities
platform_communities <- TextCommunities(platform_text_network_groups) %>% 
  arrange(., modularity_class)
platform_communities


top_words_modularity_classes <- InterpretText(combined_text_network_groups, combined_prepprd_groups) %>% 
  arrange(., modularity_class)
head(top_words_modularity_classes, 10)

word_cluster <- top_words_modularity_classes[!duplicated(top_words_modularity_classes$lemma), ] %>% 
  arrange(. ,desc(modularity_class)) %>% 
  View()


## Centrality Measures
text_centrality <- TextCentrality(combined_text_network)
head(text_centrality)



# Inspection
# zero_sample <- text_centrality %>% 
#   add_column(user = row.names(.)) %>% 
#   filter(betweenness_centrality == 0) %>% 
#   pull(user)
# zero_text <- beyondblue %>% 
#   filter(post_author %in% zero_sample) %>% 
#   select(post_text)




















