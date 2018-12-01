library(tidyverse)
library(tidytext)
library(lubridate)
library(textnets)
library(SnowballC)
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

psychcentral <- read_csv("dataset/psychcentral_forum_2.csv")

length(unique(psychcentral$post_author)) # 365 unique users
psychcentral$post_date <- ymd_hms(psychcentral$post_date)


count_date <- psychcentral %>% 
  group_by(post_date) %>% 
  count()

## Post_Count ~ Date
post_count_date <- ggplot(psychcentral, aes(post_date)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Posts on Date", x = "Post Date", y = "Post Counts"); post_count_date

## Time range of each author
user_time_range <- psychcentral %>% 
  group_by(post_author) %>% 
  summarise(range_d = max(post_date) - min(post_date)) %>% 
  arrange(., desc(range_d)); user_time_range
mean(user_time_range$range_d)  # may be we could consider merge posts of the same user (depending on the computing time)


#===================== Preprocessing ===========================================================
tidy_pc <- psychcentral %>%
  dropNA(., "post_text") %>%
  tokenization(., "post_text", token = "word") %>%
  rem_stopwords(., df_stop = stop_words) %>%
  rem_numbers() %>%
  rem_whitespaces() %>%
  stemming()

## top-n words
top_n_words <- tidy_pc %>% 
  count(word) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_pc_top_n_words <- tidy_pc %>% 
  filter(word %in% top_n_words$word)

top_n_authors <- tidy_pc %>%
  count(post_author) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_pc_top_n_authors <- tidy_pc %>% 
  filter(post_author %in% top_n_authors$post_author)



#===================== TextNet: node as post ===================================================
## Preparing Texts
pc_words <- PrepText(tidy_pc_top_n_words, groupvar = "post_author", textvar = "word",
                     node_type = "words", tokenizer = "words", pos = "nouns",
                     remove_stop_words = FALSE, compound_nouns = FALSE)
pc_groups <- PrepText(tidy_pc_top_n_authors, groupvar = "post_author", textvar = "word",
                      node_type = "groups", tokenizer = "words", pos = "nouns",
                      remove_stop_words = FALSE, compound_nouns = FALSE)


## Creating Text Networks
pc_TN_words <- CreateTextnet(pc_words)
pc_TN_groups <- CreateTextnet(pc_groups)


## Visualization
VisTextNet(pc_TN_words, alpha = 0.2, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(pc_TN_groups, alpha = 0.1, label_degree_cut = 0, betweenness = FALSE)


# VisTextNetD3(psych_text_network)

# library(htmlwidgets)
# vis <- VisTextNetD3(sotu_text_network, 
#                     height=1000,
#                     width=1400,
#                     bound=FALSE,
#                     zoom=FALSE,
#                     charge=-30)
# saveWidget(vis, "sotu_textnet.html")


## Analyzing
psych_communities <- TextCommunities(pc_TN_words)
head(psych_communities)

top_words_modularity_classes <- InterpretText(pc_TN_groups, pc_groups)
head(top_words_modularity_classes, 10)

word_cluster <- top_words_modularity_classes %>% 
  arrange(modularity_class)


## Centrality Measures
text_centrality <- TextCentrality(pc_TN_words)
text_centrality <- TextCentrality(pc_TN_groups)



# Inspection
# zero_sample <- text_centrality %>% 
#   add_column(user = row.names(.)) %>% 
#   filter(betweenness_centrality == 0) %>% 
#   pull(user)
# zero_text <- psychcentral %>% 
#   filter(post_author %in% zero_sample) %>% 
#   select(post_text)
















