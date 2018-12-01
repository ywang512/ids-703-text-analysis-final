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

beyondblue <- tbl_df(read_csv("dataset/beyondblue.csv"))

length(unique(beyondblue$post_author)) # 3944 unique users
beyondblue$post_date <- ymd(beyondblue$post_date)


count_date <- beyondblue %>% 
  group_by(post_date) %>% 
  count(); count_date

## Post_Count ~ Date
post_count_date <- ggplot(beyondblue, aes(post_date)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Posts on Date", x = "Post Date", y = "Post Counts"); post_count_date

## Time range of each author
user_time_range <- beyondblue[, 2:3] %>% 
  group_by(post_author) %>% 
  summarise(range_d = max(post_date) - min(post_date)) %>% 
  arrange(., desc(range_d)); user_time_range
mean(user_time_range$range_d)  # may be we could consider merge posts of the same user (depending on the computing time)

#===================== Preprocessing ===========================================================

tidy_bb <- beyondblue %>%
  dropNA(., "post_text") %>%
  tokenization(., "post_text", token = "word") %>%
  rem_stopwords(., df_stop = stop_words) %>%
  rem_numbers() %>%
  rem_whitespaces() %>%
  stemming()

## top-n words
top_n_words <- tidy_bb %>% 
  count(word) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_bb_top_n_words <- tidy_bb %>% 
  filter(word %in% top_n_words$word)

top_n_authors <- tidy_bb %>%
  count(post_author) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_bb_top_n_authors <- tidy_bb %>% 
  filter(post_author %in% top_n_authors$post_author)
# sam <- sample(nrow(tidy_bb_top_n_words), nrow(tidy_bb_top_n_words)/20)
# tidy_bb_top_n_words <- tidy_bb_top_n_words[sam, ]

#===================== TextNet: node as post ===================================================

# bb_prepprd <- PrepText(tidy_bb_sample, groupvar = "post_author", textvar = "word",
#                        node_type = "groups", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)

# bb_prepprd <- PrepText(tidy_bb_sample, groupvar = "post_author", textvar = "word",
#                        node_type = "words", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)

## Preparing Texts
bb_groups <- PrepText(tidy_bb_top_n_authors, groupvar = "post_author", textvar = "word",
                       node_type = "groups", tokenizer = "words", pos = "nouns",
                       remove_stop_words = FALSE, compound_nouns = FALSE)

bb_words <- PrepText(tidy_bb_top_n_words, groupvar = "post_author", textvar = "word",
                       node_type = "words", tokenizer = "words", pos = "nouns",
                       remove_stop_words = FALSE, compound_nouns = FALSE)


## Creating Text Networks
bb_text_network_groups <- CreateTextnet(bb_groups)
bb_text_network_words <- CreateTextnet(bb_words)


## Visualization
VisTextNet(bb_text_network_groups, alpha = 0.25, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(bb_text_network_words, alpha = 0.25, label_degree_cut = 0, betweenness = FALSE)


# VisTextNetD3(bb_text_network)
vis1 <- VisTextNetD3(bb_text_network_groups,
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=FALSE,
                    charge=-30)
vis2 <- VisTextNetD3(bb_text_network_words,
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=FALSE,
                    charge=-30)
saveWidget(vis1, "bb_groups.html")
saveWidget(vis2, "bb_words.html")



## Analyzing
bb_communities <- TextCommunities(bb_text_network) %>% 
  arrange(., modularity_class)
head(bb_communities)

top_words_modularity_classes <- InterpretText(bb_text_network, bb_prepprd) %>% 
  arrange(., modularity_class)
head(top_words_modularity_classes, 10)

word_cluster <- top_words_modularity_classes[!duplicated(top_words_modularity_classes$lemma), ] %>% 
  arrange(. ,desc(modularity_class)) %>% 
  View()


## Centrality Measures
text_centrality <- TextCentrality(bb_text_network)
head(text_centrality)



# Inspection
# zero_sample <- text_centrality %>% 
#   add_column(user = row.names(.)) %>% 
#   filter(betweenness_centrality == 0) %>% 
#   pull(user)
# zero_text <- beyondblue %>% 
#   filter(post_author %in% zero_sample) %>% 
#   select(post_text)





### adjacency matrix of co-appearance in a thread










