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

ER0 <- tbl_df(read_csv("dataset/ER_0.csv"))
ER1 <- tbl_df(read_csv("dataset/ER_1.csv"))

length(unique(ER0$ID)) # 1493 unique users
length(unique(ER1$ID)) # 214 unique users

#===================== Preprocessing ===========================================================

tidy_ER0 <- ER0 %>%
  dropNA(., "TEXT") %>%
  tokenization(., "TEXT", token = "word") %>%
  rem_stopwords(., df_stop = stop_words) %>%
  rem_numbers() %>%
  rem_whitespaces() %>%
  stemming()
tidy_ER1 <- ER1 %>%
  dropNA(., "TEXT") %>%
  tokenization(., "TEXT", token = "word") %>%
  rem_stopwords(., df_stop = stop_words) %>%
  rem_numbers() %>%
  rem_whitespaces() %>%
  stemming()

## top-n words
top_n_words0 <- tidy_ER0 %>% 
  count(word) %>% 
  filter(word != "http") %>% 
  filter(word != "https") %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
top_n_words1 <- tidy_ER1 %>% 
  count(word) %>% 
  filter(word != "http") %>% 
  filter(word != "https") %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_ER0_top_n_words <- tidy_ER0 %>% 
  filter(word %in% top_n_words0$word)
tidy_ER1_top_n_words <- tidy_ER1 %>% 
  filter(word %in% top_n_words1$word)

## top-n authors
top_n_authors0 <- tidy_ER0 %>% 
  count(ID) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
top_n_authors1 <- tidy_ER1 %>% 
  count(ID) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_ER0_top_n_authors <- tidy_ER0 %>% 
  filter(ID %in% top_n_authors0$ID)
tidy_ER1_top_n_authors <- tidy_ER1 %>% 
  filter(ID %in% top_n_authors1$ID)

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
ER0_groups <- PrepText(tidy_ER0_top_n_authors, groupvar = "ID", textvar = "word",
                       node_type = "groups", tokenizer = "words", pos = "nouns",
                       remove_stop_words = FALSE, compound_nouns = FALSE)
ER0_words <- PrepText(tidy_ER0_top_n_words, groupvar = "ID", textvar = "word",
                      node_type = "words", tokenizer = "words", pos = "nouns",
                      remove_stop_words = FALSE, compound_nouns = FALSE)

ER1_groups <- PrepText(tidy_ER1_top_n_authors, groupvar = "ID", textvar = "word",
                       node_type = "groups", tokenizer = "words", pos = "nouns",
                       remove_stop_words = FALSE, compound_nouns = FALSE)
ER1_words <- PrepText(tidy_ER1_top_n_words, groupvar = "ID", textvar = "word",
                      node_type = "words", tokenizer = "words", pos = "nouns",
                      remove_stop_words = FALSE, compound_nouns = FALSE)


## Creating Text Networks
ER0_text_network_groups <- CreateTextnet(ER0_groups)
ER0_text_network_words <- CreateTextnet(ER0_words)
ER1_text_network_groups <- CreateTextnet(ER1_groups)
ER1_text_network_words <- CreateTextnet(ER1_words)



## Visualization
VisTextNet(ER0_text_network_groups, alpha = 0.2, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(ER1_text_network_groups, alpha = 0.22, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(ER0_text_network_words, alpha = 0.21, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(ER1_text_network_words, alpha = 0.2, label_degree_cut = 0, betweenness = FALSE)



# VisTextNetD3(bb_text_network)
vis1 <- VisTextNetD3(ER1_text_network_words,
                     height=1000,
                     width=1400,
                     bound=FALSE,
                     zoom=FALSE,
                     charge=-30)
vis2 <- VisTextNetD3(ER0_text_network_words,
                     height=1000,
                     width=1400,
                     bound=FALSE,
                     zoom=FALSE,
                     charge=-30)
saveWidget(vis1, "ER1_words.html")
saveWidget(vis2, "ER0_words.html")



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










