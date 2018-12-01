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

straight_dope <- tbl_df(read_csv("dataset/straightdope_forum.csv")) %>% 
  mutate(post_date = ymd_hms(post_date))

length(unique(straight_dope$post_author)) # 3944 unique users
# straight_dope$post_date <- ymd_hms(straight_dope$post_date)


# count_date <- straight_dope %>% 
#   group_by(post_date) %>% 
#   count(); count_date

## Post_Count ~ Date
post_count_date <- ggplot(straight_dope, aes(post_date)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Posts on Date", x = "Post Date", y = "Post Counts"); post_count_date

## Time range of each author
user_time_range <- straight_dope %>% 
  group_by(post_author) %>% 
  summarise(range_d = max(post_date) - min(post_date)) %>% 
  arrange(., desc(range_d)); user_time_range
mean(user_time_range$range_d)  # may be we could consider merge posts of the same user (depending on the computing time)

#===================== Preprocessing ===========================================================

tidy_sd <- straight_dope %>%
  dropNA(., "post_text") %>%
  tokenization(., "post_text", token = "word") %>%
  rem_stopwords(., df_stop = stop_words) %>%
  rem_numbers() %>%
  rem_whitespaces() %>%
  stemming()

## top-n words
top_n_words <- tidy_sd %>% 
  count(word) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_sd_top_n_words <- tidy_sd %>% 
  filter(word %in% top_n_words$word)

top_n_authors <- tidy_sd %>% 
  count(post_author) %>% 
  arrange(., desc(n)) %>% 
  top_n(100)
tidy_sd_top_n_authors <- tidy_sd %>%
  filter(post_author %in% top_n_authors$post_author)

# sam <- sample(nrow(tidy_sd_top_n_words), nrow(tidy_sd_top_n_words)/20)
# tidy_sd_top_n_words <- tidy_sd_top_n_words[sam, ]

#===================== TextNet: node as post ===================================================

# sd_prepprd <- PrepText(tidy_sd_sample, groupvar = "post_author", textvar = "word",
#                        node_type = "groups", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)

# sd_prepprd <- PrepText(tidy_sd_sample, groupvar = "post_author", textvar = "word",
#                        node_type = "words", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)

## Preparing Texts
sd_groups <- PrepText(tidy_sd_top_n_authors, groupvar = "post_author", textvar = "word",
                      node_type = "groups", tokenizer = "words", pos = "nouns",
                      remove_stop_words = FALSE, compound_nouns = FALSE)

sd_words <- PrepText(tidy_sd_top_n_words, groupvar = "post_author", textvar = "word",
                     node_type = "words", tokenizer = "words", pos = "nouns",
                     remove_stop_words = FALSE, compound_nouns = FALSE)


## Creating Text Networks
sd_text_network_groups <- CreateTextnet(sd_groups)
sd_text_network_words <- CreateTextnet(sd_words)


## Visualization
VisTextNet(sd_text_network_groups, alpha = 0.17, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(sd_text_network_words, alpha = 0.2, label_degree_cut = 0, betweenness = FALSE)


# VisTextNetD3(sd_text_network)
vis1 <- VisTextNetD3(sd_text_network_groups,
                     height=1000,
                     width=1400,
                     bound=FALSE,
                     zoom=FALSE,
                     charge=-30)
vis2 <- VisTextNetD3(sd_text_network_words,
                     height=1000,
                     width=1400,
                     bound=FALSE,
                     zoom=FALSE,
                     charge=-30)
saveWidget(vis1, "sd_groups.html")
saveWidget(vis2, "sd_words.html")



## Analyzing
sd_communities1 <- TextCommunities(sd_text_network_words) %>% 
  arrange(., modularity_class)
head(sd_communities1)

top_words_modularity_classes <- InterpretText(sd_text_network_words, sd_words) %>% 
  arrange(., modularity_class)
head(top_words_modularity_classes, 10)

word_cluster <- top_words_modularity_classes[!duplicated(top_words_modularity_classes$lemma), ] %>% 
  arrange(. ,desc(modularity_class)) %>% 
  View()


## Centrality Measures
text_centrality <- TextCentrality(sd_text_network_words)
head(text_centrality)



# Inspection
# zero_sample <- text_centrality %>% 
#   add_column(user = row.names(.)) %>% 
#   filter(betweenness_centrality == 0) %>% 
#   pull(user)
# zero_text <- straight_dope %>% 
#   filter(post_author %in% zero_sample) %>% 
#   select(post_text)





### adjacency matrix of co-appearance in a thread










