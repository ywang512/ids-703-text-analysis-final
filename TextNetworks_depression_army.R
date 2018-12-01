library(tidyverse)
library(tidytext)
library(lubridate)
library(textnets)
library(SnowballC)



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

depression_army <- tbl_df(read_csv("dataset/depression_army.csv"))

length(unique(depression_army$X1)) # 81 users
depression_army$post_date <- ymd(depression_army$post_date)


count_date <- depression_army %>% 
  group_by(post_date) %>% 
  count(); count_date

## Post_Count ~ Date
post_count_date <- ggplot(depression_army, aes(post_date)) +
  geom_histogram(bins = 100) +
  labs(title = "Number of Posts on Date", x = "Post Date", y = "Post Counts"); post_count_date

## Time range of each author
user_time_range <- depression_army[, 1:3] %>% 
  group_by(X1) %>% 
  summarise(range_d = max(post_date) - min(post_date)) %>% 
  arrange(., desc(range_d)); user_time_range
mean(user_time_range$range_d)  # may be we could consider merge posts of the same user (depending on the computing time)


#===================== Preprocessing ===========================================================

tidy_da <- depression_army %>%
  dropNA(., "post_text") %>%
  tokenization(., "post_text", token = "word") %>%
  rem_stopwords(., df_stop = stop_words) %>%
  rem_numbers() %>%
  rem_whitespaces() %>%
  stemming()

top_n_words <- tidy_da %>% 
  count(word) %>%
  arrange(., desc(n)) %>% 
  top_n(100)

tidy_da_top <- tidy_da %>% 
  filter(word %in% top_n_words$word)

tidy_da_top$X1 <- as.character(tidy_da_top$X1)

#===================== TextNet: node as post ===================================================

# da_prepprd <- PrepText(tidy_da_sample, groupvar = "X1", textvar = "word",
#                        node_type = "groups", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)
# da_prepprd <- PrepText(tidy_da_sample, groupvar = "post_author", textvar = "word",
#                        node_type = "words", tokenizer = "words", pos = "nouns",
#                        remove_stop_words = FALSE, compound_nouns = FALSE)
## Preparing Texts
da_words <- PrepText(tidy_da_top, groupvar = "X1", textvar = "word",
                       node_type = "words", tokenizer = "words", pos = "nouns",
                       remove_stop_words = FALSE, compound_nouns = FALSE)
da_groups <- PrepText(tidy_da_top, groupvar = "X1", textvar = "word",
                       node_type = "groups", tokenizer = "words", pos = "nouns",
                       remove_stop_words = FALSE, compound_nouns = FALSE)


## Creating Text Networks
da_text_network_words <- CreateTextnet(da_words)
da_text_network_groups <- CreateTextnet(da_groups)


## Visualization
VisTextNet(da_text_network_words, alpha = 0.25, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(da_text_network_groups, alpha = 0.25, label_degree_cut = 0, betweenness = FALSE)

 
VisTextNet(da_text_network_words, alpha = 0.1, label_degree_cut = 0, betweenness = FALSE)
VisTextNet(da_text_network_groups, alpha = 0.07, label_degree_cut = 0, betweenness = FALSE)


# VisTextNetD3(da_text_network)

library(htmlwidgets)
vis1 <- VisTextNetD3(da_text_network_words,
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=FALSE,
                    charge=-30)
vis2 <- VisTextNetD3(da_text_network_groups,
                    height=1000,
                    width=1400,
                    bound=FALSE,
                    zoom=FALSE,
                    charge=-30)
saveWidget(vis1, "depression_army_words.html")
saveWidget(vis2, "depression_army_groups.html")


## Analyzing
da_communities <- TextCommunities(da_text_network)
head(da_communities)

top_words_modularity_classes <- InterpretText(da_text_network, da_prepprd)
head(top_words_modularity_classes, 10)

word_cluster <- top_words_modularity_classes %>% 
  arrange(modularity_class) %>% 
  View()


## Centrality Measures
text_centrality <- TextCentrality(da_text_network)
head(text_centrality)



# Inspection
# zero_sample <- text_centrality %>% 
#   add_column(user = row.names(.)) %>% 
#   filter(betweenness_centrality == 0) %>% 
#   pull(user)
# zero_text <- depression_army %>% 
#   filter(post_author %in% zero_sample) %>% 
#   select(post_text)
















