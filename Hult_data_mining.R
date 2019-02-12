#### Prep Work ####
library(textreadr)
library(tidytext)
library(tidyr)
library(janeaustenr)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)
library(widyr)
library(pdftools)
library(tm)
library(quanteda)
library(RColorBrewer)
data(stop_words)

setwd("~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining")
nm <- list.files(path="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/PDF")

Team1 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team1.txt")
Team2 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team 2.txt")
Team3 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/team3.txt")
Team4 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/team4.txt")
Team5 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team5.txt")
Team7 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team 7.txt")
Team8 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team 8.txt")
Team9 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team9.txt")
Team10 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team10.txt")
Team11 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team11.txt")
Team12 <- read_document(file="~/Hult/MBAN/Mod B/Text_Analytics/Hult Alcoholics/Hult_data_mining/Team12.txt")

results_combo <- c(Team1, Team2, Team3, Team4, Team5,
                 Team7, Team8, Team9, Team10, Team11, Team12)

a <- 38
b <- 7
results_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    results_df[i,z]<- results_combo[i*b+z-b]
  }
}

#### Creating Strings ####
string_V1 <- results_df$V1
string_V1 <- substr(string_V1, start=11 , stop = 10000)
string_V2 <- results_df$V2
string_V2 <- substr(string_V2, start=11 , stop = 10000)
string_V3 <- results_df$V3
string_V3 <- substr(string_V3, start=11 , stop = 10000)
string_V4 <- results_df$V4
string_V4 <- substr(string_V4, start=11 , stop = 10000)
string_V5 <- results_df$V5
string_V5 <- substr(string_V5, start=11 , stop = 10000)
string_V6 <- results_df$V6
string_V6 <- substr(string_V6, start=11 , stop = 10000)
string_V7 <- results_df$V7
string_V7 <- substr(string_V7, start=11 , stop = 10000)

#### Cleaning the Strings ####
V1_df <- data_frame(line=1:a, text=string_V1)
print(V1_df)
V2_df <- data_frame(line=1:a, text=string_V2)
print(V2_df)
V3_df <- data_frame(line=1:a, text=string_V3)
print(V3_df)
V4_df <- data_frame(line=1:a, text=string_V4)
print(V4_df)
V5_df <- data_frame(line=1:a, text=string_V5)
print(V5_df)
V6_df <- data_frame(line=1:a, text=string_V6)
print(V6_df)
V7_df <- data_frame(line=1:a, text=string_V7)
print(V7_df)

#### Getting rid of Stop Words ####
frequencies_tokens_nostopV1 <- V1_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
 # count(word, sort=TRUE)
print(frequencies_tokens_nostopV1)

frequencies_tokens_nostopV2 <- V2_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostopV2)

frequencies_tokens_nostopV3 <- V3_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostopV3)

frequencies_tokens_nostopV4 <- V4_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostopV4)

frequencies_tokens_nostopV5 <- V5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostopV5)

frequencies_tokens_nostopV6 <- V6_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostopV6)

frequencies_tokens_nostopV7 <- V7_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)
print(frequencies_tokens_nostopV7)

#### Counts without Stop Words ####
counts_tokens_nostopV1 <- V1_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

counts_tokens_nostopV2 <- V2_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

counts_tokens_nostopV3 <- V3_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

counts_tokens_nostopV4 <- V4_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

counts_tokens_nostopV5 <- V5_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

counts_tokens_nostopV6 <- V6_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

counts_tokens_nostopV7 <- V7_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

#### Plotting the Frequencies ####
freq_histV1 <-frequencies_tokens_nostopV1 %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_histV1)


#frequencies_tokens_nostopV2 <- na.omit(frequencies_tokens_nostopV2)
#freq_histV2 <-frequencies_tokens_nostopV2 %>%
#  count(word, sort=TRUE) %>%
 # mutate(word = reorder(word,n )) %>%
  #ggplot(aes(word, n))+
  #geom_col()+
  #xlab(NULL)+
  #coord_flip()
#print(freq_histV2)

#### Word Cloud ####
frequencies_tokens_nostopV1 %>%
  with(wordcloud(word, line, max.words = 100))
frequencies_tokens_nostopV2 %>%
  with(wordcloud(word, n, max.words = 100))
frequencies_tokens_nostopV3 %>%
  with(wordcloud(word, n, max.words = 100))
frequencies_tokens_nostopV4 %>%
  with(wordcloud(word, n, max.words = 100))
frequencies_tokens_nostopV5 %>%
  with(wordcloud(word, n, max.words = 100))
frequencies_tokens_nostopV6 %>%
  with(wordcloud(word, n, max.words = 100))
frequencies_tokens_nostopV7 %>%
  with(wordcloud(word, n, max.words = 100))

#### Sentimate Analysis  V6 ####
#frequencies_tokens_nostopV6 %>%
#  inner_join(get_sentiments("nrc")) %>%
#  count(word, sentiment, sort=TRUE) %>%
#  acast(word ~sentiment, value.var="nn", fill=0) %>%
#  comparison.cloud(colors = c("grey20", "gray80"),
#                   max.words=100)

#frequencies_tokens_nostopV6 %>%
#  inner_join(get_sentiments("bing")) %>%
#  count(word, sentiment, sort=TRUE) %>%
#  acast(word ~sentiment, value.var="nn", fill=0) %>%
#  comparison.cloud(colors = c("grey20", "gray80"),
#                   max.words=100)

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") #what is your sentiment

frequencies_tokens_nostopV6 %>%
  inner_join(nrcsurprise)

afinnV6 <- frequencies_tokens_nostopV6 %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index=word) %>% #using integer division to define larger sections of text
  summarise(sentiment=sum(score)) %>%
  mutate(method="AFINN")

bing_and_nrcV6 <- bind_rows(
  frequencies_tokens_nostopV6%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  frequencies_tokens_nostopV6 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  #count(method, index=n, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

afinnV6 %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

bing_and_nrcV6 %>%
  ggplot(aes(word, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


bing_countsV6 <- frequencies_tokens_nostopV6 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_countsV6 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#### Sentiment Analysis V7 #####
#frequencies_tokens_nostopV7 %>%
#  inner_join(get_sentiments("nrc")) %>%
#  count(word, sentiment, sort=TRUE) %>%
#  acast(word ~sentiment, value.var="nn", fill=0) %>%
#  comparison.cloud(colors = c("grey20", "gray80"),
#                   max.words=100)

#frequencies_tokens_nostopV7 %>%
#  inner_join(get_sentiments("bing")) %>%
#  count(word, sentiment, sort=TRUE) %>%
#  acast(word ~sentiment, value.var="nn", fill=0) %>%
#  comparison.cloud(colors = c("grey20", "gray80"),
#                   max.words=100)

#nrcsurprise <- get_sentiments("nrc") %>%

frequencies_tokens_nostopV7 %>%
  inner_join(nrcsurprise)

afinnV7 <- frequencies_tokens_nostopV7 %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index=word) %>%
  summarise(sentiment=sum(score)) %>%
  mutate(method="AFINN")

bing_and_nrcV7 <- bind_rows(
  frequencies_tokens_nostopV7%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  frequencies_tokens_nostopV7 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  #count(method, index=n, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

afinnV7 %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

bing_and_nrcV7 %>%
  ggplot(aes(word, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")




#### Word Cloud and Contribution #####
test_df <-data.frame(V1_df, V2_df, V3_df, V4_df, V5_df, V6_df, V7_df)
test_df$line.1 <- NULL
test_df$line.2 <- NULL
test_df$line.3 <- NULL
test_df$line.4 <- NULL
test_df$line.5 <- NULL
test_df$line.6 <- NULL

tidy_alchocol <- test_df %>%
  unnest_tokens(word, text.5) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_alchocol %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_alchocol %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="nn", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

#### N-Grams ####
alchocol_bigrams <- test_df %>%
  unnest_tokens(bigram, text.5, token = "ngrams", n=2)

alchocol_bigrams %>%
  count(bigram, sort = TRUE)

alchocol_bigrams_separated <- alchocol_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

alchocol_bigrams_filtered <- alchocol_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

alchocol_bigram_counts <- alchocol_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

#### Bigram Network ####
alchocol_bigram_graph <- alchocol_bigram_counts %>%
  filter(n>0) %>%
  graph_from_data_frame()

ggraph(alchocol_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#### Pairwise Correlations ####
alchocol_tidy <- test_df %>%
  unnest_tokens(word, text.5) %>%
  filter(!word %in% stop_words$word)

alchocol_word_cors <- alchocol_tidy %>%
  group_by(word) %>%
  filter(n() >= 1) %>%
  pairwise_cor(word,text, sort=TRUE)

alchocol_word_cors %>%
  filter(item1 %in% c('alcohol', 'drink', 'fun')) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

#### Prediction ####
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))

msg.dfm <- dfm(corpus(opinions), tolower = TRUE)
msg.dfm <- dfm_trim(msg.dfm, min_count = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm, type = "tfidf")


msg.dfm.train<-msg.dfm[1:8,]
msg.dfm.test<-msg.dfm[9:10,]

NB_classifier <- textmodel_nb(msg.dfm.train, c(1,1,1,1,0,0,0,0))
NB_classifier
summary(NB_classifier)

pred <- predict(NB_classifier, msg.dfm.test)
pred