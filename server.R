#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
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

shinyServer(function(input, output) {
  
  #### Sentiment Wordclounds for V6 ####
  output$distPlot <- renderPlot({
    
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
    
    #### NRC Sentiments V6 ####
    tidy_alchocol %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~sentiment, value.var="nn", fill=0) %>%
      comparison.cloud(colors = c("grey20", "gray80"),
                       max.words=100)
  })
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
  
  output$ngram <- renderPlot({
    
    alchocol_bigram_graph <- alchocol_bigram_counts %>%
      filter(n>0) %>%
      graph_from_data_frame()
    
    ggraph(alchocol_bigram_graph, layout = "fr") +
      geom_edge_link()+
      geom_node_point()+
      geom_node_text(aes(label=name), vjust =1, hjust=1)
  })
  
  #### Pairwise Correlations ####
  alchocol_tidy <- test_df %>%
    unnest_tokens(word, text.5) %>%
    filter(!word %in% stop_words$word)
  
  alchocol_word_cors <- alchocol_tidy %>%
    group_by(word) %>%
    filter(n() >= 1) %>%
    pairwise_cor(word,text, sort=TRUE)
  
  output$correlation <- renderPlot({
    
    alchocol_word_cors %>%
      filter(item1 %in% c('alcohol')) %>%
      group_by(item1) %>%
      top_n(5) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation)) +
      geom_bar(stat = "identity")+
      facet_wrap(~item1, scales = "free")+
      coord_flip()
  })
  
  output$prediction <- renderPlot({
  
  pred <- predict(NB_classifier, msg.dfm.test)
  plot(pred)
  
  })
  
  output$sentiment <- renderPlot({
    
    afinnV6 %>%
      ggplot(aes(index, sentiment, fill=method))+
      geom_col(show.legend=FALSE)+
      facet_wrap(~method, ncol =1, scales= "free_y")
    
    bing_and_nrcV6 %>%
      ggplot(aes(word, sentiment, fill=method))+
      geom_col(show.legend=FALSE)+
      facet_wrap(~method, ncol =1, scales= "free_y")
    
    afinnV7 %>%
      ggplot(aes(index, sentiment, fill=method))+
      geom_col(show.legend=FALSE)+
      facet_wrap(~method, ncol =1, scales= "free_y")
    
    bing_and_nrcV7 %>%
      ggplot(aes(word, sentiment, fill=method))+
      geom_col(show.legend=FALSE)+
      facet_wrap(~method, ncol =1, scales= "free_y")
    
  })
  output$contribution <- renderPlot({
  
    bing_countsV6 <- frequencies_tokens_nostopV6 %>%
      inner_join(get_sentiments("bing")) %>%
      count(sentiment, word, sort=T) %>%
      ungroup()
    
    bing_countsV6 %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word=reorder(word, nn)) %>%
      ggplot(aes(word, sentiment, fill=nn)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y")+
      labs(y="Contribution to sentiment", x=NULL)+
      coord_flip()
  })
})