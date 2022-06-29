# bot human tweet final project

library(rtweet)
library(igraph)
library(readr)
library(tidyverse)
library(dplyr)
library(tidygraph)
library(lubridate) #for extracting dates
library(quanteda)
library(stm)
library(wordcloud)

covid_other = read.csv("total_nonverified.csv")
covid_other1 = covid_other %>% select()

covid_retweet_cleaned = 
  covid_other %>%
  filter(retweet_or_quote_screen_name != "")
#write.csv(covid_retweet_cleaned, file = "covid_retweet_cleaned.csv")

#SNA ====================
edge_df <-
  covid_other %>% 
  select(user_screen_name, retweet_or_quote_screen_name) %>% #Step 1
  filter(retweet_or_quote_screen_name != "") %>% #step 3
  rename(from = user_screen_name,
        to = retweet_or_quote_screen_name) #Step 4


nodes_df <- data.frame(name = unique(c(edge_df$from,edge_df$to)),
                       stringsAsFactors = F)
node_overall = covid_other %>%
  select(user_screen_name, overall) %>% 
  mutate(name = user_screen_name, overall_score = overall) %>%
  select(name, overall_score) %>%
  distinct(name, .keep_all = TRUE)

graph_covid <- tbl_graph(nodes = nodes_df,
                          edges = edge_df,
                          directed = T)

graph_covid <- graph_covid %>% 
  activate(nodes) %>% 
  #mutate(community = group_louvain()) %>% # clustering
  activate(edges) %>% 
  filter(!edge_is_loop())
graph_covid = simplify_and_colorize(graph_covid)

#in_degree <- degree(graph_covid, mode = "in")
#out_degree <- degree(graph_covid, mode = "out")
#bt_degree <- betweenness(graph_covid, directed = TRUE)
#nodes_df$bt = bt_degree
#as.data.frame(in_degree)%>%arrange(desc(in_degree))%>%tail(100)

# data about graphs ==================
degree<-degree(graph_covid)
as.data.frame(degree)%>%arrange(desc(degree))%>%head(10)

#top 10 nodes that receive mentions, replies and retweet
in_degree <- degree(graph_covid, mode = "in") #in
as.data.frame(in_degree)%>%arrange(desc(in_degree))%>%head(10)

#top 10 nodes that mention, reply and retweet others
out_degree <- degree(graph_covid, mode = "out")
as.data.frame(out_degree)%>%arrange(desc(out_degree))%>%head(10)

#top 10 nodes with closeness - spread information quickly
cl_degree <- closeness(graph_covid, mode = "out") # out: mentioning others
as.data.frame(cl_degree)%>%arrange(desc(cl_degree))%>%head(10)

#top 10 nodes with betweenness - bridge that links different communities
bt_degree <- betweenness(graph_covid, directed = TRUE)
as.data.frame(bt_degree)%>%arrange(desc(bt_degree))%>%head(10)

#top 10 nodes with eigenvector centrality - influence over others
eigen_degree <- eigen_centrality(graph_covid, directed = TRUE)$vector #eigen degree is a list, so add $vector!
as.data.frame(eigen_degree)%>%arrange(desc(eigen_degree))%>%head(10)

mean_distance(graph_covid) #average path length, mean steps to take to get to any person in the network
graph.density(graph_covid) #how connected this network is, meaningful if compared with random graph
diameter(graph_covid, weights = NA) #network size, degrees of separation
reciprocity(graph_covid) #ratio of reciprocated edges, do ppl mention each other back etc?
transitivity(graph_covid, type="global") #ratio of triad

nodes_df = nodes_df %>%
  cbind(as.data.frame(in_degree), as.data.frame(out_degree), 
        as.data.frame(cl_degree), as.data.frame(bt_degree)) 

mean(bt_degree)
max(bt_degree)

nodes_df = nodes_df %>% select(1, 3:6)
nodes_df1 = nodes_df %>% select(1, 3:5)
nodes_df2 = nodes_df1 %>% select(1, 4)
x = merge(nodes_df, node_overall)

nodes_df2 %>% arrange(desc(nodes_df2$bt_degree)) %>% head(10)
x %>% arrange(desc(x$in_degree)) %>% head(10)

# plots ===================


plot(graph_covid, 
     edge.color = "black", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 0.5, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = ifelse(in_degree >= 10, log(in_degree)*2, 0), vertex.shape = "circle",    #vertex attributes
     vertex.color = ifelse(node_overall$overall_score <= 0.5, "blue", "red"),vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA,#ifelse(in_degree > 300, nodes_df$name , NA),
     layout=layout_with_fr)

plot(graph_covid, 
     edge.color = "black", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 0.5, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = ifelse(out_degree >= 10, log(out_degree)*3, 0), vertex.shape = "circle",    #vertex attributes
     vertex.color = ifelse(node_overall$overall_score <= 0.5, "blue", "red"),vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA,#ifelse(in_degree > 300, nodes_df$name , NA),
     layout=layout_with_fr)

plot(graph_covid, 
     edge.color = "black", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 0.5, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = ifelse(bt_degree >= 10, log(bt_degree)*3, 0), vertex.shape = "circle",    #vertex attributes
     vertex.color = ifelse(node_overall$overall_score <= 0.5, "blue", "red"),vertex.frame.color = "white",#vertex attributes
     vertex.label = NA,#ifelse(in_degree > 300, nodes_df$name , NA),
     layout=layout_with_fr)

#as.data.frame(bt_degree)%>%arrange(desc(bt_degree))%>%head(10)
# topic modeling =================
y <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-="

clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove punctuation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove emoji
    str_remove_all('[:emoji:]')%>%
    # Remove any trailing whitespace around the text
    str_trim("both") #or whatever
    #str_replace_all(y, "[[:punct:]]", "")
}
covid_retweet_cleaned = covid_retweet_cleaned %>% 
  filter(lang == "en")
covid_retweet_cleaned$text<-covid_retweet_cleaned$text %>% clean_tweets


covid_retweet_cleaned$tag = ifelse(covid_retweet_cleaned$overall <= 0.5, 0, 1)
# human 0, bot 1

  

covidCorpus <- corpus(covid_retweet_cleaned$text)

stopWords2<-c("covid","vaccine","vaccines","vaccination","coronavirus")
#tokenization
covidCorpus <- covidCorpus %>%
  tokens(what = "word") %>%
  tokens_remove(c(stopwords("english"), stopWords2))


docvars(covidCorpus, field = "tag") <- covid_retweet_cleaned$tag
head(covidCorpus)

covidDfm <- dfm(covidCorpus) #%>%
  #dfm_trim(min_count = 4000, min_docfreq = 1000)

topfeatures(covidDfm, n = 100, scheme = "docfreq")
covid.stm <- convert(covidDfm, to = "stm")
out2 <- prepDocuments(covid.stm$documents, 
                     covid.stm$vocab, 
                     covid.stm$meta, 
                     lower.thresh = 5000) 

#didn't run search K - someone else is running it ==========

model_5 <- stm(out2$documents, out2$vocab,
             K = 5, #waiting on the optimal k number
             max.em.its = 15, #its number should follow searchK output; this is the number of iterations wanted; may want a higher number if output says terminated before convergence reached
             data = out2$meta, 
             init.type = "Spectral")

labelTopics(model_5, c(1:5), frexweight = 0.5,n = 15)
labels <- labelTopics(model_5, n = 10) # this gets top 20 keywords, but you can get more 
topwords <- data.frame("features" = t(labels$frex))
colnames(topwords) <- paste("Topics", c(1:5))
topwords[1:5]

par(bty="n",col="grey40",lwd=6)
plot(model, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Topic 1: ",
                     "Topic 2: ",
                     "Topic 3: ",
                     "Topic 4: ",
                     "Topic 5: "),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")


