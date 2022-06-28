# bot human tweet final project

library(rtweet)
library(igraph)
library(readr)
library(tidyverse)
library(dplyr)
library(tidygraph)

covid_other = read.csv("total_nonverified.csv")

#covid_retweet_cleaned = 
#  covid_other %>%
#  filter(retweet_or_quote_screen_name != "")
#write.csv(covid_retweet_cleaned, file = "covid_retweet_cleaned.csv")

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

in_degree <- degree(graph_covid, mode = "in")
out_degree <- degree(graph_covid, mode = "out")

#as.data.frame(node_overall)%>%arrange(desc(node_overall))%>%head(10)

plot(graph_covid, 
     edge.color = "black", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 0.5, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = log(in_degree)*2, vertex.shape = "circle",    #vertex attributes
     vertex.color = ifelse(node_overall$overall_score <= 0.5, "blue", "red"),vertex.frame.color = "white",     #vertex attributes
     vertex.label = NA,#ifelse(in_degree > 300, nodes_df$name , NA),
     layout=layout_with_fr)



