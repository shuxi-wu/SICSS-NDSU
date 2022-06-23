library(rtweet)
library(igraph)
library(readr)
bearer_token <- read_file("bearer_token.txt")

fandom <- search_tweets(q = '"#bts" OR "#blackpink"',n = 500, include_rts = TRUE)
fandom$screen_name
fandom_tok = tokenize(fandom$screen_name)

full_followers = as.data.frame(NULL)
for (x in fandom1$fandom.screen_name[1:5]) {
  followers = get_followers(x, n = 10)
  full_followers = rbind(full_followers, followers)
  print(x)
}

fandom1 = data.frame(fandom$screen_name, fandom$followers_count)
fandom1 = fandom1 %>% arrange(desc(fandom1$fandom.followers_count))


fandom.net <- network_graph(fandom, c("retweet, quote, mention, reply"))

fandom.net <- simplify(fandom.net, remove.multiple = FALSE, remove.loop = TRUE) 

in_degree <- degree(fandom.net, mode = "in")
as.data.frame(in_degree)%>%arrange(desc(in_degree))%>%head(10)
out_degree <- degree(fandom.net, mode = "out")
as.data.frame(out_degree)%>%arrange(desc(out_degree))%>%head(10)
cl_degree <- closeness(fandom.net, mode = "out") # out: mentioning others
as.data.frame(cl_degree)%>%arrange(desc(cl_degree))%>%head(10)
bt_degree <- betweenness(fandom.net, directed = TRUE)
as.data.frame(bt_degree)%>%arrange(desc(bt_degree))%>%head(10)
eigen_degree <- eigen_centrality(fandom.net, directed = TRUE)$vector #eigen degree is a list, so add $vector!
as.data.frame(eigen_degree)%>%arrange(desc(eigen_degree))%>%head(10)


plot(fandom.net, 
     edge.color = "grey", edge.width = 1, edge.curved = 0, #edge attributes
     edge.arrow.mode = 1, edge.arrow.size = 0.2, #arrow attributes
     vertex.size = 4, vertex.shape = "circle",    #vertex attributes
     vertex.color = "darkblue", vertex.frame.color = "white",     #vertex attributes
     vertex.label = ifelse(in_degree > 50, fandom$screen_name, NA), #label
     layout=layout_with_fr)

plot(fandom.net, 
     edge.color = "grey", edge.width = 0.6, edge.curved = 0.4, #edge attributes
     edge.arrow.mode=1, edge.arrow.size = 0.1,#arrow attributes
     vertex.size = log(in_degree+8)*1.5, vertex.shape = "circle",    #vertex attributes
     vertex.color = "skyblue", vertex.frame.color = "white",     #vertex attributes
     vertex.label = ifelse(in_degree > 30, fandom$screen_name, NA), #only show top nodes
     vertex.label.color = "black", 
     vertex.label.cex = 0.6, vertex.label.degree = -pi/2,
     layout=layout_with_fr)

cluster_walktrap(fandom.net, steps = 11)
fandom.comm<-cluster_walktrap(fandom.net, steps = 11)
plot(fandom.comm, fandom.net, vertex.size=4, vertex.label=NA, 
     edge.arrow.mode=0, layout=layout_with_fr) 

fandom.net$community <- fandom.comm$membership #store membership 

#use the membership to replot
plot(fandom.net, 
     edge.color = "grey", edge.width = 0.5, edge.curved = 0, #edge attributes
     edge.arrow.mode = 1, edge.arrow.size = 0.1,#arrow attributes
     vertex.size = log(10+bt_degree), vertex.shape = "circle",    #vertex attributes
     vertex.color = membership(fandom.comm), vertex.frame.color = "white",     #vertex attributes
     vertex.label = ifelse(in_degree > 30, fandom$screen_name, NA), #label
     layout = layout_with_kk)
