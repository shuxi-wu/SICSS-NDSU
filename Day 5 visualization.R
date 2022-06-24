# Data Visualization

library(tidyverse)
library(RColorBrewer)
library(maps)
library(ggnetwork)
library(gcookbook)
library(networkD3)
library(igraph)


ca <- read_csv("https://raw.githubusercontent.com/ScienceParkStudyGroup/r-lesson-based-on-ohi-data-training/gh-pages/data/ca.csv") 

p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors))
p
# Why does it show nothing?

p + geom_point()
p + geom_smooth()
p + geom_point() + 
  geom_smooth()
# check out https://r-graph-gallery.com/index.html or https://r-graph-gallery.com/ggplot2-package.html for more geoms


p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors/1000, # reduce visitors var by 1000 
                          color=park_name)) #color segregated by var! 

p + geom_smooth(show.legend = FALSE) + facet_wrap(~park_name) #can drop the legend if we're facet-wrapping
p + geom_boxplot() 


p <- ggplot(data=ca,
            mapping = aes(x=year,
                          y=visitors/1000,
                          color=park_name,
                          size=visitors/1000))

p + geom_point(alpha=0.4) + 
  ylab('Visitors') +
  xlab('Year') + # use xy labs for axes
  labs(color="Parks", size="Visitors (x1000)")+ #use labs for legend labs
#scale_color_hue(h=c(0,360), c=100, l=50) # Remember to use scale_fill_hue for shapes
#scale_color_manual(values = c('red', 'blue', 'green', 'purple', 'yellow', 'black', 'magenta', 'gray', 'green'))
scale_color_brewer(type="qual", palette = 8, aesthetics = "color") #how to increase the size of the legend?

# On to maps
map()
map('usa')
map('county')
map('state', regions="north dakota")
map('france')

# These functions draw a polygon representation of a map. They're nice, but we can't do much with them.
# Let's look inside:
map_data('county')
map_data('state', regions='north dakota')
midwest_counties <- subset(map_data('county'), region=="ohio"|
                             region=="indiana"|
                             region=="wisconsin"|
                             region=="illinois"|
                             region=="michigan")
#region=="north dakota")

# Now, let's fill them:
p <- ggplot(data=map_data('county'),
            mapping = aes(x=long, y=lat, group=group)) #Sets up the canvas

p + geom_polygon(fill='white', color='blue') + # Recreates in ggplot so now we can use the full grammar
  coord_map(projection = 'sinusoidal') + #tilts the map
  xlab('longitude') + 
  ylab('latitude')


# Let's make a choropleth map of poverty levels in the midwest
# This requires JOINING two datasets: our map polygons and data

View(midwest_counties)
View(midwest)

# we need to merge these two
# JOINS require a "key" (a column) that's the same in both datasets. But we don't have that...
# So let's make one:

midwest$subregion <- tolower(midwest$county)
midwest_merged <- left_join(midwest_counties, midwest) #You can specify merge key or let R figure it out

View(midwest_merged)

# Now we can go back to ggplot:

p <- ggplot(data=midwest_merged, aes(x=long, y=lat, group=group, fill=percollege)) #note we fill!

p + 
  geom_polygon(color="grey10") + 
  scale_fill_distiller(palette = "RdYlBu") +
  labs(fill = "% college education") +
  xlab("longitude") +
  ylab("latitude")

midwest_merged %>% arrange(desc(percollege)) %>% head(30)

# Networks
madmen_relations <- madmen

p <- simpleNetwork(madmen_relations) #uses networkd3 package
p
#saveWidget(p) lets you save this as a html file

# Or use igraph:
network <- graph_from_data_frame(madmen_relations, directed=F)
network #tweak directed variable
plot(network)



# Sankey
links <- madmen2
links$value <- 1 #sankey needs 3 variables: node1, node2, and value/weight of link
links <- links[links$Name1 < links$Name2, ]  # Removes reciprocal links, ie A-B/B-A count only once. Try without!


nodes <- data.frame(name=c(madmen2$Name1, madmen2$Name2)) %>% 
  unique()

#now, merge and collapse dataset 
links$IDName1 <- match(links$Name1, nodes$name)-1 # -1 bc Needs to be zero-indexed if want to use JS later. assigns a unique code to each unique datapoint
links$IDName2 <- match(links$Name2, nodes$name)-1

p <- sankeyNetwork(Links = links, 
                   Nodes = nodes, 
                   Source="IDName1", 
                   Target="IDName2", 
                   Value="value", 
                   NodeID="name",
                   sinksRight=TRUE) 
p
