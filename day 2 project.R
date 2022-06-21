library(rvest)
library(tidyverse)
library(ggplot2)
library(rtweet)

url_data <- "https://pressgallery.house.gov/member-data/members-official-twitter-handles"
url_data %>% 
  read_html()  #read HTML into R
css_selector <- "#region-content" #using inspection tool
data<-url_data %>% 
  read_html() %>% 
  html_element(css = css_selector) %>% 
  html_table()

cleandata = data[2:443,]
cleandata = cleandata %>%
  rename(firstname = 'X1', lastname = 'X2', handle = 'X3', state_district = 'X4', party = 'X5')%>%
  mutate(handle = gsub('@', '', handle))

rep_tweets = search_tweets('from:RepAdams', n=100)

rep_tweets = as.data.frame(NULL)

for (x in cleandata$handle) {
  x1 = paste('from:', x, sep = '')
  tweets = as.data.frame(search_tweets(x1, n = 10))
  rep_tweets = rbind(rep_tweets, tweets)
}

rep_tweets = rep_tweets %>% rename(handle = 'screen_name')
newdata = left_join(cleandata, rep_tweets, by="handle")

newdata_dem = newdata %>% filter(party == 'D')
newdata_rep = newdata %>% filter(party == 'R')

wordcloud(newdata$text, min.freq = 100)
wordcloud(newdata_dem$text, min.freq = 20)
wordcloud(newdata_rep$text, min.freq = 20)