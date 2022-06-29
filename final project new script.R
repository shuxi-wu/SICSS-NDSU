library(lubridate) #for extracting dates
library(tidyverse)
library(quanteda)
library(stm)
library(RColorBrewer)
library(wordcloud)

#prepare covariates - month
#elements <- str_split(covid$created_at, fixed(" "), n=2)
#tweets$Date <- map_chr(elements, 1)
#tweets$month <- month(as.POSIXlt(tweets$Date, format="%Y-%m-%d"))

#load covid dataset with tag and cleaned retweet
covid = read.csv("covid_retweet_cleaned.csv")
covid$tag[covid$overall > 0.5] <- 1
covid$tag[covid$overall <= 0.5] <- 0

covid <- covid%>%filter(lang == "en")

#clean tweets, you can tweak the codes below based on your research
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
    str_trim("both")
}
covid$text<-covid$text %>% clean_tweets
stopWords2<-c("covid","vaccine","vaccines","vaccination","coronavirus")

#tokenization
covidCorpus <- corpus(covid$text)
covidCorpus <- covidCorpus %>%
  tokens(what = "word") %>%
  tokens_remove(c(stopwords("english"), stopWords2))


#pass covariates to corpus, you may select other features
docvars(covidCorpus, field = "tag") <- covid$tag # covariance field "tag: bot or human"
#docvars(covidCorpus, field = "text") <- covid$text # covariance field "text"

#converting corpus to document frequency matrix
covidDfm <- dfm(covidCorpus) 
topfeatures(covidDfm, n = 100, scheme = "docfreq")

#prepare stm data
covid.stm <- convert(covidDfm, to = "stm")
out_covid <- prepDocuments(covid.stm$documents, 
                           covid.stm$vocab, 
                           covid.stm$meta, 
                           lower.thresh = 63)
#Topic modeling
#How do we decide how many topics?
#1.Statistical fit
#2.Interpretability and relevance of topics
#Let's find the optimal K, this will take a long time, be prepared k = c(3:15)means choosing three to ten topics
#set.seed(1234)
#storage_covid <- searchK(out_covid$documents, out_covid$vocab, K = c(3:15),
 #                        data=out_covid$meta)


model_10 <- stm(out_covid$documents, out_covid$vocab,
               K = 10, #waiting on the optimal k number
               max.em.its = 169, #its number should follow searchK output; this is the number of iterations wanted; may want a higher number if output says terminated before convergence reached
               data = out_covid$meta, 
               init.type = "Spectral")

labelTopics(model_10, c(1:10), frexweight = 0.5,n = 15)
labels <- labelTopics(model_10, n = 10) # this gets top 20 keywords, but you can get more 
topwords <- data.frame("features" = t(labels$frex))
colnames(topwords) <- paste("Topics", c(1:10))
topwords[1:10]
