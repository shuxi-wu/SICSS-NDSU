# Day 3 project

#load("covid.csv")

library(quanteda)
library(devtools)
library(tidyverse)
library(quanteda.corpora)
library(quanteda.sentiment)
library(quanteda.textstats)
library(quanteda.textplots)
library(spacyr)

covid = covid %>% arrange(desc(overall))
covid1 = head(covid, 1000) 

covid2 = tail(covid, 1000)


corp_1 <- corpus(covid1)
corp_2 <- corpus(covid2)


corp_1$tag <- "H"
corp_2$tag <- "T"

clean_tokens1 <- tokens(
  corp_1,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

clean_tokens2 <- tokens(
  corp_2,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

data_freq_matrix1 <- dfm(clean_tokens1,
                        tolower=TRUE,
                        stem=TRUE,
                       remove= stopwords('english'))
data_freq_matrix1 = dfm_remove(data_freq_matrix1, c(stopwords('english'), 'rt'))
dfm_wordstem(data_freq_matrix1, language = quanteda_options("language_stemmer"))


data_freq_matrix2 <- dfm(clean_tokens2,
                        tolower=TRUE,
                        stem=TRUE,
                        remove=stopwords('english'))
data_freq_matrix2 = dfm_remove(data_freq_matrix2, c(stopwords('english'), 'rt'))
dfm_wordstem(data_freq_matrix2, language = quanteda_options("language_stemmer"))


features1 <- textstat_frequency(data_freq_matrix1, n=30)
features2 <- textstat_frequency(data_freq_matrix2, n=30)
features1$feature <- with(features1, reorder(feature, -frequency))
features2$feature <- with(features2, reorder(feature, -frequency))

ggplot(features1, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Word frequency for the most human-like tweets")

ggplot(features2, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Word frequency for the most bot-like tweets")



