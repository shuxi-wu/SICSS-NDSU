# Day 3 NLP 

#install.packages('quanteda')
#install.packages('devtools')
#install.packages('spacyr')
#devtools::install_github("quanteda/quanteda.corpora", force = TRUE)
#devtools::install_github("quanteda/quanteda.sentiment", force = TRUE)
#devtools::install_github("quanteda/quanteda.textplots", force = TRUE)
#devtools::install_github("quanteda/quanteda.textstats", force = TRUE)
# or use install.packages('quanteda.corpora') etc
 

library(quanteda)
library(devtools)
library(tidyverse)
library(quanteda.corpora)
library(quanteda.sentiment)
library(quanteda.textstats)
library(quanteda.textplots)
library(spacyr)

# Quanteda can import a variety of text formats directly, 
#from .txt files to JSON from the Twitter API, 
#using the `corpus` command. (i.e. you can read anything as a corpus)
# Here, we'll use a built-in corpus of all inaugural addresses by U.S. presidents.

inaugural <- data_corpus_inaugural
#tweets <- read_parquet('/my_path/control_full_03.parquet')
#tweets <- corpus(tweets$text)

# Let's take a look inside:
View(summary(inaugural))
#corpora can be formatted differently, but it's just a collection of docs
#the summary(corpus) shows how the docs have been broken down/processed
#View(summary(tweets))

# ... and create tokens:
# Q: what are tokens in NLP?
tokens <- tokens(
  inaugural)
#this then separates the raw corpus into a raw collection of tokens 

clean_tokens <- tokens(
  inaugural,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)
#docvars: metadata of each document, e.g. name of president, party etc.;
# can drop them if there're too many

clean_tokens #Coding is like cooking. Always taste at every step

# Create word frequency matrix; try with and without stopwords
data_freq_matrix <- dfm(clean_tokens,
                        tolower=TRUE,
                        stem=TRUE,
                        remove=stopwords('english'))
dfm_remove(data_freq_matrix, stopwords('english'))
dfm_wordstem(data_freq_matrix, language = quanteda_options("language_stemmer"))
# stem: reduce all versions of a word to the root, e.g. consult, consultant,
# consultations, consulting...all is "consult"
# stopwords('english') is a dictionary; can create your own
# sparse: means how much of it is just zeros; can make less sparse by 
# trimming (set min and max frequency)

data_freq_matrix

# Less sparse
data_freq_matrix <- dfm_trim(data_freq_matrix,
                             min_docfreq = 0.075,
                             max_docfreq = 0.9,
                             docfreq_type = "prop"
) 
# if a word appears in less than 7.5% and more than 90% of address, trim it
# reduces sparseness! but min/max is a judgement call

data_freq_matrix

# Visualize keyword in context
subset <- corpus_subset(inaugural, Year > 1990)
kwic(subset, pattern="war") %>% textplot_xray()
#shows where in the doc a key word is mentioned

# Next, lets visualize frequency
# Let's go back and create a DFM from this subset

features <- textstat_frequency(data_freq_matrix, n=30)
# only looks at top 30 words used & how much they're used
View(features)

features$feature <- with(features, reorder(feature, -frequency))

ggplot(features, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Keyness
t_o_subset <- corpus_subset(subset, President %in% c('Obama', 'Trump'))
# %in%? 
t_o_dfm <- tokens(t_o_subset, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_group(groups = President) %>% #creating groups bc obama and trump speech numbers are different
  dfm()
# tokenize => clean up tokens => dfm => analysis/visualization etc.

keyness <- textstat_keyness(t_o_dfm, target='Obama')
textplot_keyness(keyness)
# Remember what precisely 'keyness' refers to?
# It's basically your typical wordcloud, on steroids


# Next, let's do a feature co-occurrence matrix (fcm), NOT sequential! 
# a 0 in the co-occurence, it means the word only appears once in each doc!
# For when we want to compare how often terms co-occur in a given document
# Look into the 'window' parameter
obama_subset <- corpus_subset(t_o_subset, President %in% 'Obama')
obama_dfm <- tokens(obama_subset, remove_punct=TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()
obama_fcm <- fcm(obama_dfm)
obama_fcm 

trump_subset <- corpus_subset(t_o_subset, President %in% 'Trump')
trump_dfm <- tokens(trump_subset, remove_punct=TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()
trump_fcm <- fcm(trump_dfm)
trump_fcm

pro_tags <- dfm_select(trump_dfm, pattern="pro*") #designate a pattern
toptag <- names(topfeatures(pro_tags, 50)) #how often do these patterns occurs?
toptag

pro_tags_fcm <- fcm(pro_tags)
pro_tags_fcm

textplot_network(pro_tags_fcm)
# What could this dfm_select feature be really useful for?
# With Twitter data, you can even combine all tweets within a given conversation_id under one document.

# Cosine similarity: how similar is obama's address is to other addresses?
obama_simil <- textstat_simil(data_freq_matrix, data_freq_matrix[c("2009-Obama" , "2013-Obama"),], 
                              margin = "documents", method = "cosine")
obama_simil
#Let's go back to the full corpus for these.
#example q: are first inaugurals more similar to each other than the second?


# Sentiment, Basic
textstat_polarity(inaugural, dictionary=data_dictionary_LSD2015)
# What do you observe?
# another Q: second speech is more happy than the first one? can test


# For more advanced sentiment analysis (and POS/dependency tagging), consider using the spacyR package.
# It's a little tricky to install because of its dependency on python, but well worth it.
# Start here. You need to first install a python package manager called anaconda/miniconda, and from there, everything can be handled inside R:
# https://spacyr.quanteda.io/

spacy_install()
spacy_initialize(model="en_core_web_sm")
parsed_by_spacy <- spacy_parse(head(tweets$text, 10))
View(parsed_by_spacy)

# Basic parser uses the Universal Tagset for POS.
# https://universaldependencies.org/u/pos/all.html

# If you want a more detailed tagset (Penn Treebank Tagset), use:
# parsed_by_spacy <- spacy_parse(head(tweets$text, 10), tag=TRUE)
# https://spacy.io/usage/linguistic-features#pos-tagging

#Just want NER tags?
entity_extract(parsed_by_spacy)


# Or better dependency tagging?
parsed_by_spacy <- spacy_parse(head(tweets$text, 10), dependency = TRUE, entity = FALSE)
View(parsed_by_spacy)

# Want to grab email addresses, URLs, or any other attribute described here?
# https://spacy.io/api/token#attributes

parsed_by_spacy <- spacy_parse(head(tweets$text, 10), dependency = TRUE, entity = FALSE,
                               additional_attributes = c("like_email", "like_url")
)
View(parsed_by_spacy)


# Want other languages? Grab models here:
# https://spacy.io/usage/models
# and initialize with them

# spacy is a memory hog. When you're done, do:
spacy_finalize()