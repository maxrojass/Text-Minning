#install.packages("openNLPmodels.es", repos = "http://datacube.wu.ac.at/", type = "source")
#install.packages("openNLP")
#install.packages("NLP")
#install.packages("tm")
#install.packages("stringr")

library(NLP)
library(openNLP)
library(openNLPmodels.es)
library(tm)
library(stringr)

setwd("C:\\Users\\Daniela\\Desktop\\UAI\\practica")
opiniones_a <- read.csv("opinion_a.csv", header=FALSE, sep=",")

a <- opiniones_a[1:5000,]
tweets <- a$X6
token_tweets <- MC_tokenizer(tweets)
f_tweets <- as.factor(token_tweets)

tagPOS <-  function(x) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  f_tweets <- Annotation(1L, "sentence", 1L, nchar(s))
  f_tweets <- annotate(s, word_token_annotator, f_tweets)
  ann <- annotate(s, Maxent_POS_Tag_Annotator(), f_tweets)
  ann_w <- ann[ann$type == "word"]
  POStags <- unlist(lapply(ann_w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[ann_w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)}

tagged_str <- tagPOS(f_tweets)
tagged_str

############################################

tags <- c("CC"="conjunction", "CD"="cardinal number", "DT"="determiner", "EX"="existencial there", "FW"="foreign word", "IN"="preposition",
"JJ"="adjective", "JJR"="comparative adjective", "JJS"="superlative adjective", "LS"="list item marker", "MD"="modal",
"NN"="singular noun", "NNS"="plural noun", "NNP"="singular proper noun", "NNPS"="plural proper noun", "PDT"="predeterminer",
"POS"="possesive ending", "PRP"="personal pronoun", "PRP$"="possesive pronoun", "RB"="adverb", "RBR"="comparative adverb", 
"RBS"="superlative adverb", "RP"="particle", "SYM"="symbol", "TO"="to", "UH"="interjection", "VB"="base form verb",
"VBD"="past tense verb", "VBG"="present participle verb", "VBN"="past participle verb", "VBP"="non-3rd person singular present verb",
"VBZ"="3rd person singular present verb", "WDT"="wh-determiner", "WP"="wh-pronoun", "WP$"="possesive wh-pronoun", "WRB"="wh-adverb")

tagged_str$POStags <- tags[tagged_str$POStags]
tagged_str$POStags
