library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(twitteR)
consumer_key = ''
consumer_secret <- ''
access_token = ''
access_secret = ''
#funcion para autorizar el acceso a data
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#funcion para sacar tweets 
tweets = searchTwitter("", n=, lang="")
# vuelca la informacion de los tweets a un data frame
df = twListToDF(tweets)
#guardar lista de tweets para clasificar
write.csv(df, file = "C:/Users/max/Desktop/UAI/Text minning/newfile.csv")
# random de tweets rescatados
set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)
#convetir lista de tweets en texto
tweets_text = sapply(tweets, function(x) x$getText())
#ver estructura de nuevo arreglo
str(tweets_text)
#crear corpus desde vector de texto 
tweet_corpus = Corpus(VectorSource(tweets_text))
#inspeccionar tweet numero x
inspect(tweet_corpus[x])
# remueve retweets
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_text)
# remueve @otragente
txtclean = gsub("@\\w+", "", txtclean)
# remueve simbolos de puntuaci?n
txtclean = gsub("[[:punct:]]", "", txtclean)
# remueve n?meros
txtclean = gsub("[[:digit:]]", "", txtclean)
# remueve links
txtclean = gsub("http\\w+", "", txtclean)
## en este metodo de limpieza qui se tiene que convetir a corpus txtclean
tweet_corpus = Corpus(VectorSource(txtclean))

###metodo para corpus 
#limpiar textos de puntuacion
tweet_clean = tm_map(tweet_corpus, removePunctuation)
#poner todo en minusculas
tweet_clean = tm_map(tweet_clean, content_transformer(tolower))
#remover stopwords
tweet_clean = tm_map(tweet_clean, removeWords, stopwords("spanish"))
#remover numeros
tweet_clean = tm_map(tweet_clean, removeNumbers)
#remover espacios
tweet_clean = tm_map(tweet_clean, stripWhitespace)
#remover palabras especificas
tweet_clean = tm_map(tweet_clean, removeWords, c("aborto"))
#guardar corpus como plain text en pc
writeLines(as.character(tweet_corpus), con="tweet_Corpus.txt")
###################################
#word steaming
tweet_clean2 <- tm_map(tweet_clean, PlainTextDocument)  # needs to come before stemming
tweet_clean2 <- tm_map(tweet_clean2, stemDocument, "spanish")
tweet_corpus2 = Corpus(VectorSource(tweet_clean2))



#representacion de palabras en una matriz
dtm <- DocumentTermMatrix(tweet_clean)
#inspeccionar fragmento matriz
inspect(dtm[40:50, 10:15])
#convertir dtm en matriz
dtm2= as.matrix(dtm)
#sumar repeticiones de terminos
colSums(dtm2)
#particion DF, corpus y matriz para pode entrenar (80%//20%)
df.train <- df[1:4000,]
df.test <- df[4001:5000,]

dtm.train <- dtm[1:4000,]
dtm.test <- dtm[4001:5000,]

corpus.clean.train <- tweet_clean[1:4000]
corpus.clean.test <- tweet_clean[4001:5000]
####selecci?n de elementos necesarios####
#numero de elementos existentes
dim(dtm.train)
#eliminando elementos que tengan una frecuencia menor a 5
fivefreq = findFreqTerms(dtm.train, 25)
length((fivefreq))
# matriz idf 
#creating term matrix with TF-IDF weighting
terms <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

#or compute cosine distance among documents
dissimilarity(tdm, method = "cosine")
#creacion de nuevas matrices de frecuencia (train y test)

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.test.nb)
# Funcion para convertir frecuencia de palabras en si esta(yes) o no (absence) y etiquetarlas
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
#aplicar funcion a matrices para obtener muestreo final 
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
# entrenar el clasificador /////
system.time( classifier <- naiveBayes(trainNB, df.train$text, laplace = 1) )
# Usando el clasificador para predecir usando test set.
system.time( pred <- predict(classifier, newdata=testNB) )
# Creando una tabla de erdad comparando la clase de la predicci?n con la etiqueta real 
table("Predictions"= pred,  "Actual" = df.test$text )
# creando matriz confusion
conf.mat <- confusionMatrix(pred, df.test$class)
# ver estadisticas
conf.mat
#medir precisi?n
conf.mat$overall['Accuracy']
