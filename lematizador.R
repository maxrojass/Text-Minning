require(SnowballC)

worder1<- c("I am taking","these are the samples",
            "He speaks differently","This is distilled","It was placed")
#pasando a dataframe tweets limpios
dataframe <- data.frame(text=sapply(tweet_clean, identity), 
                        stringsAsFactors=F,words=tweet_clean)
df1 <- data.frame(id=1:5000, words=dataframe)
#convertir corpus en lista de texto
dataframe <- data.frame(text=sapply(mycorpus, identity), 
                        stringsAsFactors=F)

##luego de volver a pasar a dataframe twitter limpios pasar a corpus de nuevo
corp <- Corpus(VectorSource(dataframe))

corp1 <- Corpus(VectorSource(dataframe$text))
inspect(corp1)
corp1 <- tm_map(corp1, stemDocument)
inspect(corp1)
corp1 <- Corpus(VectorSource(dataframe$text))

tdm1 <- TermDocumentMatrix(corp1, control=list(stemming=TRUE))
as.matrix(tdm1) 
#pasar de corpus a data frame para lematizar
dataframe <- data.frame(text=sapply(corp1, identity), 
                        stringsAsFactors=F)
#identificar por oracion
df1 <- data.frame(id=1:5000, words=dataframe)