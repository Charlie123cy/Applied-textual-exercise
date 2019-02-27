#setwd()
#How is the sentiment score computed in the package SentimentAnalysis
install.packages("SentimentAnalysis")
require(SentimentAnalysis)

#(1)input is vector
# stopwords are removed by the function analyzeSentiment().
# wordcount gives 5 words in first element and 4 words in second element
x <-c("This is good restaurant.I have been here several times and I liked it.",
      "This restaurant is neither good nor bad.")
analyzeSentiment(x)
#x<-loadDictionaryGI()
#x[[2]][grepl("sever",x[[2]])]
#(2)Input is a Document Term Matrix
#  Stopwords are not removed by the function analyzeSentiment().
#  Wordcount gives 10 for the first "document" and 5 for the second.
require(tm)
corpus<-Corpus(VectorSource(x))
dtm <- DocumentTermMatrix(corpus,
                          control = list(stopwords=T))
#constructing dtm you must remove stopwords
analyzeSentiment(dtm)
#After removing stopwords, the wordcount is identical the wordcount
#for vector input
#(3)Input is a copus object
#stopwords are removed by the function analyzeSentiment
corpus.v<-VCorpus(VectorSource(x))
analyzeSentiment(corpus.v)

#(4)Input is a data frame
#    Stopwords are removed by the function analyzeSentisment
# 4. Input is a data frame
# Stopwords are removed by the function analyzeSentiment.
df <- as.data.frame(x,
                    stringsAsFactors = F)
analyzeSentiment(df)
