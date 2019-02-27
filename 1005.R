# cheet sheet
#set working director
setwd()

#load data
load("C:/Users/Administrator/Downloads/firm_dataset (1).Rda")

#packages
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("tibble")
install.packages("dplyr")
install.packages("SentimentAnalysis")
install.packages("tm")
install.packages("slam")
require(ggplot2)
require(tibble)
require(dplyr)
require(SentimentAnalysis)
require(tm)
require(slam)
require(ggthemes)
#document term matrix
section.7.mda.v1<-iconv(section.7.mda,to="UTF-8")
dtm <- DocumentTermMatrix(Corpus(VectorSource(section.7.mda.v1)))
dim(dtm)
#term frequencies
term.freq<-tibble(term = colnames(dtm),
                  freq = colSums(dtm))  #term=colnames(dtm)
DictionaryGI$negative
#most frequent H4N negetive terms
term.freq %>% 
  filter(term %in% DictionaryGI$negative) %>% 
  arrange(desc(freq)) %>%
  filter(nchar(term)>6)
#compute sentiment
raw.data$H4N.Inf<-row_sums(dtm[,colnames(dtm)%in% DictionaryGI$negative]) / row_sums(dtm)
raw.data$fin.neg<-row_sums(dtm[,colnames(dtm)%in% DictionaryLM$negative]) / row_sums(dtm)
plot(raw.data$H4N.Inf,raw.data$fin.neg)
#tf-idf
term$tfidf.score<- (col_sums(dtm)/
  sum(col_sums(dtm))) *
  log(nrow(dtm)/col_sums(dtm != 0))
?ggplot
