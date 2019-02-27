require(ca)
#load data
load("C:/Users/Administrator/Downloads/firm_dataset (2).Rda")
#make identifier to the business description
names(section.1.business)<-raw.data$cik
names(section.1.business)
#compile the data into corpus
require(tm)
corpus<-Corpus(VectorSource(iconv(section.1.business,to="UTF-8")))
corpus
#build a tdm
tdm<-TermDocumentMatrix(corpus,
                        control = list(removePunctuation = T,
                                       removeNumbers =T,
                                       stripWhitespace = T,
                                       tolower= T,
                                       stopwords = T,
                                       wordLength = c(3,20),
                                       bounds = list(global = c(2,30))
                        ))
#convert the tdm to a plain matrix
tdm.matrix<-as.matrix(tdm)
#inspect the plain matrix
tdm.matrix[1:5,1:5]
#plot correspondence analysis
plot(ca(iconv(tdm.matrix),to="utf-8"),map="colprincipal",
     main="ehiheihei",
     what=c("none","all"),
     labels=c(0,2))
