install.packages("ca")
#setwd()
load("C:/Users/Administrator/Downloads/firm_dataset (2).Rda")
###############
#Excercise1:Example from Greenacre(2007)
m<-matrix(c(6,1,11,1,3,11,4,25,0,2,2,20),ncol=3,byrow=T)
colnames(m)<-c("Holidays","Half_Days","Full Days")
rownames(m)<-c("Norway","Canada","Greece","France/Germany")
require(ca)
plot(ca(m),
     map="rowprincipal",#compare contries in row
     what=c("all","none"),#all data in row, no in col
     labels=c(2,0)) 
###########################################
#Exercise2:
#1.prepare the data
#The business description in the vector can not be identified
# no use names(section.1.business)
names(section.1.business)<-raw.data$cik
names(section.1.business)
#2.Compile a corpus of business description
#LOad tm package
require(tm)
#Load the first 100 documents in the specified vector
text.source<-VectorSource(section.1.business[1:100])
corpus<-Corpus(text.source)
# check corpus
#Inspet names of the documents
names(corpus)
#3.Create a Term Document Matrix.
#Each additional term is one more dimension in the data,so we have to reduce
#as much as possible
tdm<-TermDocumentMatrix(corpus,
                        control = list(stopwords = T,
                                       tolower = T,
                                       stripWhitespace=T,
                                       removeNumbers = T,
                                       removePunctuation = T,
                                       wordLengths = c(3,20),
                                       bounds = list(global= c(2,30))
                        ))
#Convert the Term Document Matrix to "plain" matrix. This is necassary for the
#correspondence analysis.
tdm.matrix<-as.matrix(tdm)
#inspect the document term matrix
tdm.matrix[1:5,1:5]
#4.Submit the data to a correspondece analysis
require(ca)
plot(ca(tdm.matrix),
     map="colprincipal",
     what=c("none","all"),
     labels=c(0,2))
#summary(ca(tdm.matrix))
#plot the column profiles(colprincipal)
#0001001233:SANGAMO THERAPEUTICS, INC
#there seem to be three vertices
#only about 5% of the variation in the data is explained(this is due)
#4a. Re-define the next identifiers
tdm<-TermDocumentMatrix(corpus,
                        control = list(stopwords = T,
                                       tolower = T,
                                       stripWhitespace=T,
                                       removeNumbers = T,
                                       removePunctuation = T,
                                       wordLengths = c(3,20),
                                       bounds = list(global= c(2,30))
                        ))
plot(ca(tdm.matrix),
     map="colprincipal",
     what=c("none","all"),
     labels=c(0,2))
#4bUse a different text source only texts from 6 industries
text.source<-VectorSource(section.1.business[grepl("46 Insur|48 Fin|13 Drugs|12 MedEq|30 Oil|41 Trans",names(section.1.business))])
corpus<-Corpus(text.source)

names(corpus)

tdm<-TermDocumentMatrix(corpus,
                        control = list(stopwords = T,
                                       tolower = T,
                                       stripWhitespace=T,
                                       removeNumbers = T,
                                       removePunctuation = T,
                                       wordLengths = c(3,20),
                                       bounds = list(global= c(2,30))
                        ))
#convert to a plain mtrix
tdm.matrix<-as.matrix(tdm)
plot(ca(tdm.matrix),
       map="colprincipal",
       what=c("none","all"),
       labels=c(0,2))
#check how many rows in matrix
nrow(tdm.matrix)
#figure out ways to reduce the row 
#maybe wordlengths c(8,20)
#check nrow(tdm.matrix) check the image plot looks like different still have clusters
