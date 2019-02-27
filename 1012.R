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
mcdonald.cik <- "0000063908"
cocacola.cik <- "0001491675"
pepsi.cik <- "0000077476"
#find the row we intetersted (cik equals the company we need)
#look at industry assignment
raw.data%>%filter(cik==mcdonald.cik)%>%select(industry.fama.french.49)
raw.data%>%filter(cik==cocacola.cik)%>%select(industry.fama.french.49)
raw.data%>%filter(cik==pepsi.cik)%>%select(industry.fama.french.49)
#means industry description not very reliable
#Define Cosine Similarity(向量相乘)
CosineSimilarity<-function(A,B){sum(A*B)/sqrt(sum(A^2)*sum(B^2))}
CosineSimilarity(A=c(1,0),B=c(0,1))
#Data
section.1.business.v1<-iconv(section.1.business,to="UTF-8",sub="")
dtm<-DocumentTermMatrix(Corpus(VectorSource(section.1.business.v1)),
                        control=list(
                          bounds=list(global=c(0.01,0.1)*500)
                        ))

dim(dtm) 
str(dtm)
colnames(dtm)
#binary
#In the original dtm, dtm$i equals the vector value corresponding to each column
#But here, we only want to use binary value={0,1}, so we make all the dtm$i<-c(1,1,1,...)
#every"1" stands for a show up of a vector value
dtm$v<-rep(1,length(dtm$v))
#length adjustment
#每一个v的值不一样，我们想把他们化成单位向量�?
dtm$v<-dtm$v/sqrt(row_sums(dtm^2))[dtm$i]   #matrix multiplycation#corresponding paramter
#compare company
CosineSimilarity(A=dtm[ raw.data$cik==mcdonald.cik,],
                 B=dtm[ raw.data$cik==pepsi.cik,])
############# dtm is a sparse matrix which should be .... here

cluster<-kmeans( as.matrix(dtm),centers=50)
                                cluster$tot.withinss #385.7155
cluster$cluster #481 6
raw.data$industry.kmeans<-cluster$cluster
means.per.indstry<-aggregate(raw.data$total.assets,
                             by=list(raw.data$industry.fama.french.49),
                             mean)
sd(means.per.indstry[,2])
###########cluster by myself3######
set.seed(12345)
raw.data$industry.random<-sample(1:50,size=nrow(raw.data),replace=T)
table(raw.data$industry.random)
#compute cluster means
cluster.means<-aggregate(as.matrix(dtm),list(raw.data$industry.random),
                         mean)
sq.dev<-rowSums((as.matrix(dtm)-cluster.means[raw.data$industry.random,])^2)
wcss<-sum(sq.dev)
######
var<-raw.data$operating.income/raw.data$total.assets
raw.data$var<-var
var.means<-aggregate(raw.data$var,list(raw.data$),mean)
var.means[,2]
var.sd<-sd(var.means[,-1])
table(table(cluster.means$Group.1))
table(table(3:50))
