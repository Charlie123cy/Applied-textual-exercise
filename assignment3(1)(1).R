setwd("C:/Users/Meiqi/Desktop/Meiqi/NHH/Autumn Semester/BAN432/assignment/Assignment3")
load("firm_dataset.Rda")
#Data
section.1.business.v1<-iconv(section.1.business,to="UTF-8",sub="")
#load package
require(tm)
require(dplyr)
require(tibble)
require(slam)
#each punctuation, stopwords, numericals is an additional term, which will
#add more dimensions in dtm, so we should remove them
dtm<-DocumentTermMatrix(Corpus(VectorSource(section.1.business.v1)),
                        control=list(
                          stopwords = T,
                          tolower = T,
                          stripWhitespace=T,
                          removeNumbers = T,
                          removePunctuation = T,
                          wordLengths = c(3,20),
                          bounds = list(global= c(5,50))
                        ))
#check dtm
dim(dtm)
b<-dtm$dimnames$Docs[3]
#add a new column to raw data
raw.data$index<-1:nrow(raw.data)
#get cik of firms related oil  and the index of oil related firms 
#in section.1.business list
cik.oil<-raw.data%>%
  filter(industry.fama.french.49=="30 Oil")%>%
  select(cik,index)
#find the part in dtm related oil industry
dtm.oil<-dtm[cik.oil$index,]
#get the mean of oil related terms' fre
oil.matrix<-as.matrix(dtm.oil)
mean.oil<-colMeans(oil.matrix)
#create a matrix to store the data of oil related terms and their freq
oil.fre<-rbind(rownames(dtm.oil),mean.oil)
dim(oil.fre)
#transform oil.fre into a simple triplet matrix
oil.fre.simple<-as.simple_triplet_matrix(oil.fre)

# Define Cosine Similarity
CosineSimilarity <- function(A, B){sum(A*B)/sqrt(sum(A^2)*sum(B^2))}
# Have a small test
CosineSimilarity(A = c(1,0), B = c(0,1))
# binary
dtm$v <- rep(1, length(dtm$v))
oil.fre.simple$v<- rep(1, length(oil.fre.simple$v))
# length adjustment
dtm$v <- dtm$v / sqrt(row_sums(dtm ^ 2))[dtm$i]
oil.fre.simple$v <- oil.fre.simple$v / sqrt(row_sums(oil.fre.simple ^ 2))[oil.fre.simple$i]

#compare companies
nonoil.companies <- raw.data[!grepl("^30 Oil$", raw.data$industry.fama.french.49),]
nonoil.companies$cosine.similarity <- as.numeric(0)


for (p in 1: nrow(nonoil.companies)){
  cs <- CosineSimilarity(A = dtm[raw.data$cik == nonoil.companies[p,]$cik,],
                         B = oil.fre.simple[1,])
  nonoil.companies[p,]$cosine.similarity <- cs
}

nonoil.companies <- nonoil.companies[order(nonoil.companies$cosine.similarity, decreasing = T),]

oiltracking.portfolio <- nonoil.companies[1:25,]
