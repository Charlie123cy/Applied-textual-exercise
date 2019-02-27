setwd("C:/Users/Meiqi/Desktop/Meiqi/NHH/Autumn Semester/BAN432/working dictionary for R")
load("firm_dataset.Rda")

require(tibble)
require(dplyr)
require(tm)
require(slam)

mcdonald.cik <- "0000063908"
cocacola.cik <- "0001491675"
pepsi.cik <- "0000077476"

# dplyr package is very powerful, which can specify the exact row and column
# Look for the classification of industry of three companies
raw.data %>% filter(cik == mcdonald.cik) %>% select(industry.fama.french.49)
raw.data %>% filter(cik == cocacola.cik) %>% select(industry.fama.french.49)
raw.data %>% filter(cik == pepsi.cik) %>% select(industry.fama.french.49)

# Define Cosine Similarity
CosineSimilarity <- function(A, B){sum(A*B)/sqrt(sum(A^2)*sum(B^2))}
# Have a small test
CosineSimilarity(A = c(1,0), B = c(0,1))

# Data
section.1.business.v1 <- iconv(section.1.business, to="UTF-8", sub = "")
dtm <- DocumentTermMatrix(Corpus(VectorSource(section.1.business.v1)), 
                          control = list(
                            bounds = list(global = c(0.01, 0.1) * 500)
                          ))
dim(dtm)

# binary
dtm$v <- rep(1, length(dtm$v))

# length adjustment
dtm$v <- dtm$v / sqrt(row_sums(dtm ^ 2))[dtm$i]  # matrix multiple

# compare companies

CosineSimilarity(A = dtm[raw.data$cik == mcdonald.cik,],
                 B = dtm[raw.data$cik == pepsi.cik,])
CosineSimilarity(dtm[raw.data$cik == mcdonald.cik,],
                 dtm[raw.data$cik == cocacola.cik,])
CosineSimilarity(dtm[raw.data$cik == cocacola.cik,],
                 dtm[raw.data$cik == pepsi.cik,])
# go home and change the word limit of dtm

####################
cluster <- kmeans(as.matrix(dtm), centers = 50)
cluster$tot.withinss

raw.data$industry.kmeans <- cluster$cluster

means.per.industry <- aggregate(raw.data$total.assets, 
                                by = list(raw.data$industry.fama.french.49), 
                                mean)
sd(means.per.industry[,2])
require(dplyr)
