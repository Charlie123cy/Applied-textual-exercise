#setwd()
#read in business descriptions into R
load("C:/Users/Administrator/Downloads/firm_dataset.Rda")


#Use sapply and generate a list where each element is a character vector that
#contatins the individual words of a document as individual elements.
#We can use scan()for the tokenizaiton of the document
#here business.des is a list of character vectors;one vector for each document;
#each vector has one word per element
business.des<-sapply(section.1.business,
                    function(x) scan(text =x,
                                     what = "character",
                                     quote = "")) #
View(business.des)

#Find the ements in each of the 500 vectors(which means 500 documents) that match the search term
#here we get index for each document that contain "environment.*"
index.env<- sapply(business.des,
                   function(x)grep("environment.*",x))
#See if the matching works
View(index.env)   
business.des[[3]][index.env[[3]]]#注意list里的定位一个向量的方式
business.des[[1]][1-n:1]

#define a function that constructs a tibble (data frame) of the desired format
#Arguments to the function are:index.env,business.des,n(length of context),
#and doc.nr position to find where it is 
install.packages("tibble")
require(tibble)
#WATCH OUT!Inside this function, business.des is just one character vector,so we
#don't need the "[[]]"
#Here we just want to create a function, so it doesn't matter the value of n.You
#stupid!!!!!!
make.KWIC <- function(index.env,business.des,n,doc.nr){
  KWIC <- tibble(left = sapply(index.env,
                               function(i) {paste(business.des[i-n:1],collapse = " ")}),
                 keyword = business.des[index.env],
                 right = sapply(index.env,
                                function(i){paste(business.des[i+1:n],collapse = " ")}),
                 doc.nr = doc.nr,
                 position.in.text = index.env/(length(business.des)*0.01))
  return(KWIC)
}
#position.in.text:here teacher wants to scale the position making it range from
#0(=first word) to 100(=last word). Before we scale it, the position is 1,2,3..500
#which is a little bit large so is not very comparable. Here, he uses index/one percent of the total number
#of elements in that document to scale the index number
#Create an empty list that will hold the tibbles with KWIC-table for each document
result<- list()

#Iterate over the lists 'index.env' and 'business.des' using the function we defined above.
for(i in 1:length(business.des)){
  result[[i]]<-make.KWIC(index.env[[i]],
                         business.des[[i]],
                         n=3,
                         doc.nr = i)
}
#Examples for undesired hits: result[[1]] and result[[5]].The word "envionment"
#does not have a meaning of "natural environment" here.
result[[1]]
result[[5]]
#Combine the 500 tibbles into one, using do.call()
merged.results<-do.call("rbind",result)
#View(merged,results)
# Find the most common words for each position(visualize as a wordcloud).
#Make a frequency list for "left" and "right"column
#Load tm-package because we need the stopword list
#paste the whole column into one string
right<-paste(merged.results$right,collapse=" ")
#Tokenize the string with scan()
right<-scan(text = right,
            what = "character",
            quote = "")
#keep only those elements that are not in tm's stopword list
#and convert to lower case
install.packages("tm")
require(tm)
right<- right[!tolower(right) %in% stopwords()]
right[1:50]
#make a frequency list and convert into a tibble
right.freq<-as_tibble(table(right))
#Order the tibble in descending order of frequency and make a wordcloud
right.freq<-right.freq[order(-right.freq$n),]
install.packages('wordcloud')
require(wordcloud)
wordcloud(words = right.freq$right[1:100],
          freq = right.freq$n[1:100])
#Remove some of the words from 'left-freq' that obviously don't have
#anything to do with natural environment
custom.stopwords<-"(law|local|state|economic|compl|current|compet|regula|applicable|operating)"
right.freq <- right.freq[!grepl(custom.stopwords,right.freq$right),]               
#re-run?
wordclound(words = right.freq$right[1:100],
           freq = right.freq$n[1:100])
#Have a look at those firms that talk about 'environmental protection'
#nr.45,53 and 59
m <- gregexpr(".{100}environmental protection.{20}",section.1.business[[59]])
regmatches(section.1.business[[59]],m)
#where in the text does the word "environment" appear
