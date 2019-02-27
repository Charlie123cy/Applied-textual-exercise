setwd()
load("C:/Users/Administrator/Downloads/data_for_lecture_07.Rdata")
##############333
#Task 1:
#write a simple sentence splitter
################
input<-section.1.business[1]
input
#(1)split the string at certain points   

item.1.sentences<-strsplit(input,"(\\.|\\?|\\!|;|:)[[:space:]]")[[1]]
item.1.sentences
#(2)look at the output.What happened to the abbrevations?What can we do?
abbr <- "(e\\.g|i\\.e|D\\.C|U\\.S\\.)"
y<-"This is e.g funny."
gsub("(This) (is) (funny)","\\2 \\1 \\3",y)
item.1.sentences<-gsub(item.1.sentences,"<abbr>\\1</abbr>",input)
#In the test example, we split the sentence "This is funny" to 
#different words manually. So in practice example, [item.1.sentences]is
#already a sentence composed with splitted words.
#In practice example, 
item.1.sentences
#(3)remove abbr-masking
item.1.sentences<-gsub("<abbr>(.+?)</abbr>","\\1",item.1.sentences)
item.1.sentences

#######################
#Task 2 :
#Term extraction
#######################
#Find all types in the "Business description"corpus that do not occur in the 
#British National Corpus

merged.frequencies <- merge (x= bnc.freq,
                             y= item1.freq,
                             by = "word",
                             all = T) #contains all rows from bnc
#得到同一个词在不同列（不同数据框）的出现频率
View(merged.frequencies)
#filter out those types that are only present in item1.freq
#i.e they have 'NA' in 'Freq.x'
term.candidates<-subset(merged.frequencies,is.na(merged.frequencies$Freq.x))
head(merged.frequencies
     )
#A lot of noise.Select only term candidates with a frequency>100
#in "Businesss Description" corpus
term.candidates<-subset(merged.frequencies,
                        is.na(merged.frequencies$Freq.x)&merged.frequencies$Freq.y >= 100)
View(term.candidates)
#Order the data frame 'term.candidates'
term.candidates <- term.candidates[order(term.candidates$Freq.y,
                                         decreasing = T), ]                       
View(term.candidates)
################################
install.packages("tm")
library(tm)
#(2)read the text data（create vector source first) 
text.source<-VectorSource(section.1.business)
#(3)compile a corpus of texts(create the corpus)
corpus<- Corpus(text.source)
Corpus
#(4.1)Create on DTM(文档词条矩阵。默认情况下矩阵的元素是
#词的频率)
dtm <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = T,
                                         removeNumbers = T,
                                         stopwords=T,
                                         tolower = T)) #remove stopwords
dtm.matrix<-as.matrix(dtm) #inspect[1:10,1:10]
x<-stopwords()#inspect 174
#