#
load("C:/Users/Administrator/Downloads/data_for_lection_10.Rdata")
#(1.1)Extract 'id','text' and 'created' from the list 'query.results' easiest 
#to way to extract a list is to use sapply

require(tibble)
index<-c(1:length(query.results))
#一定一定千万注意用List定位的写法
tweets<- tibble(doc_id=sapply(index,function(i)query.results[[i]]$id),
                text = sapply(index,function(i)query.results[[i]]$text),
                timestamp =sapply(index,function(i)query.results[[i]]$created))#482 
#Convert the text to UTF-8 (cuz in twit there eom..)
tweets$text <- iconv(tweets$text,to="utf-8",sub ="")

#Convert the timestamp to the format YYYY-dd-mm
tweets$timestamp<-as.Date(as.POSIXct(tweets$timestamp,
                                     origin="1970-01-01"))


#i icreasing by one each time 
#(1.2)
#For rhe actual sentimane analysis,call the function analyzeSentiment() use$to denote
#the dictionary we use
tweets$sentimentGI <- analyzeSentiment(tweets$text)$SentimentGI
#(b)Plot sentiment scores:
plot(x = tweets$timestamp,
     y = tweets$sentimentGI)
Sys.setlocale("LC_TIME","English")
           #or "LC_TIME","English_United States.1252
#Messy plot,solution
#(c)Refine the approach by aggregating the sentiment scores by the dates by timestamp
daily.sentiment<-aggregate(tweets$sentimentGI,
                           by = list(tweets$timestamp),
                           mean)
plot(x= daily.sentiment$Group.1,
     y= daily.sentiment$x,
     type = "l")
#######################################
#TASK 2
#Read 
install.packages("jsonlite")
require(jsonlite)
#fromJSON函数就是把json结构的数据转换为R中常见数据类型的工具
tweets.lufthansa<-fromJSON(txt = "C:/Users/Administrator/Downloads/lufthansa_tweets_2016.json")
####
#(2.3.1)Reduce the data frame 'tweets.lufthansa'
tweets.lufthansa<-tibble(doc_id = tweets.lufthansa$doc_id,
                         text = iconv(tweets.lufthansa$text,to="utf-8"),
                         timestamp = tweets.lufthansa$timestamp)
#View(tweets.lufthansa)                         
#(2.3.2)Compile a VCorpus that is submitted to 'analyzeSentiment' in the next
#step (Time difference of 16.10521 secs)
require(tm)
corpus<-VCorpus(DataframeSource(tweets.lufthansa))
#(2.3.3)Use 'analyzeSentiment()' to calculate the sentiment scores
tweets.lufthansa$sentiment.score<-analyzeSentiment(corpus)$SentimentGI
######
#(2.4)Plot tweet frequencies per week
#(a)format date column as data in format year-month-day
tweets.lufthansa$timestamp<-as.Date(tweets.lufthansa$timestamp,format = "%Y-%m-%d")
#(b)add a new column for the week number,formatted as year-week

tweets.lufthansa$week<-format(tweets.lufthansa$timestamp,"%y-%W")
#make sureits an 
#(c)plot tweet frequencies per week
plot(table(tweets.lufthansa$week))

#(2.6)Plot sentiment scores
#(a)use 'aggregate()' to calculate the mean sentiment score per week
#(b)plot the sentiment score per week
weekly.sentiment<-aggregate(tweets.lufthansa$sentiment.score,
                            by=list(tweets.lufthansa$week),
                            mean)
plot(x=weekly.sentiment$x,
     type="l")
#or
