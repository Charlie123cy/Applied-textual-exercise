install.packages("jsonlite")
library(jsonlite)
input.edgar<-readLines("04_Sildes_data (2).xml")
#define the URL
endpoint<-"http://en.wikipedia.org/w/api.php?"
action<-"action=query"
titles<-"titles=Enron%20scandal"
prop<-"prop=exracts"
format<-"format=json"
my.URL<-paste0(endpoint,action,"&",titles,"&",prop,"&",format)
#inspect
my.URL
#send the url to the API and process
install.packages("curl")
require(curl)
my.result<-fromJSON(my.URL)
#inspect the structur of result
my_str<-str(my.result)
install.packages("twitteR",dependencies = T)
require(twitteR)




#EDGAR
input.edgar<-readLines
#<.+?>?
 # paste0 means space are already moved
input.wiki<-fromJSON(search.url)
text.wiki<-input.wiki$query$pafes$'11954274'$extract
gsub("<.+?>","",text.wiki)
query.results[[1]]$tex
ac<-readLines("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20170101&end=20171231")
usefulco<-ac[908:913]
gq<-gsub("<.+?>","",usefulco)
gq
