base.url<-"https://www.sec.gov/Archives/edgar/full-index/2018/QTR"
base.destfile<-"D:/我的文档"
for(q in c(1:2)){
download.file(url=paste0(base.url,q,"/master.idx"),
              destfile=paste0(base.destfile,q,"_master.idx"),
              mode="wb")
}
filings<-read.table(file="D:/我的文档1_master.idx",
                     header= F,
                     sep="|",
                     nrows=30000,
                     skip=11,
                     stringsAsFactors=F)
colnames(filings)<-c("CIK", "Name", "Form", "Date", "URL")
choose<-grepl("^10-K$",filings$Form)
filings<-filings[choose,]
head(filings,2)
for(i in c(1:2)){
  download.file(url=paste0("https://www.sec.gov/Archives/",
                         filings$URL[i]),
                destfile = paste0("D:/我的文档/newtry",i,
                                 "-",
                                 filings$CIK[i],
                                 ".txt"),
                mode="wb")
}
files<-list.files("D:/我的文档/newtry/",full.names=T)
sustainability<-data.frame(CIK=character(),
                           susscore=numeric(),
                           susscaled=numeric(),
                           stringsAsFactors = F)
for(f in 1:length(files)){
  text<-paste(readLines(files[f]),collapse = " ")
  text<-gsub(".+?(<TEXT>.+?</TEXT>).+","\\1",text,ignore.case=T)
  text<-gsub("<.+?>","",text)
  text<-gsub("&.+?;","",text)
  text<-gsub("[[:punct:]]","",text)
  text<-gsub("[[:digit:]]","",text)
  text<-gsub("[[:space:]]{2,}","",text)
  #get the name of cik
  cik<-gsub(".+/.+-([[:digit:]]+).txt","\\1",files[f])
  #calculate score
  match<-gregexpr("sustainab.*?\\s",text,ignore.case = T)
  match.length<-length(regmatches(text,match)[[1]])
  #calculate scaled score
  word.match<-gregexpr(" ",text)
  word.match2<-length(regmatches(text,word.match)[[1]])
  scale.score<-match.length/(word.match2*0.001)
  sustainability[f,]<-c(cik,match.length,scale.score)

}