
general<-"https://www.sec.gov/Archives/edgar/full-index/"
year<-2015
quarter<-1
url.adress<-paste0(general,year,"/QTR",quarter,"/master.idx",sep="")
url.adress
edgar.index.raw<-readLines(url.adress)

#clean our data
usefulcode<-strsplit(edgar.index.raw,"\\|")
usefulcode2<-do.call(rbind,usefulcode)
usefulcode2
#find 10-k
usefulcode3<-grep("10-K",usefulcode2[,3])#get the row number
usefulcode3
use=usefulcode2[usefulcode3,]#satisfied row
for( i in usefulcode3) {
form.10k<-readLines(paste0("https://www.sec.gov/Archives/",use[i,5]),n=100)
}
save(form.10k,file = paste0("form10k",use[i,1],".Rda"))
