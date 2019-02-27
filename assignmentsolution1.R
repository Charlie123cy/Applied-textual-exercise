source.code <- readLines("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20170101&end=20171231")
start <- grep("<div class=\"table-responsive\">", source.code)
end <- grep("</tbody>", source.code)
crypto.source <- source.code[start:end]                         
# limit to relevant rows
crypto.source <- crypto.source[grepl("^.*{0,20}<td", crypto.source)]
# structure into a matrix
crypto.source <- matrix(crypto.source, ncol = 7, byrow = T)
# cleaning
crypto.source <- gsub("<.+?>", "", crypto.source)
crypt <- data.frame(date = crypto.source[,1],
                    open = as.numeric(crypto.source[,2]),
                    high = as.numeric(crypto.source[,3]),
                    low = as.numeric(crypto.source[,4]),
                    close = as.numeric(crypto.source[,5]),
                    volume = as.numeric(gsub(",","",crypto.source[,6])),
                    market.cap = as.numeric(gsub(",","",crypto.source[,7])),
                    stringsAsFactors = F)
# adjust variables
Sys.setlocale("LC_TIME", "English") # unix: Sys.setlocale("LC_TIME", "C")
FALSE [1] "English_United States.1252"
crypt$date <- as.Date(crypt$date, format = "%b %d, %Y")
Desired output:
# desired data.frame
head(crypt)
install.packages("ggplot2")
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)
ggplot2::ggplot(data = crypt, aes(x = date, y = close)) +
  geom_line() + xlab("Date") + ylab("Closing price in USD") +
  ggtitle("Bitcoin price \n") + theme_economist()
