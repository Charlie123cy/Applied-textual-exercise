setwd("C:/Users/Meiqi/Desktop/Meiqi/NHH/Autumn Semester/BAN432/working dictionary for R")

# Task 1:
# Add sentiment scores to a data frame with tweet texts
# Load the data frame
load("C://Users/Administrator/Downloads/data_for_lecture_11.Rdata")

# (1.1) Add sentiment scores based on the 4 dictionaries that
#       are included in the package "SentimentAnalysis"
# SentimentGI
# SentimentHE
# SentimentLM
# SentimentQDAP
require(SentimentAnalysis)
temp <- "This is a good restaurant"
analyzeSentiment(temp)

emirates.tweets$SentimentGI <- analyzeSentiment(emirates.tweets$text)$SentimentGI
emirates.tweets$SentimentHE <- analyzeSentiment(emirates.tweets$text)$SentimentHE
emirates.tweets$SentimentLM <- analyzeSentiment(emirates.tweets$text)$SentimentLM
emirates.tweets$SentimentQDAP <- analyzeSentiment(emirates.tweets$text)$SentimentQDAP

# (1.2) Make a plot of the SentimentGI sentiment score:
#       We don't see anything!
plot(emirates.tweets$SentimentGI)

# (1.3) Calculate weekly sentiment score
emirates.tweets$week <- format(emirates.tweets$timestamp, "%y-%W")
emirates.weekly <- aggregate(emirates.tweets[,3:10], 
                             by = list(emirates.tweets$week), 
                             mean)
plot(emirates.weekly$SentimentGI, type = "l")


#########################################
# Task 2: 






