
require(tibble)
require(dplyr)
require(tm)

require(slam)
load("C:/Users/Administrator/Downloads/verbs.Rda")
load("C:/Users/Administrator/Downloads/syllable.Rda")
load("C:/Users/Administrator/Downloads/firm_dataset.Rda")
head(syllable_counts_data)
head(verbs)
#complex words
section.7.mda.1<-iconv(section.7.mda,to="UTF-8",sub="")
doc<-paste(section.7.mda.1,collapse=" ")
dtm <-DocumentTermMatrix(Corpus(VectorSource(doc)))
dim(dtm)

term.freq<-tibble(term=dtm$dimnames$Terms,
                  freq = as.vector(dtm[1,]))
term.freq %>% arrange(-freq)
term.freq$syllable<-syllable_counts_data$syllables[match(term.freq$term,
                          syllable_counts_data$word)]
term.freq
#most frequent complex terms
term.freq%>% filter(!is.na(syllable)&syllable>2)%>%
  arrange(-freq)   #results shows that these words are not very hard even have 4 sylabbles
#######
raw.data$obj.size <-
  raw.data$nr.words <-
  raw.data$nr.sentences <-
  raw.data$words.per.sentence <-
  raw.data$nr.complex.words <-
  raw.data$share.complex.words <-
  raw.data$nr.verbs <-
  raw.data$FOG <-
  raw.data$length <-
  raw.data$YULES.K<-
  raw.data$FICHTNERS.C <- NA  #declare empty varible use NA at last declare every before is NA

yules.k <- function(input) {
  m1 <- length(input); temp <- table(table(input))
  m2 <- sum("*"(temp, as.numeric(names(temp))^2))
  return(10000*(m2-m1) / (m1^2))
}

# loop
i<-1
for( i in 1){
  # make words
  doc <- strsplit(section.7.mda[i], "\\s+")[[1]]
  # clean a bit
  doc <- tolower(doc)
  doc.stemmed <- stemDocument(doc) #contains-contain
  # individual statistics
  raw.data$obj.size[i] <- as.numeric(object.size(section.7.mda.1[i]))
    raw.data$nr.words[i] <- length(doc)
    raw.data$nr.sentences[i] <- sum(grepl("(\\.|\\?|\\!|;|:)$",doc))
    raw.data$words.per.sentence[i] <- raw.data$nr.words[i]/raw.data$nr.sentences[i]
    raw.data$nr.complex.words[i] <-sum(doc%in%
                                         syllable_counts_data[
                                         syllable_counts_data$syllables>2])
    raw.data$share.complex.words[i] <-raw.data$nr.complex.words[i]/
                                                    raw.data$nr.words[i]
    raw.data$nr.verbs[i] <- sum(doc %in% tolower(verbs))
    # readability measures
    raw.data$FOG[i] <- 0.4*(raw.data$words.per.sentence[i]+100*
                              raw.data$nr.complex.words[i] )
    raw.data$length[i] <- log(raw.data$nr.words[i]+1) #delete infinitive or do it later as in slides
    raw.data$YULES.K[i] <- yules.k(doc.stemmed)
    raw.data$FICHTNERS.C[i] <- raw.data$nr.verbs[i]/raw.data$nr.sentences[i]*
      raw.data$nr.words[i]/raw.data$nr.sentences[i]
    # progress
    print(i)   #print which document we are currently
}
character(10)
