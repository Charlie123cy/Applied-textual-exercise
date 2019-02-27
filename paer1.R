install.packages("tibble")
load("C:/Users/Administrator/Downloads/firm_dataset.Rda")
str(raw.data)
us.states<- datasets::state.name #r base
us.states #
#replace space with "."
us.spaces<-gsub("[[:space:]]",".",us.states)
us.spaces
#is a state referenced in a 10K
grepl(us.states[1],section.1.business[1],ignore.case = T)
grepl(us.states[1],section.1.business,ignore.case = T)
#goal
raw.data$state.count<-0
for(temp.state in us.states){
  raw.data$state.count<-raw.data$state.count+
  grepl(temp.state,section.1.business,ignore.case = T)
}
raw.data$state.count
hist(raw.data$state.count
    )  
barplot(table(raw.data$state.count))

#find breakpoints
bp<-quantile(raw.data$state.count,
             c(0,0.2,0.4,0.6,0.8,1))
bp
#split in buckets
raw.data$state.court.quintle<-cut(raw.data$state.count,
    breaks = bp,
    include.lowest = T)
 
table(raw.data$state.court.quintle)
#compute statistics per bucket
aggregate(raw.data$state.count,
          by = list(raw.data$state.court.quintle),
          mean)
aggregate(raw.data$market.value,
          by = list(raw.data$state.court.quintle),
          mean)#
aggregate(raw.data$market.value,
          by = list(raw.data$state.court.quintle),
          median)
