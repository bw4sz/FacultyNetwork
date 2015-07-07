#Scopus Database Query By Journal Article
library(XML)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(httr)
library(stringr)
library(chron)
library(vegan)
library(knitr)
library(RSQLite)
library(bipartite)
library(doSNOW)
library(foreach)
library(igraph)
library(proto)

#source function 
source("Funtions.R")

#open database
d<-src_sqlite(path = "C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3")

#Read in Journal Class
journaldf<-d %>% tbl("journal_scopus") %>% collect()

j<-d %>% tbl("JA") %>% select(Journal) %>% distinct_() %>% collect()

journaldf<-journaldf[!journaldf$title %in% j$Journal,]

#make sure to not to get journal of biological chhmistry, it is badly malformed?
journaldf<-journaldf[!journaldf$title=="Journal of Biological Chemistry",]

journaldf<-journaldf[!duplicated(journaldf$query),]



#loop until we run out of calls. Parallelized in user defined chunks
r<-TRUE
while(r){
  #define number of journals and cluster size
  r<-queryscopus(runs=40,size=10)
  gc()
}

#save.image("Journal.RData")