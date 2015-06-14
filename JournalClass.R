#Aim
#load libraries

library(XML)
library(stringr)
library(RCurl)
library(tm)
require(reshape)
require(bipartite)
require(dplyr)
require(stringr)
library(httr)
library(chron)
library(vegan)
library(knitr)
library(bipartite)
library(doSNOW)
library(foreach)

#Source functions
source("Funtions.R")

#connect to db
my_db<-src_sqlite(path = "C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3")

#read in all other URLS
a<-read.table("Data/GoogleClass.txt",sep="\t")
URLS<-as.character(a$V1)

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#remove duplicates
URLS<-URLS[!duplicated(URLS)]

#Get journals from each of the google urls
out<-list()

for (x in 1:length(URLS)){
  #subdiscipline
  field<-str_match(URLS[x],"vq=\\w+_(\\w+)")[2]
  
  #discipline - will need to translate
  discipline<-str_match(URLS[x],"vq=(\\w+)_\\w+")[2]
  
  xtab= readHTMLTable(URLS[x], header=T, which=1,stringsAsFactors=F)[,-1]
  out[[x]]<-data.frame(Discipline=discipline,Class=.simpleCap(field),xtab)
}

j_class<-rbind_all(out)

#Add table to the database
copy_to(my_db,j_class,"j_class",temporary = F)

#string querying needs to be cleaned?
#can't have + in title?
table(str_detect(j_class$Publication,"'+'"))

####Get Journal ID

#collapse into a string, formatting spaces with + and place a boolean OR in between each
serial<-j_class$Publication
s<-gsub(x=serial,replacement="\\+",pattern=" ")
s<-gsub(x=s,"\\&+","")

#needs to replace the the's in front of journal title
a<-which(sapply(s,word,sep="\\+")=="The")
for(x in a){
  words<-length(strsplit(s[x],"\\+")[[1]])
  torep<-word(s[x],start=2,end=words,sep="\\+")  
  s[x]<-torep
}

#Get the journal source ID
#journaldf<-list()

#search for scopus ID in parallel
cl<-makeCluster(2,"SOCK")
registerDoSNOW(cl)
journaldf<-foreach(x=1:length(s),.packages=c("httr","XML")) %dopar% {
    response<-getSourceID(inquery = s[x])
    return(parseSource(response,s[x]))
  }

#bind together, after ignoring the blank rows
journaldf<-journaldf[!sapply(journaldf,length)==1]
journaldf<-journaldf[!sapply(journaldf,nrow)==0]

journaldf<-rbind_all(journaldf)

journaldf<-journaldf[!duplicated(journaldf),]

#write to file
copy_to(my_db,journaldf,"journal_scopus",temporary = F)
#write.csv(journaldf,"C:/Users/Ben/Dropbox/FacultyNetwork/JournalID.csv")
