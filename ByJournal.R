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

#Read in journals that already run
# comp<-dbGetQuery(d$con,"SELECT DISTINCT Journal
# FROM JA,j_class
# WHERE UPPER(JA.Journal) = UPPER(j_class.Publication)")

j<-d %>% tbl("JA") %>% select(Journal) %>% distinct_() %>% collect()

journaldf<-journaldf[!journaldf$title %in% j$Journal,]

#make sure to not to get journal of biological chhmistry, it is badly malformed?
journaldf<-journaldf[!journaldf$title=="Journal of Biological Chemistry",]

#write a test query that you know works to ensure you have space
tq<-scquery("AUK","2014")

#create a data holder

#set placement of journal
jp<-read.table("Data/JournalSection.txt")$x

#update new
#how many journals to run?
#if last run ended in exceed query, run 0!
runs<-0
jp<-(max(jp)+1):(max(jp)+runs)

if (tq$status_code==200){

cl<-makeCluster(20,"SOCK")
registerDoSNOW(cl)
dat<-foreach(x=jp,.errorhandling = "pass",.packages=c("httr","XML","reshape2","plyr","dplyr","chron","stringr")) %dopar% {
  print(x)
  #get articles from a journal and parse it
  q<-paste("source-id(",journaldf$ID[x],")",sep="")
  
  #call query
  responses<-allyears(query=q,yearrange=1995:2014)
  
  #parse result
  return(responses)
}
stopCluster(cl)

#bind journals, remove no matched
#correct runs have length of 6
df<-rbind_all(dat[lapply(dat,length)==6])
  
#Standardize capitalization
df$Journal<-sapply(df$Journal,.simpleCap)

#remerge the "The" in names, sorry bit of ugly code
#levels(df$Journal)[levels(df$Journal) %in% gsub(x=s[a],pattern="\\+",replacement=" ")]<-paste("The",levels(df$Journal)[levels(df$Journal) %in% gsub(x=s[a],pattern="\\+",replacement=" ")])

#if we ran out of calls, figure out where, using a test query
tq<-scquery("AUK","2014")

if(!tq$status_code==200){
    sapply(dat,function(x){
    max(as.numeric(as.character(x$Year)))
  })
}

#turn unknowns to NA, it was just a place holder
df[df$Author %in% "Unknown","Author"]<-NA

#write.table(df,"C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",append=T,col.names=F,row.names=F,sep=",")

#write journal and DOI table to the JA table
towrite<-df %>%  distinct(DOI) %>% select(DOI,Journal)
db_insert_into(con=d$con,table="JA",values=as.data.frame(towrite))

towrite<-df %>%  distinct(DOI) %>% select(DOI,Author,Order,Citations,Year)
db_insert_into(con=d$con,table="Meta",values=as.data.frame(towrite))


write.table(jp,"Data/JournalSection.txt")
}

save.image("Journal.RData")