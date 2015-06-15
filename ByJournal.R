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
j<-d %>% tbl("JA") %>% select(Journal) %>% distinct_() %>% collect()

comp<-dbGetQuery(d$con,"SELECT DISTINCT Journal 
            FROM JA,j_class
           WHERE JA.Journal = j_class.Publication")

comp<-dbGetQuery(d$con,"SELECT DISTINCT Journal 
            FROM JA")


journaldf<-
  tail(journaldf[!journaldf$title %in% j$Journal,])

#create a data holder

cl<-makeCluster(10,"SOCK")
registerDoSNOW(cl)
dat<-foreach(x=1:10,.errorhandling = "pass",.packages=c("httr","XML","reshape2","plyr","dplyr","chron","stringr")) %dopar% {
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
#correct runs have length of 7
df<-rbind_all(dat[lapply(dat,length)==7])

#Standardize capitalization
df$Journal<-sapply(df$Journal,.simpleCap)
j_class$Publication<-sapply(j_class$Publication,.simpleCap)

#legacy name change
tocompare<-df

#remerge the "The" in names, sorry bit of ugly code
levels(tocompare$Journal)[levels(tocompare$Journal) %in% gsub(x=s[a],pattern="\\+",replacement=" ")]<-paste("The",levels(tocompare$Journal)[levels(tocompare$Journal) %in% gsub(x=s[a],pattern="\\+",replacement=" ")])

#append journal classifier
tocompare<-droplevels(merge(tocompare,j_class,by.x="Journal",by.y="Publication"))

#turn unknowns to NA, it was just a place holder
tocompare[tocompare$Affiliation %in% "Unknown","Affiliation"]<-NA
tocompare[tocompare$Author %in% "Unknown","Author"]<-NA

write.table(tocompare,"C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",append=T,col.names=F,row.names=F,sep=",")

save.image("Journal.RData")