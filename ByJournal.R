

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
library(bipartite)
library(doSNOW)
library(foreach)
library(igraph)

#source function 
source("Funtions.R")

#Read in Journal Class
journaldf<-read.csv("C:/Users/Ben/Dropbox/FacultyNetwork/JournalID.csv",row.names=1)


#create a data holder
#dat<-list()

cl<-makeCluster(10,"SOCK")
registerDoSNOW(cl)
dat<-foreach(x=601:608,.errorhandling = "pass",.packages=c("httr","XML","reshape2","plyr","dplyr","chron","stringr")) %dopar% {
  print(x)
  #get articles from a journal and parse it
  q<-paste("source-id(",journaldf$ID[x],")",sep="")
  
  #call query
  responses<-allyears(query=q,yearrange=1995:2015)
  
    #parse result
  #dat[[x]]<-responses
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
