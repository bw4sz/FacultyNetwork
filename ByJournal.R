
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
library(igraph)

#source function 
source("Funtions.R")

###Journals
#read in data
j_class<-read.csv("Class.csv",row.names=1)

#Journal names are characters, are mutable
j_class$Publication<-as.character(j_class$Publication)

#some known errors that need to be handformatted
j_class[208,"Publication"]<-"Annual+Review+of+Plant+Biology"
j_class[209,"Publication"]<-"Plant, Cell and Environment"

#collapse into a string, formatting spaces with + and place a boolean OR in between each
serial<-j_class$Publication
s<-gsub(x=serial,replacement="\\+",pattern=" ")
s<-gsub(x=s,"\\&+","")

#couple malformed ones
s<-s[-c(19,767,121,645,595,563,485,371,157,147,133,132,130,131,137,127,144,142,157,134)]


#start with the auk, s=175
#needs to replace the the's in front of journal title
a<-which(sapply(s,word,sep="\\+")=="The")
for(x in a){
  words<-length(strsplit(s[x],"\\+")[[1]])
  torep<-word(s[x],start=2,end=words,sep="\\+")  
  s[x]<-torep
}

#create a data holder
dat<-list()
#,82

#Get issn that matches each

#0-200 is 2005 to 2015.
#beginning at 200, 1995 to 2015
#during the while loop, r=51, x=208 , error is   'names' attribute [1] must be the same length as the vector [0]

#Get the journal source ID
journaldf<-list()

for (x in 1:length(s)){
    response<-getSourceID(s[x])
    journaldf[[x]]<-parseSource(response,s[x])      
}

journaldf<-rbind_all(journaldf)

for (x in 3){
  print(x)
  #get articles from a journal and parse it
  q<-paste("source-id(",journaldf$ID[x],")",sep="")
  
  #call query
  responses<-allyears(query=q,yearrange=1995:2015)
  
    #parse result
  dat[[x]]<-responses
}

#bind journals, remove no matched
df<-rbind_all(dat[!lapply(dat,length)==1])

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

write.table(tocompare,"C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",append=F,sep=",",col.names=T,row.names=F)

#save.image("Journal.RData")
