
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

#collapse into a string, formatting spaces with + and place a boolean OR in between each
serial<-j_class$Publication
s<-gsub(x=serial,replacement="\\+",pattern=" ")
s<-gsub(x=s,"\\&+","")

#couple malformed ones
s<-s[-c(19,767,645,595,563,485,371,157,147,133,132,130,127)]

##Departments
#depts<-read.csv("depts.csv")

#start with the auk, s=175
#needs to replace the the's in front of journal title
a<-which(sapply(s,word,sep="\\+")=="The")
for(x in a){
  words<-length(strsplit(s[x],"\\+")[[1]])
  torep<-word(s[x],start=2,end=words,sep="\\+")  
  s[x]<-torep
}

x=175

dat<-list()

for (x in c(100:110)){
  print(x)
  #get articles from a journal and parse it
  q<-paste("exactsrctitle(",s[x],")",sep="")
  
  #call query
  responses<-allyears(q,2010:2014)
  
  #parse result
  dat[[x]]<-responses
}

#remove blank
dat<-dat[!sapply(dat,length)==0]

datparse<-list()
#Parse results

for (x in 1:length(dat)){
  r<-lapply(dat[[x]],sc_parse)
  datparse[[x]]<-rbind_all(r[!sapply(r,length)==1])
}

#bind journals
df<-rbind_all(datparse)

##Filter Authors
#Cutoff for articles, atleast 2 in my little example
keep<-names(which(table(df$Author)>2))
tocompare<-droplevels(df[df$Author %in% keep,])

#Build Dissimilarity matrix
#Merge table classification
tocompare$Journal<-as.factor(tocompare$Journal)
#remerge the "The" in names, sorry bit of ugly code
levels(tocompare$Journal)[levels(tocompare$Journal) %in% gsub(x=s[a],pattern="\\+",replacement=" ")]<-paste("The",levels(tocompare$Journal)[levels(tocompare$Journal) %in% gsub(x=s[a],pattern="\\+",replacement=" ")])

#append journal classifier
tocompare<-droplevels(merge(tocompare,j_class,by.x="Journal",by.y="Publication"))

#turn unknowns to NA, it was just a place holder
tocompare[tocompare$Affiliation %in% "Unknown","Affiliation"]<-NA
tocompare[tocompare$Author %in% "Unknown","Author"]<-NA

write.csv(tocompare,"Data/ParsedData.csv")

save.image("Journal.RData")
