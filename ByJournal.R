
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

for (x in 175){
  print(x)
  #get articles from a journal and parse it
  q<-paste("exactsrctitle(",s[x],")",sep="")
  
  #call query
  responses<-allyears(q,2005:2008)
  
  #parse result
  dat[[x]]<-responses
}

#remove blank
dat<-dat[!sapply(dat,length)==0]

#Parse results
datparse<-lapply(dat,function(x){
  r<-lapply(x,sc_parse)
  return(rbind_all(r[!sapply(r,length)==1]))
})

#bind journals
df<-rbind_all(datparse)

##Filter Authors
sort(table(df$Author))

#Cutoff for articles, atleast 5 in my little example
keep<-names(which(table(df$Author)>1))
tocompare<-droplevels(df[df$Author %in% keep,])

#Build Dissimilarity matrix
#Merge table classification

#append journal classifier
tocompare<-droplevels(merge(tocompare,j_class,by.x="Journal",by.y="Publication"))
siteXspp<-as.data.frame.array(table(tocompare$Author,tocompare$Class))

##Biplot
biplot(prcomp(siteXspp,scale=T))

##Niche Overlap
#Horn's distance between authors
dis<-as.matrix(vegdist(siteXspp,"horn"))

##Specialization
visweb(siteXspp)
plotweb(siteXspp)

#as one way.
#only connect players 
#ig<-graph.adjacency(dis[dis<.75]<-0)

#bad.vs<-V(ig)[degree(ig)<20] #identify those vertices part of less than three edges
#ig.network<-delete.vertices(ig, bad.vs) #exclude them from the graph

save.image("Journal.RData")
