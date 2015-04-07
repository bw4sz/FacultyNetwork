
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
depts<-read.csv("depts.csv")

dat<-list()

for (x in 1:100){
  print(x)
  #get articles from a journal and parse it
  q<-paste("srctitle(",s[x],")+AND+","affil(Stony+Brook+University)",sep="")
  
  #call query
  response<-scquery(q)
  
  #parse result
  dat[[x]]<-sc_parse(response)  
}

#remove blank 
df<-rbind_all(dat[!sapply(dat,length)==1])

table(df$Author)

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
dis<-vegdist(siteXspp,"horn")

##Specialization
visweb(siteXspp)
plotweb(siteXspp)

#as one way.


