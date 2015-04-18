
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

tocompare<-read.csv(tocompare,"Data/ParsedData.csv")

siteXspp<-as.data.frame.array(table(tocompare$Author,tocompare$Class))

##Biplot
biplot(prcomp(siteXspp,scale=T))

##Niche Overlap
#Horn's distance between authors
dis<-as.matrix(vegdist(siteXspp,"horn"))

#set to self is NA
diag(dis)<-NA

##Specialization
#visweb(siteXspp)
plotweb(siteXspp)

#for each paper calculate niche overlap
tolist<-split(tocompare,tocompare$DOI)
niche.author<-lapply(tolist,function(x){
  mean(dis[rownames(dis) %in% x$Author,colnames(dis) %in% x$Author],na.rm=TRUE)
})
niche.author<-melt(niche.author)

#name the columns
names(niche.author)<-c("Niche.Overlap","DOI")

#merge back with data.frame
dat<-merge(tocompare,niche.author,by="DOI",all=T)

#There are multiple entries per DOI, mean should be all the same
d<-group_by(dat,DOI) %>% select(Journal,Citations,Class,h5.index,Niche.Overlap) %>% distinct()

ggplot(d[d$Citations>0,],aes(x=Niche.Overlap,y=Citations)) + geom_point() + theme_bw() + stat_smooth()

#Network analysis of disciplines as undirected graph.
save.image("Analysis.RData")