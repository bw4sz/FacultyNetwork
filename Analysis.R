
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
library(sna)
library(igraph)

#source function 
source("Funtions.R")

tocompare<-read.csv("C:/Users/Ben/Dropbox/FacultyNetwork/ParsedData.csv")

#filter authors
#distribution of publication
hist(table(tocompare$Author))
keep<-names(which(table(tocompare$Author)>4))
tocompare<-droplevels(tocompare[tocompare$Author %in% keep,])

#remove duplicates.
tocompare<-tocompare[!duplicated(tocompare),]

#in case the column names in there 
tocompare<-droplevels(tocompare[!tocompare$Journal %in% "DOI",])

dim(tocompare)

siteXspp<-as.data.frame.array(table(tocompare$Author,tocompare$Class))
dim(siteXspp)

##Biplot
#biplot(prcomp(siteXspp,scale=T))

##Niche Overlap
#Horn's distance between authors
# dis<-as.matrix(vegdist(siteXspp,"horn"))
# 
# #set to self is NA
# diag(dis)<-NA
# 
# ##Specialization
# #visweb(siteXspp)
# #plotweb(siteXspp)
# 
# #for each paper calculate niche overlap
# tolist<-split(tocompare,tocompare$DOI)
# niche.author<-lapply(tolist,function(x){
#   mean(dis[rownames(dis) %in% x$Author,colnames(dis) %in% x$Author],na.rm=TRUE)
# })
# niche.author<-melt(niche.author)
# 
# #name the columns
# names(niche.author)<-c("Niche.Overlap","DOI")
# 
# #merge back with data.frame
# dat<-merge(tocompare,niche.author,by="DOI",all=T)
# 
# #Citations are numeric
# dat$Citations<-as.numeric(dat$Citations)
# 
# #There are multiple entries per DOI, mean should be all the same
# d<-group_by(dat,DOI) %>% select(Journal,Citations,Class,h5.index,Niche.Overlap) %>% distinct()
# 
# ggplot(d[d$Citations>0,],aes(x=Niche.Overlap,y=Citations)) + geom_point() + theme_bw() + stat_smooth()

#########Class connections
siteXspp<-as.data.frame.array(table(tocompare$Author,tocompare$Class))

#Compare disciplines
topics<-1-as.matrix(vegdist(t(siteXspp),"horn"))
g<-graph.adjacency(topics,"undirected",weighted=TRUE)

g<-simplify(g)

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
layout1 <- layout.fruchterman.reingold(g)


V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam=E(g)$weight/max(E(g)$weight)
E(g)$color<-rgb(0,1,0,alpha=E(g)$weight/max(E(g)$weight),maxColorValue=1)

ramp <- colorRamp(c("blue","red"),alpha=T)

E(g)$color = apply(ramp(E(g)$weight), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255,alpha=T) )

# plot the graph in layout1
layout1<- layout.fruchterman.reingold(g)

#If you need to delete
g.copy <- delete.edges(g, which(E(g)$weight<.05))
#width
width<-(E(g.copy)$weight/max(E(g.copy)$weight))*8

#label sizes
V(g.copy)$label.cex <- V(g.copy)$degree / max(V(g.copy)$degree)*1+.2
plot(g.copy,vertex.size=6,edge.width=width)

wt <- walktrap.community(g, modularity=TRUE)
dend <- as.dendrogram(wt, use.modularity=TRUE)
plot(as.hclust(dend))

#Network analysis of journals as undirected graph
siteXspp_j<-as.data.frame.array(table(tocompare$Author,tocompare$Journal))

#Compare disciplines
topics<-1-as.matrix(vegdist(t(siteXspp_j),"horn"))
g<-graph.adjacency(topics,"undirected",weighted=TRUE)
g<-simplify(g)

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
layout1 <- layout.fruchterman.reingold(g)

#get h5 index for each journal
j<-V(g)$name
ramp <- colorRamp(c("white","black"),alpha=T)
ind<-filter(tocompare,Journal %in% j) %>% distinct(h5.index)
vertex_index<-sapply(j,function(x){ind[ind$Journal==x,"h5.index"]})

V(g)$color<-apply(ramp(vertex_index/max(vertex_index)), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255,alpha=T) )

V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA

egam=E(g)$weight/max(E(g)$weight)
ramp <- colorRamp(c("blue","red"),alpha=T)
E(g)$color = apply(ramp(E(g)$weight), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255,alpha=T) )

# plot the graph in layout1
layout1<- layout.fruchterman.reingold(g)
#plot.igraph(g,edge.width=E(g)$weight/max(E(g)$weight)*2,layout=layout1)

#If you need to delete
g.copy <- delete.edges(g, which(E(g)$weight <.05))
V(g.copy)$label.cex <- V(g.copy)$degree / max(V(g.copy)$degree)+ .1
plot(g.copy)

#Centrality of Journals
betweenJ<-sort(betweenness(g.copy))

wt <- walktrap.community(g, modularity=TRUE)
dend <- as.dendrogram(wt, use.modularity=TRUE)
plot(as.hclust(dend))

###Connectance through time

yearcompare<-split(tocompare,tocompare$Year)

yeardat<-lapply(yearcompare,function(x){
  siteXspp<-as.data.frame.array(table(x$Author,x$Class))
  
  #Compare disciplines
  topics<-1-as.matrix(vegdist(t(siteXspp),"horn"))
  diag(topics)<-NA
  return(topics)
})

yeardat<-melt(yeardat)

colnames(yeardat)<-c("To","From","Niche.Overlap","Year")

#remove very weak connections
#work from an example data
#make into characters
yeardat$To<-as.character(yeardat$To)
yeardat$From<-as.character(yeardat$From)

exdat<-group_by(yeardat,To,From) %>% filter(To=="Ecology",Niche.Overlap>0.02)
exdat$Combo<-paste(exdat$To,exdat$From,sep="-")
ggplot(exdat,aes(x=Year,y=Niche.Overlap,col=Combo)) + geom_point() + geom_smooth(aes(group=Combo)) + facet_wrap(~Combo,scales="free")

#trend estimation
#first pass just fit a linear line and determine if its possitive or negative
yeardat$combo<-paste(sort(c(yeardat$To,yeardat$From)))
lm(data=yeardat,(Niche.Overlap~Year:paste(To,From)))

#make time series
#estimate trend
#if trend is changing is positive label

###D3 Visualizations

###Which groups are most specailized?


save.image("Analysis.RData")
