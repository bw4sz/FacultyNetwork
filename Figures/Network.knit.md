---
title: "Academic Betadiversity and Future of Biology Research"
author: "Ben Weinstein"
date: "Thursday, April 23, 2015"
output: 
html_document: 
  toc: yes
---



```r
#Scopus Database Query By Journal Article
library(XML)
library(proto)
library(broom)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(httr)
library(stringr)
library(chron)
library(vegan)
library(RSQLite)
library(knitr)
library(bipartite)
library(sna)
library(igraph)
library(knitr)
library(gridExtra)
library(GGally)
library(stringr)
library(networkD3)
```



```r
#source functions
source("Funtions.R")

#set knitr options
opts_chunk$set(echo=T,cache=F,fig.align='center',fig.height=12,fig.width=14,warning=F,message=F)
```

#Read in data. Processed from ByJournal.R


```r
#open database
d<-src_sqlite(path = "C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3")

#what does the data look like?
dbGetQuery(d$con, "SELECT * FROM JA limit 10")
```

```
##                      DOI                       Journal
## 1  SCOPUS_ID:84919714912   Plant Biotechnology Journal
## 2   SCOPUS_ID:0005764719 Agricultural Water Management
## 3   SCOPUS_ID:0028794786 Agricultural Water Management
## 4   SCOPUS_ID:0028794953 Agricultural Water Management
## 5   SCOPUS_ID:0028812064 Agricultural Water Management
## 6   SCOPUS_ID:0028813096 Agricultural Water Management
## 7   SCOPUS_ID:0028813684 Agricultural Water Management
## 8   SCOPUS_ID:0028854075 Agricultural Water Management
## 9   SCOPUS_ID:0028870250 Agricultural Water Management
## 10  SCOPUS_ID:0028885292 Agricultural Water Management
```

```r
dbGetQuery(d$con, "SELECT * FROM Meta limit 10")
```

```
##                      DOI Order      Author Citations Year
## 1  SCOPUS_ID:84919714912     1 55540061400         0 2015
## 2   SCOPUS_ID:0005764719     1  6506468386        11 1995
## 3   SCOPUS_ID:0005764719     2  6506622226        11 1995
## 4   SCOPUS_ID:0028794786     1  7006418829        50 1995
## 5   SCOPUS_ID:0028794786     2  7003590574        50 1995
## 6   SCOPUS_ID:0028794953     1 56574084900        11 1995
## 7   SCOPUS_ID:0028812064     1 55742487300        26 1995
## 8   SCOPUS_ID:0028813096     2  7004903137        26 1995
## 9   SCOPUS_ID:0028813096     1  7003803780        26 1995
## 10  SCOPUS_ID:0028813684     1  7005943986        48 1995
```

```r
#Order seems to be an issue, lets create a table that just merges the two column and keeps the larger number.

dbGetQuery(d$con, "CREATE TEMP TABLE m AS SELECT * FROM Meta WHERE NOT Year = 2015")

dbGetQuery(d$con,"UPDATE m SET Author = `Order` WHERE AUTHOR < 100 ")

#we want to remove authors that have few publications, atleast 4
sq<-"DELETE FROM m WHERE Author in (SELECT Author FROM (SELECT Author, Count(*) as p FROM m GROUP BY Author HAVING p < 4))"
auth<-dbGetQuery(d$con, sq)
```

Basic data cleaning. We only want records from active authors. Atleast 5 publications in the entire record.


```r
#i want to do this in sql, but right now i'm just better in R
j_class<-dbGetQuery(d$con,"SELECT * From j_class")

jscopus<-dbGetQuery(d$con,"SELECT * FROM journal_scopus")

JA<-dbGetQuery(d$con,"Select * FROM JA")

m<-dbGetQuery(d$con,"SELECT * FROM m")

# we need metadata for each of the classes, some of the spelling conflicts.
head(jscopus)
```

```
##                                    title     ID
## 1                   Analytical Chemistry  23915
## 2            Journal of Chromatography A 130000
## 3          Biosensors and Bioelectronics  15437
## 4                 Analytica Chimica Acta  23911
## 5                     Clinical Chemistry  26786
## 6 Analytical and Bioanalytical Chemistry  23913
##                                    query
## 1                   Analytical+Chemistry
## 2            Journal+of+Chromatography+A
## 3          Biosensors+and+Bioelectronics
## 4                 Analytica+Chimica+Acta
## 5                     Clinical+Chemistry
## 6 Analytical+and+Bioanalytical+Chemistry
```

```r
head(j_class)
```

```
##   Discipline               Class                   Publication h5.index
## 1        chm Analyticalchemistry          Analytical Chemistry      102
## 2        chm Analyticalchemistry   Journal of Chromatography A       79
## 3        chm Analyticalchemistry Biosensors and Bioelectronics       78
## 4        chm Analyticalchemistry                 Lab on a Chip       77
## 5        chm Analyticalchemistry        Analytica Chimica Acta       75
## 6        chm Analyticalchemistry            Clinical Chemistry       74
##   h5.median
## 1       132
## 2       104
## 3       100
## 4       104
## 5       105
## 6       109
```

```r
j_class$Source<-toupper(j_class$Publication)
jscopus$Source<-toupper(jscopus$title)

#which need the "the added back"
jscopus$Source[!jscopus$Source %in% j_class$Source]<-paste("THE",jscopus$Source[!jscopus$Source %in% j_class$Source])

jc<-merge(j_class,jscopus)

#remove duplicates
jc<-jc[!duplicated(jc[,"Source" ]),]

copy_to(d,jc,"jc",temporary = T)
```

```
## Source: sqlite 3.8.6 [C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3]
## From: jc [2,393 x 9]
## 
##                                 Source Discipline
## 1          ACADEMIC EMERGENCY MEDICINE        med
## 2                    ACADEMIC MEDICINE        soc
## 3        ACADEMY OF MANAGEMENT JOURNAL        soc
## 4         ACADEMY OF MANAGEMENT REVIEW        soc
## 5                   ACCOUNTING HISTORY        soc
## 6            ACCOUNTING HISTORY REVIEW        soc
## 7        ACCOUNTS OF CHEMICAL RESEARCH        chm
## 8                        ACS CATALYSIS        chm
## 9                             ACS NANO        chm
## 10 ACTA ANAESTHESIOLOGICA SCANDINAVICA        med
## ..                                 ...        ...
## Variables not shown: Class (chr), Publication (chr), h5.index (chr),
##   h5.median (chr), title (chr), ID (chr), query (chr)
```



```r
#This takes forever but it works...
#siteXspp<-dbGetQuery(d$con,"SELECT Author, SOURCE, JA.DOI, title, Discipline, Class, Year FROM JA JOIN jc ON UPPER(jc.title) = UPPER(Journal) Join m ON m.DOI=JA.DOI")
```


```r
S<-merge(m,JA,by="DOI")
S<-S[,colnames(S) %in% c("DOI","Author","Year","Journal")]
S$jname<-toupper(S$Journal)
jc$jname<-toupper(jc$title)

system.time(tocompare<-merge(S,jc[,colnames(jc) %in% c("title","jname","Source","Discipline","Class")],by.x="jname"))
```

```
##    user  system elapsed 
##  142.69    0.49  143.19
```

#Descriptive statistics
How many journals


```r
paste("Number of Journals:",length(unique(tocompare$Journal)))
```

```
## [1] "Number of Journals: 900"
```

```r
#How many authors
paste("Number of Authors:",length(unique(tocompare$Author)))
```

```
## [1] "Number of Authors: 452449"
```

```r
#How many papers
paste("Number of Papers:",length(unique(tocompare$DOI)))
```

```
## [1] "Number of Papers: 2105193"
```

```r
ta<-sort(table(tocompare$Journal))
print("Most published journals")
```

```
## [1] "Most published journals"
```

```r
tail(ta)
```

```
## 
##                             Atmospheric Environment 
##                                               46196 
##                       Molecular Biology Of The Cell 
##                                               54674 
##                              Nucleic Acids Research 
##                                               58270 
##                            Journal Of Power Sources 
##                                               65689 
## Biochemical And Biophysical Research Communications 
##                                              121946 
##                      Molecular And Cellular Biology 
##                                              183114
```

##How many papers from each discipline over time?


```r
class_year<-melt(table(tocompare$Class,tocompare$Year))
colnames(class_year)<-c("Class","Year","Papers")

ggplot(class_year,aes(x=as.factor(Year),y=Papers,group=Class)) + geom_line() + theme_bw() + facet_wrap(~Class,scale="free_y",ncol=2) + labs(x="Year")
```

<img src="./Network_files/figure-html/unnamed-chunk-8.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="1152" style="display: block; margin: auto;" />

```r
ggsave("Figures/Papers_Year.svg",dpi=300)
```

Create matrix of authors in each class - analagous to the site by species matrix used in ecology


```r
siteXspp<-as.data.frame.array(table(tocompare$Author,tocompare$Class))
dim(siteXspp)
```

```
## [1] 452448     82
```

#Dissimalarity among classes

Use the abundance of papers by each author to calculate niche overlap (dist=1-Horn's) between classes. 

Low overlap=0
High overlap=1



```r
#Compare disciplines
topics<-1-as.matrix(vegdist(t(siteXspp),"horn"))
```

##Visualize interactions

  * Stronger interactions are more red.
  * Weaker interactions are blue.
  * Stronger connections are thicker
  * Weaker connections are thinner
  * More central members (greater number of partners) are in larger text
  * Less central members (fewer number of partners) are in smaller text
  

```r
g<-graph.adjacency(topics,"undirected",weighted=TRUE)

g<-simplify(g)

# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam=E(g)$weight/max(E(g)$weight)

ramp <- colorRamp(c("gray90","black"),alpha=T)

E(g)$color = apply(ramp(E(g)$weight/max(E(g)$weight)), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255,alpha=T) )


#If you need to delete
g.copy <- delete.edges(g, which(E(g)$weight<.05))
#width
width<-(E(g.copy)$weight/max(E(g.copy)$weight))*8

#label sizes
V(g.copy)$degree <- degree(g.copy)
V(g.copy)$label.cex <- V(g.copy)$degree / max(V(g.copy)$degree)*.5+.5

#get vertex size
size<-group_by(tocompare,Class) %>% summarize(Papers=length(unique(DOI)))
size<-round(as.numeric(table(tocompare$Class))/max(table(tocompare$Class))*5)+2

#plot network
layout1 <- layout.fruchterman.reingold(g.copy,niter=10000,area=vcount(g.copy)^2.3)

plot(g.copy,edge.width=width,vertex.size=size,layout=layout1,vertex.color="grey40")
```

<img src="./Network_files/figure-html/unnamed-chunk-111.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" width="1344" style="display: block; margin: auto;" />

```r
#try coloring nodes by discipline
clookup<-jc %>% group_by(Class,Discipline) %>% distinct() %>% select(Class,Discipline)

#randomly assign class for duplicate
clookup<-clookup[!duplicated(clookup$Class),]
V(g.copy)$color<-as.numeric(as.factor(merge(data.frame(Class=V(g.copy)$label),clookup)$Discipline))

plot(g.copy,edge.width=width,vertex.size=size,layout=layout1)
```

<img src="./Network_files/figure-html/unnamed-chunk-112.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" width="1344" style="display: block; margin: auto;" />


```r
#save full  network
svg(filename = "Figures/Overall_NetworkIgraph.svg",width=10,height=10)
plot(g.copy,edge.width=width,vertex.size=size,layout=layout1)
dev.off()

jpeg(filename = "Figures/Overall_NetworkIgraph.jpeg",res=600,height=12,width=10,units="in")
plot(g.copy,edge.width=width,vertex.size=size,layout=layout1,vertex.color="grey40")
dev.off()
```

##View as a dendrogram


```r
wt <- walktrap.community(g, modularity=TRUE)
dend <- as.dendrogram(wt, use.modularity=TRUE)
plot(as.hclust(dend))
```

<img src="./Network_files/figure-html/unnamed-chunk-13.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" width="1344" style="display: block; margin: auto;" />

#Calculate network statistics

We are interested in the centrality, modularity and compartamentalization of the biological sciences

  * Betweenness - number of shortest path that includes node. This tends to find gatekeeps among components.
  * Degree - number of connections for each node
  * Eigenvector centrality: A node is important if it is connected to many important nodes. A node with a small number of influential contacts will be better than a larger number of mediocre contacts.

```r
between_class<-betweenness(g.copy)
degree_class<-degree(g.copy)
eigenV<-evcent(g.copy)$vector
vdat<-data.frame(Class=names(between_class),Between=between_class,Degree=degree_class,Eigen=eigenV)
```

##Correlation among measures.


```r
ggpairs(vdat[,-1])
```

<img src="./Network_files/figure-html/unnamed-chunk-15.png" title="plot of chunk unnamed-chunk-15" alt="plot of chunk unnamed-chunk-15" width="672" style="display: block; margin: auto;" />

```r
#reoroder levels
vdat$Class<-factor(vdat$Class,levels=vdat[order(vdat$Between,vdat$Degree),"Class"])
```

##Top actors for each statistic


```r
size<-data.frame(Class=colnames(siteXspp),Papers=apply(siteXspp,2,sum))

#merge with disciplines
size<-merge(size,clookup)

vdat<-merge(size,vdat)

vdat$Insularity<-vdat$Degree/vdat$Papers
mdat<-melt(vdat)

colnames(mdat)<-c("Class","Discipline","Metric","Score")

#order and plot
dt<-group_by(mdat,Metric) %>% mutate(Svalue=as.numeric(scale(Score))) %>% group_by(Metric,Class) %>% arrange(Score)

#sort by betweenness
ord<-dcast(dt,Class~Metric) %>% select(Class,Between,Degree) %>% arrange(Between,Degree) %>% select(Class)

dt$Class<-factor(dt$Class,levels=ord$Class)
ggplot(dt[dt$Metric %in% c("Between","Degree","Insularity"),],aes(y=Class,x=Svalue)) + geom_bar_horz(stat="identity",position="identity") + facet_grid(.~Metric,scale="free_x") + labs(x="Z-score",y="Biological Field") + theme_bw()
```

<img src="./Network_files/figure-html/unnamed-chunk-161.png" title="plot of chunk unnamed-chunk-16" alt="plot of chunk unnamed-chunk-16" width="1344" style="display: block; margin: auto;" />

```r
#try by discipline -

ggplot(dt[dt$Metric %in% c("Between","Degree","Insularity"),],aes(y=Class,x=Svalue)) + geom_bar_horz(stat="identity",position="identity") + facet_grid(Discipline~Metric,scale="free") + labs(x="Z-score",y="Biological Field") + theme_bw()
```

<img src="./Network_files/figure-html/unnamed-chunk-162.png" title="plot of chunk unnamed-chunk-16" alt="plot of chunk unnamed-chunk-16" width="1344" style="display: block; margin: auto;" />

```r
ggsave("Figures/OveralMetrics.jpg",dpi=300,height=10,width=12)
ggsave("Figures/OveralMetrics.svg",dpi=300)
```


```r
d<-dcast(dt,Discipline + Class~Metric)
#merge with papers
ds<-merge(d,size,by=c("Class","Discipline"))

ggplot(ds,aes(x=Between,y=Degree,col=Discipline,label=Class,size=log(Papers.y))) + geom_point(alpha=.3) + geom_abline() + geom_text(data=ds[ds$Between > 0 | ds$Degree > 0,],size=2.5,col="black") + theme_bw() + scale_size(range=c(2,13)) +labs(size="Publications") + coord_fixed()
```

<img src="./Network_files/figure-html/unnamed-chunk-17.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" width="1344" style="display: block; margin: auto;" />

##Average connectedness and degree and link strength

```r
group_by(mdat,Metric) %>% summarize(mean=mean(Score))
```

```
## Source: local data frame [5 x 2]
## 
##       Metric      mean
## 1     Papers 4.784e+04
## 2    Between 6.288e+01
## 3     Degree 3.634e+00
## 4      Eigen 8.998e-02
## 5 Insularity 1.381e-04
```

```r
overall.density<-graph.density(g.copy)
mean(E(g.copy)$weight)
```

```
## [1] 0.1073
```

#Network stats over time

```r
#split into a time frame 5 years?

m<-seq(1995,2014,2)

tocompare$Time<-cut(as.numeric(as.character(tocompare$Year)),breaks=m,labels=m[1:length(m)-1])

yearcompare<-split(tocompare,tocompare$Time)

#all over 

#caculate degree distribution
dd<-melt(sapply(yearcompare,CalcDD))
ggplot(dd,aes(x=Var1,y=value,col=as.factor(L1))) + geom_line(size=.5) + geom_point(size=4,aes(group=as.factor(L1))) + theme_bw() + xlab("Node Degree") + ggtitle("Degree Distribution") + labs(col="Year") 
```

<img src="./Network_files/figure-html/unnamed-chunk-191.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" />

```r
yearstats<-lapply(yearcompare,calcN)
```

<img src="./Network_files/figure-html/unnamed-chunk-192.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-193.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-194.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-195.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-196.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-197.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-198.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-199.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" /><img src="./Network_files/figure-html/unnamed-chunk-1910.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" />

```r
yearstats<-melt(yearstats)
ggplot(yearstats,aes(x=L1,y=value,col=Class)) + geom_point() +geom_line(aes(group=Class)) + facet_wrap(~variable,scales="free",ncol=1)
```

<img src="./Network_files/figure-html/unnamed-chunk-1911.png" title="plot of chunk unnamed-chunk-19" alt="plot of chunk unnamed-chunk-19" width="864" style="display: block; margin: auto;" />

##Connectance through time


```r
yeardat<-lapply(yearcompare,function(x){
  siteXspp<-droplevels(as.data.frame.array(table(x$Author,x$Class)))
  
  #drop empty rows and colums
  siteXspp<-siteXspp[rownames(siteXspp) %in% names(which(apply(siteXspp,1,sum) > 0)),colnames(siteXspp) %in% names(which(apply(siteXspp,2,sum) > 0))]
  
  #Compare disciplines
  topics<-1-as.matrix(vegdist(t(siteXspp),"horn"))
  diag(topics)<-NA
  topics[upper.tri(topics)]<-NA
  return(topics)
})

yeardat<-melt(yeardat)
colnames(yeardat)<-c("To","From","Niche.Overlap","Year")
```

Remove extremely weak connections 


```r
#remove very weak connections
#work from an example data
#make into characters
yeardat$To<-as.character(yeardat$To)
yeardat$From<-as.character(yeardat$From)

#remove weak connections
exdat<-group_by(yeardat,To,From) %>% filter(Niche.Overlap>0.05 & !is.na(Niche.Overlap))

exdat$Combo<-paste(exdat$To,exdat$From,sep="-")

#plot
ggplot(exdat,aes(x=Year,y=Niche.Overlap,col=From)) + geom_point() + geom_line(aes(group=Combo)) + facet_wrap(~To,scales="free",ncol=3) + theme_bw() + geom_text(data=exdat[exdat$Year==m[length(m)/2],],aes(label=From),size=4) 
```

<img src="./Network_files/figure-html/unnamed-chunk-21.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" width="1248" style="display: block; margin: auto;" />

```r
ggsave("Figures/LinkTime.svg",dpi=300)
ggsave("Figures/LinkTime.jpeg",height=10,width=14,dpi=300)
```

#Trend estimation

Its not clear to me if i need to use a time-series model. The connection at time A should be independent of connection at time A+1. While they may be related due to the trend - there is no mechanistic connection among publications between years. Its not like a population, where the number of available producers directly influences the offspring in the next year. For the moment, i'm just using a linear model with year as a continious variable. This probably needs to change.



```r
exdat$Combo<-paste(exdat$To,exdat$From,sep="-")

#year is a number value for the moment
exdat$Year<-as.numeric(exdat$Year)-1995

sdat<-split(exdat,exdat$Combo)

#get rid of combinations with less than ten years of points
sdat<-sdat[lapply(sdat,nrow) > 2]

#
# Break up d by state, then fit the specified model to each piece and
# return a list
tmod<-rbind_all(lapply(sdat,function(df){
  tdat<-tidy(lm(Niche.Overlap ~ Year, data = df))
  tdat<-tdat[tdat$term =="Year",]
  tdat$Combo<-unique(df$Combo)
  return(tdat)
}))

#extract names into columns
tmod$To<-str_match(tmod$Combo,"(.*)-")[,2]
tmod$From<-str_match(tmod$Combo,"-(.*)")[,2]
```

##Visualize effect over time

Positive values are significantly increasing connections
Negative values are significantly decreasing connections


```r
tmod$svalue<-scale(tmod$estimate)

ggplot(tmod[tmod$p.value < 0.05 & tmod$term=="Year",],aes(x=To,y=From,fill=estimate)) + geom_tile() + theme_bw() + scale_fill_gradientn(colours=c("blue","gray","red"),limits=c(-max(tmod$estimate),max(tmod$estimate))) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(x="",y="")
```

<img src="./Network_files/figure-html/unnamed-chunk-23.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" width="1344" style="display: block; margin: auto;" />

```r
ggsave("Figures/InteractionsTime.svg",dpi=300,height=5,width=8)
ggsave("Figures/InteractionsTime.jpeg",height=5,width=8) 
```

#D3 Visualizations

The wonderful D3 package connections!


```r
MisLinks<-melt(topics)

#remove very weak connections
MisLinks<-MisLinks[MisLinks$value > 0.05,]
MisLinks$value<-MisLinks$value*10
colnames(MisLinks)<-c("To","From","value")
MisLinks$To<-as.character(MisLinks$To)
MisLinks$From<-as.character(MisLinks$From)

MisNodes<-data.frame(name=as.factor(sort(as.character(unique(c(MisLinks$To,MisLinks$From))))))
#Add groups
MisNodes$group<-as.integer(1)

MisLinks$source<-as.integer(sapply(MisLinks$To,function(x) which(x ==MisNodes$name)))-1
MisLinks$target<-as.integer(sapply(MisLinks$From,function(x) which(x ==MisNodes$name)))-1

#Order by source
MisLinks<-MisLinks[order(MisLinks$source),]

simpleNetwork(MisLinks,fontSize = 15)
```

<!--html_preserve--><div id="htmlwidget-2841" style="width:1344px;height:1152px;" class="simpleNetwork"></div>
<script type="application/json" data-for="htmlwidget-2841">{ "x": {
 "links": {
 "source": [ "Addiction", "Addiction", "Agronomycropscience", "Agronomycropscience", "Agronomycropscience", "Agronomycropscience", "Agronomycropscience", "Agronomycropscience", "Agronomycropscience", "Agronomycropscience", "Aidshiv", "Aidshiv", "Aidshiv", "Aidshiv", "Alternativetraditionalmedicine", "Analyticalchemistry", "Analyticalchemistry", "Analyticalchemistry", "Analyticalchemistry", "Anesthesiology", "Anesthesiology", "Animalbehavior", "Animalbehavior", "Animalbehavior", "Animalbehavior", "Animalbehavior", "Animalbehavior", "Animalbehavior", "Animalhusbandry", "Animalhusbandry", "Animalhusbandry", "Atmosphericsciences", "Atmosphericsciences", "Atmosphericsciences", "Atmosphericsciences", "Atmosphericsciences", "Audiologyspeechlanguagepathology", "Audiologyspeechlanguagepathology", "Biochemistry", "Biochemistry", "Biochemistry", "Biochemistry", "Biochemistry", "Biochemistry", "Biochemistry", "Biochemistry", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Biodiversityconservationbiology", "Bioethics", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Biogeneral", "Bioinformatics", "Bioinformatics", "Bioinformatics", "Bioinformatics", "Bioinformatics", "Biomedicaltechnology", "Biomedicaltechnology", "Biophysics", "Biophysics", "Biophysics", "Biophysics", "Biophysics", "Biophysics", "Biophysics", "Biophysics", "Biotechnology", "Biotechnology", "Biotechnology", "Biotechnology", "Biotechnology", "Biotechnology", "Birds", "Birds", "Birds", "Birds", "Birds", "Birds", "Botany", "Botany", "Botany", "Botany", "Botany", "Botany", "Botany", "Botany", "Botany", "Cardiology", "Cellbiology", "Cellbiology", "Cellbiology", "Cellbiology", "Cellbiology", "Cellbiology", "Ceramicengineering", "Ceramicengineering", "Chemicalkineticscatalysis", "Chemicalkineticscatalysis", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Chmgeneral", "Clinicallaboratoryscience", "Combustionpropulsion", "Communicablediseases", "Communicablediseases", "Communicablediseases", "Communicablediseases", "Compositematerials", "Compositematerials", "Corrosion", "Criticalcare", "Criticalcare", "Crystallographystructuralchemistry", "Crystallographystructuralchemistry", "Developmentalbiologyembryology", "Developmentalbiologyembryology", "Developmentalbiologyembryology", "Developmentalbiologyembryology", "Developmentalbiologyembryology", "Developmentalbiologyembryology", "Developmentalbiologyembryology", "Dispersionchemistry", "Dispersionchemistry", "Dispersionchemistry", "Dispersionchemistry", "Dispersionchemistry", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Ecology", "Electrochemistry", "Electrochemistry", "Electrochemistry", "Electrochemistry", "Electrochemistry", "Electrochemistry", "Environmentalgeologicalengineering", "Environmentalgeologicalengineering", "Environmentalgeologicalengineering", "Environmentalgeologicalengineering", "Environmentallawpolicy", "Environmentallawpolicy", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Environmentalsciences", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Evolutionarybiology", "Foodsciencetechnology", "Foodsciencetechnology", "Foodsciencetechnology", "Forestsforestry", "Forestsforestry", "Forestsforestry", "Forestsforestry", "Forestsforestry", "Forestsforestry", "Forestsforestry", "Geneticsgenomics", "Geneticsgenomics", "Geneticsgenomics", "Geneticsgenomics", "Geochemistrymineralogy", "Geochemistrymineralogy", "Geology", "Geology", "Geology", "Geology", "Geology", "Heartthoracicsurgery", "Hydrology", "Hydrology", "Hydrology", "Hydrology", "Inorganicchemistry", "Inorganicchemistry", "Inorganicchemistry", "Inorganicchemistry", "Insectsarthropods", "Insectsarthropods", "Insectsarthropods", "Insectsarthropods", "Insectsarthropods", "Insectsarthropods", "Insectsarthropods", "Marinesciencesfisheries", "Marinesciencesfisheries", "Marinesciencesfisheries", "Marinesciencesfisheries", "Marinesciencesfisheries", "Marinesciencesfisheries", "Marinesciencesfisheries", "Materialsengineering", "Materialsengineering", "Materialsengineering", "Materialsengineering", "Materialsengineering", "Materialsengineering", "Materialsengineering", "Medicalinformatics", "Medicinalchemistry", "Medicinalchemistry", "Microbiology", "Microbiology", "Microbiology", "Microbiology", "Microbiology", "Microbiology", "Molecularbiology", "Molecularbiology", "Molecularbiology", "Molecularbiology", "Molecularbiology", "Molecularbiology", "Molecularbiology", "Molecularbiology", "Molecularbiology", "Molecularmodeling", "Molecularmodeling", "Molecularmodeling", "Mycology", "Mycology", "Nanotechnology", "Nanotechnology", "Nanotechnology", "Nanotechnology", "Nanotechnology", "Nanotechnology", "Nanotechnology", "Nanotechnology", "Naturalmedicinesmedicinalplants", "Naturalmedicinesmedicinalplants", "Nursing", "Nutritionscience", "Oceanography", "Oceanography", "Oceanography", "Oceanography", "Oceanography", "Oceanography", "Oilpetroleumnaturalgas", "Oncology", "Oncology", "Oncology", "Oncology", "Oncology", "Oncology", "Organicchemistry", "Organicchemistry", "Organicchemistry", "Organicchemistry", "Organicchemistry", "Otolaryngology", "Otolaryngology", "Paleontology", "Paleontology", "Pestcontrolpesticides", "Pestcontrolpesticides", "Pestcontrolpesticides", "Pestcontrolpesticides", "Plantpathology", "Plantpathology", "Plantpathology", "Plantpathology", "Plantpathology", "Plantpathology", "Plantpathology", "Polymersplastics", "Polymersplastics", "Polymersplastics", "Polymersplastics", "Polymersplastics", "Polymersplastics", "Proteomicspeptides", "Proteomicspeptides", "Proteomicspeptides", "Proteomicspeptides", "Proteomicspeptides", "Proteomicspeptides", "Psychology", "Publichealth", "Publichealth", "Publichealth", "Soilsciences", "Soilsciences", "Soilsciences", "Soilsciences", "Soilsciences", "Sustainabledevelopment", "Sustainabledevelopment", "Sustainabledevelopment", "Sustainabledevelopment", "Sustainabledevelopment", "Sustainabledevelopment", "Sustainableenergy", "Sustainableenergy", "Sustainableenergy", "Sustainableenergy", "Sustainableenergy", "Toxicology", "Tropicalmedicineparasitology", "Veterinarymedicine", "Veterinarymedicine", "Veterinarymedicine", "Virology", "Virology", "Virology", "Virology", "Virology", "Woodsciencetechnology", "Zoology", "Zoology", "Zoology", "Zoology", "Zoology", "Zoology", "Zoology" ],
"target": [ "Addiction", "Publichealth", "Agronomycropscience", "Botany", "Environmentalsciences", "Forestsforestry", "Geneticsgenomics", "Pestcontrolpesticides", "Plantpathology", "Soilsciences", "Aidshiv", "Communicablediseases", "Publichealth", "Virology", "Alternativetraditionalmedicine", "Analyticalchemistry", "Chmgeneral", "Electrochemistry", "Proteomicspeptides", "Anesthesiology", "Criticalcare", "Animalbehavior", "Biodiversityconservationbiology", "Birds", "Ecology", "Evolutionarybiology", "Insectsarthropods", "Zoology", "Animalhusbandry", "Foodsciencetechnology", "Veterinarymedicine", "Atmosphericsciences", "Biogeneral", "Environmentalsciences", "Hydrology", "Oceanography", "Audiologyspeechlanguagepathology", "Otolaryngology", "Biochemistry", "Biogeneral", "Biophysics", "Cellbiology", "Developmentalbiologyembryology", "Molecularbiology", "Oncology", "Proteomicspeptides", "Animalbehavior", "Biodiversityconservationbiology", "Biogeneral", "Birds", "Botany", "Ecology", "Environmentalsciences", "Evolutionarybiology", "Forestsforestry", "Insectsarthropods", "Marinesciencesfisheries", "Sustainabledevelopment", "Zoology", "Bioethics", "Atmosphericsciences", "Biochemistry", "Biodiversityconservationbiology", "Biogeneral", "Bioinformatics", "Biophysics", "Botany", "Cellbiology", "Developmentalbiologyembryology", "Ecology", "Environmentalsciences", "Evolutionarybiology", "Geology", "Marinesciencesfisheries", "Microbiology", "Molecularbiology", "Oceanography", "Oncology", "Virology", "Biogeneral", "Bioinformatics", "Biophysics", "Evolutionarybiology", "Proteomicspeptides", "Biomedicaltechnology", "Biophysics", "Biochemistry", "Biogeneral", "Bioinformatics", "Biomedicaltechnology", "Biophysics", "Chmgeneral", "Molecularbiology", "Proteomicspeptides", "Biotechnology", "Environmentalgeologicalengineering", "Environmentalsciences", "Foodsciencetechnology", "Microbiology", "Sustainableenergy", "Animalbehavior", "Biodiversityconservationbiology", "Birds", "Ecology", "Evolutionarybiology", "Zoology", "Agronomycropscience", "Biodiversityconservationbiology", "Biogeneral", "Botany", "Ecology", "Evolutionarybiology", "Forestsforestry", "Geneticsgenomics", "Plantpathology", "Cardiology", "Biochemistry", "Biogeneral", "Cellbiology", "Developmentalbiologyembryology", "Molecularbiology", "Oncology", "Ceramicengineering", "Materialsengineering", "Chemicalkineticscatalysis", "Chmgeneral", "Analyticalchemistry", "Biophysics", "Chemicalkineticscatalysis", "Chmgeneral", "Dispersionchemistry", "Electrochemistry", "Inorganicchemistry", "Materialsengineering", "Molecularmodeling", "Nanotechnology", "Organicchemistry", "Polymersplastics", "Sustainableenergy", "Clinicallaboratoryscience", "Combustionpropulsion", "Aidshiv", "Communicablediseases", "Microbiology", "Virology", "Compositematerials", "Polymersplastics", "Corrosion", "Anesthesiology", "Criticalcare", "Crystallographystructuralchemistry", "Inorganicchemistry", "Biochemistry", "Biogeneral", "Cellbiology", "Developmentalbiologyembryology", "Molecularbiology", "Oncology", "Veterinarymedicine", "Chmgeneral", "Dispersionchemistry", "Materialsengineering", "Nanotechnology", "Polymersplastics", "Animalbehavior", "Biodiversityconservationbiology", "Biogeneral", "Birds", "Botany", "Ecology", "Environmentalsciences", "Evolutionarybiology", "Forestsforestry", "Insectsarthropods", "Marinesciencesfisheries", "Zoology", "Analyticalchemistry", "Chmgeneral", "Electrochemistry", "Materialsengineering", "Nanotechnology", "Sustainableenergy", "Biotechnology", "Environmentalgeologicalengineering", "Environmentalsciences", "Sustainabledevelopment", "Environmentallawpolicy", "Sustainabledevelopment", "Agronomycropscience", "Atmosphericsciences", "Biodiversityconservationbiology", "Biogeneral", "Biotechnology", "Ecology", "Environmentalgeologicalengineering", "Environmentalsciences", "Hydrology", "Marinesciencesfisheries", "Oceanography", "Soilsciences", "Sustainabledevelopment", "Animalbehavior", "Biodiversityconservationbiology", "Biogeneral", "Bioinformatics", "Birds", "Botany", "Ecology", "Evolutionarybiology", "Insectsarthropods", "Zoology", "Animalhusbandry", "Biotechnology", "Foodsciencetechnology", "Agronomycropscience", "Biodiversityconservationbiology", "Botany", "Ecology", "Forestsforestry", "Soilsciences", "Sustainabledevelopment", "Agronomycropscience", "Botany", "Geneticsgenomics", "Plantpathology", "Geochemistrymineralogy", "Geology", "Biogeneral", "Geochemistrymineralogy", "Geology", "Oceanography", "Paleontology", "Heartthoracicsurgery", "Atmosphericsciences", "Environmentalsciences", "Hydrology", "Soilsciences", "Chmgeneral", "Crystallographystructuralchemistry", "Inorganicchemistry", "Organicchemistry", "Animalbehavior", "Biodiversityconservationbiology", "Ecology", "Evolutionarybiology", "Insectsarthropods", "Pestcontrolpesticides", "Plantpathology", "Biodiversityconservationbiology", "Biogeneral", "Ecology", "Environmentalsciences", "Marinesciencesfisheries", "Oceanography", "Zoology", "Ceramicengineering", "Chmgeneral", "Dispersionchemistry", "Electrochemistry", "Materialsengineering", "Nanotechnology", "Polymersplastics", "Medicalinformatics", "Medicinalchemistry", "Organicchemistry", "Biogeneral", "Biotechnology", "Communicablediseases", "Microbiology", "Molecularbiology", "Virology", "Biochemistry", "Biogeneral", "Biophysics", "Cellbiology", "Developmentalbiologyembryology", "Microbiology", "Molecularbiology", "Oncology", "Proteomicspeptides", "Chmgeneral", "Molecularmodeling", "Nanotechnology", "Mycology", "Plantpathology", "Chmgeneral", "Dispersionchemistry", "Electrochemistry", "Materialsengineering", "Molecularmodeling", "Nanotechnology", "Polymersplastics", "Sustainableenergy", "Naturalmedicinesmedicinalplants", "Organicchemistry", "Nursing", "Nutritionscience", "Atmosphericsciences", "Biogeneral", "Environmentalsciences", "Geology", "Marinesciencesfisheries", "Oceanography", "Oilpetroleumnaturalgas", "Biochemistry", "Biogeneral", "Cellbiology", "Developmentalbiologyembryology", "Molecularbiology", "Oncology", "Chmgeneral", "Inorganicchemistry", "Medicinalchemistry", "Naturalmedicinesmedicinalplants", "Organicchemistry", "Audiologyspeechlanguagepathology", "Otolaryngology", "Geology", "Paleontology", "Agronomycropscience", "Insectsarthropods", "Pestcontrolpesticides", "Plantpathology", "Agronomycropscience", "Botany", "Geneticsgenomics", "Insectsarthropods", "Mycology", "Pestcontrolpesticides", "Plantpathology", "Chmgeneral", "Compositematerials", "Dispersionchemistry", "Materialsengineering", "Nanotechnology", "Polymersplastics", "Analyticalchemistry", "Biochemistry", "Bioinformatics", "Biophysics", "Molecularbiology", "Proteomicspeptides", "Psychology", "Addiction", "Aidshiv", "Publichealth", "Agronomycropscience", "Environmentalsciences", "Forestsforestry", "Hydrology", "Soilsciences", "Biodiversityconservationbiology", "Environmentalgeologicalengineering", "Environmentallawpolicy", "Environmentalsciences", "Forestsforestry", "Sustainabledevelopment", "Biotechnology", "Chmgeneral", "Electrochemistry", "Nanotechnology", "Sustainableenergy", "Toxicology", "Tropicalmedicineparasitology", "Animalhusbandry", "Developmentalbiologyembryology", "Veterinarymedicine", "Aidshiv", "Biogeneral", "Communicablediseases", "Microbiology", "Virology", "Woodsciencetechnology", "Animalbehavior", "Biodiversityconservationbiology", "Birds", "Ecology", "Evolutionarybiology", "Marinesciencesfisheries", "Zoology" ] 
},
"options": {
 "linkDistance":                50,
"charge":              -200,
"fontSize":                15,
"linkColour": "#666",
"nodeColour": "#3182bd",
"nodeClickColour": "#E34A33",
"textColour": "#3182bd",
"opacity":               0.6 
} 
},"evals": [  ] }</script><!--/html_preserve-->






