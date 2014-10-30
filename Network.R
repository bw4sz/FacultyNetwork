#Network


source("FacultySource.R")

#Script to take in vector of names to create word clouds and network of abstract word interactions
#ie to show how the department is connected

#load libraries

library(XML)
library(stringr)
library(RCurl)
library(wordcloud)
library(tm)
require(reshape)
require(sna)
require(bipartite)
require(dplyr)
require(stringr)


URLS<-c(
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_agronomycropscience",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_animalbehavior",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_animalhusbandry",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_atmosphericsciences",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_biochemistry",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_biodiversityconservationbiology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_bioinformatics",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_biophysics",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_biotechnology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_birds",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_botany",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_cellbiology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_developmentalbiologyembryology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_ecology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_environmentalgeologicalengineering",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_environmentalsciences",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_evolutionarybiology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_foodsciencetechnology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_forestsforestry",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_geochemistrymineralogy",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_geology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_hydrology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_insectsarthropods",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_biogeneral",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_marinesciencesfisheries",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_microbiology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_molecularbiology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_mycology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_oceanography",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_paleontology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_pestcontrolpesticides",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_plantpathology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_proteomicspeptides",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_soilsciences",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_sustainabledevelopment",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_sustainableenergy",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_virology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_woodsciencetechnology",
  "http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_zoology")


.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#Get journal classification
out<-list()

for (x in 1:length(URLS)){
  nam<-str_match(URLS[x],"bio_(\\w+)")[2]
  xtab= readHTMLTable(URLS[x], header=T, which=1,stringsAsFactors=F)[,-1]
  out[[x]]<-data.frame(Class=.simpleCap(nam),xtab)
}

j_class<-rbind_all(out)

names(me)<-profs
mem<-melt(me,id.var=c("word","freq"))

#what are the strongest interacting words
word_matrix<-acast(mem,L1~word,value.var="freq",fill=0)

#as distance matrix
dist_matrix<-dist(word_matrix)

#To do only keep shared words?

g<-graph.adjacency(as.matrix(dist_matrix),diag=FALSE,mode="lower",weighted=TRUE)

#names of the vertices you just imported:
V(g)$name
E(g)$size

plot.igraph(g,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(g)$weight/50) 

#color by weight
cols<-gray(E(g)$weight/max(E(g)$weight))

plot.igraph(g,layout=layout.fruchterman.reingold, edge.color=cols,edge.width=E(g)$weight/50) 

#that was grey, try color

colramp<-colorRampPalette(c("blue","red"))(length(E(g)$weight))

#original order
orig<-E(g)$weight/max(E(g)$weight)

orig.order<-data.frame(orig,1:length(orig))

weight.order<-orig.order[order(E(g)$weight/max(E(g)$weight)),]

#merge with col
colramp.w<-data.frame(weight.order,colramp)

#get original order
colsRB<-colramp.w[order(colramp.w$X1.length.orig.),]

plot.igraph(g,layout=layout.fruchterman.reingold, edge.color=as.character(colsRB$colramp),edge.width=(E(g)$weight/100)) 

mem$word<-as.character(mem$word)


#make another prettier graph

## Test
plot(g,edge.arrow.size=E(g)$size/10)

mem[order(mem$freq,decreasing=TRUE),][1:20,]


#Step two make a network of the participants

profs<-c("Heather J. Lynch","Catherine H. Graham","Lev Ginsburg","H. Resit Akcakaya","Diana Padilla","John R. True","Walt Eanes","Mike Bell","Jeff S. Levinton","Brenna Henn", "Liliana M. Davalos","Joshua S. Rest","Jessica Gurevitch","Stephen B. Baines")
abs_all<-lapply(profs,function(x){
  abs<-getAbstracts(x,"Stony Brook",2000,2014,20)  
}
)

#plot all as one word cloud
plotWC(abs_all,8,"Accent")
