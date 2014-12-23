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
