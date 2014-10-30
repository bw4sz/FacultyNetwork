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


#Source abstract script
source("FacultySource.R")

# Get inputs, download abstracts, and create a corresponding wordcloud 

#Run test
abs<-getAbstracts(author="Catherine H. Graham", "Stony Brook",2010,2014,10)

#plot the abstracts, the 2nd and third argument are the color brewer ?brewer.pal, number of colors and palette
plotWC(abs,8,"Accent")


