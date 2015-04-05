# Faculty Niche
Ben Weinstein  
Saturday, November 01, 2014  

#Approach

  * Query Articles
    * Format Data
  * Define Authors by ID
  * Characterize journals
  * Define Author Niche
  * Calculate Niche Overlap
    * Within institution
    * For a given article
  * Question 1: Model Fitting: Are articles by specialist or generalist more cited?
  * Question 2: Level of academic specialization within departments
  
#Hypotheses

  * Articles by highly similiar authors with high niche overlap will be higher cited due intense domain-specific knowledge
  * Articles by authors with low niche overlap will be higher cited due to novel insights
  
  
#Basic Query Search


```r
library(XML)
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(reshape2)
library(httr)
library(stringr)
library(chron)
```

##Test query

Here is the base url; http://api.elsevier.com/content/search/scopus
*query=the main query with descriptors.

The descriptors are ennumerated here:
http://api.elsevier.com/content/search/fields/scopus

Get all articles by author Ben Weinstein

The boolean escapes are + and parenthesis need to be %28 and %29

```r
#format string
query<-"query=affil(Stony+Brook)+AND+SUBJAREA(AGRI)+OR+SUBJAREA(ENVI)"

#url encoding
#reform query to html encoded
queryF<-gsub(x=query,"\\(","%28")
queryF<-gsub(x=queryF,"\\)","%29")
queryF<-gsub(x=queryF,"\\+","%20")

queryF
```

```
## [1] "query=affil%28Stony%20Brook%29%20AND%20SUBJAREA%28AGRI%29%20OR%20SUBJAREA%28ENVI%29"
```

###Query Parameters
*httpAccept=application/xml returns an xml result
Add my api key

Institution token - cannot be viewed in browser, save in file outside of git.

```r
inst.token<-readLines("C:/Users/Ben/Dropbox/FacultyNetwork/InstToken.txt")
apiKey<-readLines("C:/Users/Ben/Dropbox/FacultyNetwork/apikey.txt")
```



```r
#format string
str<-"https://api.elsevier.com/content/search/scopus?&httpAccept=application/xml&count=20&view=complete"

#fields
f<-"field=affiliation,prism:publicationName,dc:title,dc:creator,citedby-count,prism:coverDate,author"
```

##Build queries

  * Arguments
  * Desired fields
  * Query terms
  
It's easiest to bind these together seperately, to create one long call

We need to know author, affiliation, publication name, and citation count


```r
toget<-paste(str,queryF,sep="&")

#add in api and institutional key
toget<-paste(toget,"&apiKey=",apiKey,sep="")
toget<-paste(toget,"&insttoken=",inst.token,sep="")

#add fields
toget<-paste(toget,f,sep="&")
```

Request query

```r
#call
response <- GET(toget)
```

Parse


```r
xml <- xmlInternalTreeParse(response)
xmltop = xmlRoot(xml) #gives content of root
class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
```

```
## [1] "XMLInternalElementNode" "XMLInternalNode"       
## [3] "XMLAbstractNode"
```

```r
xmlName(xmltop) #give name of node
```

```
## [1] "search-results"
```

```r
xmlSize(xmltop) #how many children in node
```

```
## [1] 28
```

```r
xmlName(xmltop[[1]]) #name of root's children
```

```
## [1] "totalResults"
```

```r
#set namespaces
#define name spaces
nsDefs<-xmlNamespaceDefinitions(xmltop)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

names(ns)[1] <- "xmlns"
```

#Format Results

##Get journal


```r
#get
journal<-xpathSApply(xmltop,"//prism:publicationName",xmlValue,namespaces=ns)
```

##Get first author

List with a position for each article


```r
authors<-xpathSApply(xmltop,"//dc:creator",xmlValue,namespaces=ns)
```

##All authors
The requestor is not authorized to access this resource yet?

```r
#how many articles are there?
lresponse<-length(getNodeSet(xmltop,"//xmlns:entry",namespaces=ns,xmlValue))

#loop through each article and get author list
allauthors<-list()
for (x in 1:lresponse){
  #make an xpath statement
  xpath<-paste("//xmlns:entry[",x,"]//xmlns:author/xmlns:authname",sep="")
  allauthors[[x]]<-as.list(xpathSApply(xmltop,xpath,xmlValue,namespaces=ns))
}
```

##Affiliation


```r
#first author
aff<-xpathSApply(xmltop,"//xmlns:entry//xmlns:affilname[1]",xmlValue,namespaces=ns)
```


```r
#All affiliations
allaff<-list()
for (x in 1:lresponse){
  #make an xpath statement
  xpath<-paste("//xmlns:entry[",x,"]//xmlns:affiliation/xmlns:affilname",sep="")
  allaff[[x]]<-as.list(xpathSApply(xmltop,xpath,xmlValue,namespaces=ns))
}

#fill any null positions with NA
allaff[sapply(allaff,length)==0][[1]]<-NA
```

##Citation Count


```r
citation<-xpathSApply(xmltop,"//xmlns:entry//xmlns:citedby-count",xmlValue,namespaces=ns)
```

##Year


```r
Year<-years(xpathSApply(xmltop,"//xmlns:entry//prism:coverDate",xmlValue,namespaces=ns))
```

Bind article level statistics

```r
artdf<-data.frame(First_Author=authors,Journal=journal,Citations=citation,Year=Year)
```

Bind author level statistics

```r
#melt and combine
allauthors<-melt(allauthors)
colnames(allauthors)<-c("Author","Order","Article")
allaff<-melt(allaff)
colnames(allaff)<-c("Affiliation","Order","Article")
authdf<-merge(allauthors,allaff)
```

#Storing Results
  * Create a database to update results
  This only needs to be done once.

```r
#Create a database (once)
#my_db <- src_sqlite("Scopus", create = T)
copy_to(my_db, artdf, temporary = FALSE,name="Articles")
copy_to(my_db, authdf, temporary = FALSE,name="Authors")
```

##Update Results
