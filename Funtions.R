#Source functions
library(proto)

##Make Query
scquery<-function(inquery,year){
  
  #format string
  query<-paste("query=",inquery,sep="")
  
  #url encoding
  #reform query to html encoded
  queryF<-gsub(x=query,"\\(","%28")
  queryF<-gsub(x=queryF,"\\)","%29")
  queryF<-gsub(x=queryF,"\\+","%20")
  
  ###Query Parameters  
  #Institution token - cannot be viewed in browser, save in file outside of git.
  inst.token<-readLines("C:/Users/Ben/Dropbox/FacultyNetwork/InstToken.txt")
  apiKey<-readLines("C:/Users/Ben/Dropbox/FacultyNetwork/apikey.txt")  
  
  #format string
  str<-"https://api.elsevier.com/content/search/scopus?&httpAccept=application/xml&view=complete&count=100"
  
  #fields
  f<-"field=prism:publicationName,dc:title,dc:creator,citedby-count,prism:coverDate,author,dc:identifier"
  
  #bind
  toget<-paste(str,queryF,sep="&")
  
  #bind to the year
  toget<-paste(toget,year,sep="&date=")
  
  #add in api and institutional key
  toget<-paste(toget,"&apiKey=",apiKey,sep="")
  toget<-paste(toget,"&insttoken=",inst.token,sep="")
  
  #add fields
  toget<-paste(toget,f,sep="&")
  
  #Request query
  #call
  response <- GET(toget)
  return(response)
}

##

sc_parse<-function(response){
  
  #Does the response have data?
  if(!response$status_code==200){return("Invalid Query")}
  
  xml <- xmlInternalTreeParse(response)
  xmltop<-xmlRoot(xml)
  
  #define name spaces
  nsDefs<-xmlNamespaceDefinitions(xmltop)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  names(ns)[1] <- "xmlns"
  
  #set namespaces
  e<-xpathSApply(xmltop,"//xmlns:entry/xmlns:error",xmlValue,namespaces=ns)
  if(length(e)==1){return(NA)}
  
  #Format Results
  
  ##Get journal
  journal<-xpathSApply(xmltop,"//prism:publicationName",xmlValue,namespaces=ns)
  
  ##All authors  
  #how many articles are there?
  lresponse<-length(getNodeSet(xmltop,"//xmlns:entry",namespaces=ns,xmlValue))
  
  #loop through each article and get author list
  allauthors<-list()
  for (x in 1:lresponse){
    #make an xpath statement
    xpath<-paste("//xmlns:entry[",x,"]//xmlns:author/xmlns:authid",sep="")
    allauthors[[x]]<-as.list(xpathSApply(xmltop,xpath,xmlValue,namespaces=ns))
  }

  names(allauthors)<-xpathSApply(xmltop,"//xmlns:entry//dc:identifier",xmlValue,namespaces=ns)
  
  #if missing
  allauthors[lapply(allauthors,length)==0]<-"Unknown"
  
  ##Citation Count
  citation<-as.numeric(xpathSApply(xmltop,"//xmlns:entry//xmlns:citedby-count",xmlValue,namespaces=ns))
  
  ##Year
  Year<-years(xpathSApply(xmltop,"//xmlns:entry//prism:coverDate",xmlValue,namespaces=ns))
  
  #Indentifier
  DOI<-xpathSApply(xmltop,"//xmlns:entry//dc:identifier",xmlValue,namespaces=ns)
  
  #Bind article level statistics
  artdf<-data.frame(Journal=journal,Citations=citation,Year=Year,DOI=DOI)
  
  #melt and combine
  allauthors<-melt(allauthors)
  colnames(allauthors)<-c("Author","Order","DOI")
    
  #Match journal to classification
  #legacy name change
  artmatch<-artdf
  if(nrow(artmatch)==0){ artmatch<-artdf[paste("The",artdf$Journal) %in% j_class$Publication,] }

  #merge into final table
  dat<-droplevels(merge(allauthors,artmatch))
  return(dat)
}


#How many results from a query, used to loop until complete
getCount<-function(response){  
  xml <- xmlInternalTreeParse(response)
  xmltop<-xmlRoot(xml)
  
  #define name spaces
  nsDefs<-xmlNamespaceDefinitions(xmltop)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  names(ns)[1] <- "xmlns"
  
  #Get total results
  tresults<-as.numeric(xpathSApply(xmltop,"//opensearch:totalResults",xmlValue,namespaces=ns))
  return(tresults)
}

currentCount<-function(response){
  
  xml <- xmlInternalTreeParse(response)
  xmltop<-xmlRoot(xml)
  
  #define name spaces
  nsDefs<-xmlNamespaceDefinitions(xmltop)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  names(ns)[1] <- "xmlns"
  
  #Get current results total
  tresults<-length(xpathSApply(xmltop,"//xmlns:entry",xmlValue,namespaces=ns))
  return(tresults)
}

#run for each year
allyears<-function(query,yearrange){
  out<-list()
  for(y in 1:length(yearrange)){
    
    yeardat<-list()
      
      #Iterators
      tcount<-0
      r<-1
  
      #Get initial query
      response<-scquery(inquery=query,yearrange[y])
      
      if(!response$status_code==200){next}
    
      #How many results are there?
      tresults<-getCount(response)
      
      #How many results did we get?
      tcount<-tcount+currentCount(response)
      yeardat[[r]]<-response
      
      #break function if no response
      if(tresults==0){
        yeardat[[r]]<-NA
        next
      }
    
      #Iterate until we get all results
      while(!tcount==tresults){
        r=r+1
        newrequest<-paste(response[[1]],"&start=",tcount,sep="")
        newresponse<-GET(newrequest)
        if(!newresponse$status_code==200){break}
        yeardat[[r]]<-newresponse
        tcount<-tcount+currentCount(newresponse)
      }
      
      #remove blank
      yeardat<-yeardat[!sapply(yeardat,length)==0]
      
      #remove if only one hit
      yeardat<-yeardat[!lapply(yeardat,currentCount)==1]
    
      #bind the yeardat
      out[[y]]<-rbind_all(lapply(yeardat,sc_parse))
      print(paste(str_extract(query,"\\(.*?\\)"),yearrange[y]))
  }
  
  dat<-rbind_all(out[!sapply(out,length)==1])
  return(dat)
}

#helper function for standardizing caps
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

getSourceID<-function(inquery){
  
  #
  query<-paste("title=",inquery,sep="")
  
  #url encoding
  #reform query to html encoded
  queryF<-gsub(x=query,"\\(","%28")
  queryF<-gsub(x=queryF,"\\)","%29")
  queryF<-gsub(x=queryF,"\\+","%20")
  
  ###Query Parameters  
  #Institution token - cannot be viewed in browser, save in file outside of git.
  inst.token<-readLines("C:/Users/Ben/Dropbox/FacultyNetwork/InstToken.txt")
  apiKey<-readLines("C:/Users/Ben/Dropbox/FacultyNetwork/apikey.txt")  
  
  #format string
  toget<-"https://api.elsevier.com/content/serial/title?&httpAccept=application/xml&count=100"
    
  #bind
  toget<-paste(toget,queryF,sep="&")
  
  #add in api and institutional key
  toget<-paste(toget,"&apiKey=",apiKey,sep="")
  toget<-paste(toget,"&insttoken=",inst.token,sep="")
  
  #Request query
  #call
  response <- GET(toget)
  
}

parseSource<-function(response,inquery){
 
  if(!response$status_code==200){return(NA)}
  
  xml <- xmlInternalTreeParse(response)
  xmltop<-xmlRoot(xml)
  
  #define name spaces
  nsDefs<-xmlNamespaceDefinitions(xmltop)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  
  #Get current results total
  title<-xpathSApply(xmltop,"//dc:title",xmlValue,namespaces=ns)
  ID<-xpathSApply(xmltop,"//source-id",xmlValue,namespaces=ns)
  
  if(length(title)==0){return(NA)}
  
  r<-data.frame(title=title,ID=ID,query=inquery)
  
  #just get the one that matches
  r<-r[toupper(r$title) %in% toupper(gsub(inquery,pattern="\\+",replacement=" ")),]
  return(r)
}


##Horizontal ggplot

geom_bar_horz <- function (mapping = NULL, data = NULL, stat = "bin", position = "stack", ...) {
  GeomBar_horz$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomBar_horz <- proto(ggplot2:::Geom, {
  objname <- "bar_horz"
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionStack
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, weight = 1, alpha = NA)
  
  required_aes <- c("y")
  
  reparameterise <- function(., df, params) {
    df$width <- df$width %||%
      params$width %||% (resolution(df$x, FALSE) * 0.9)
    OUT <- transform(df,
                     xmin = pmin(x, 0), xmax = pmax(x, 0),
                     ymin = y - .45, ymax = y + .45, width = NULL
    )
    return(OUT)
  }
  
  draw_groups <- function(., data, scales, coordinates, ...) {
    GeomRect$draw_groups(data, scales, coordinates, ...)
  }
  guide_geom <- function(.) "polygon"
})

##Calculate network stats from df

calcN<-function(x){
  x<-droplevels(x)
  siteXspp<-as.data.frame.array(table(x$Author,x$Class))

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
  
  between_class<-betweenness(g.copy)
  degree_class<-degree(g.copy)
  closeness_class<-closeness(g.copy)
  eigenV<-evcent(g.copy)$vector
  vdat<-data.frame(Class=names(between_class),Between=between_class,Degree=degree_class,Closeness=closeness_class,Eigen=eigenV)
 return(vdat) 
}
CalcG<-function(x){
x<-droplevels(x)
siteXspp<-as.data.frame.array(table(x$Author,x$Class))

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

#connectance
gdensity<-graph.density(g.copy)

}


CalcDD<-function(x){
  x<-droplevels(x)
  siteXspp<-as.data.frame.array(table(x$Author,x$Class))
  
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
  dd<-degree.distribution(g.copy)
  return(as.matrix(dd))
  }
  
shead<-function(tab){dbGetQuery(d$con,paste("SELECT * FROM",tab,"limit 10"))}


queryscopus<-function(runs,size=20){

  #create a data holder
  
  cl<-makeCluster(size,"SOCK")
  registerDoSNOW(cl)
  
  #write a test query that you know works to ensure you have space
  tq<-scquery("AUK","2014")
  
  #set placement of journal
  jp<-read.table("Data/JournalSection.txt")$x
  print(jp)
  
  #update new
  #how many journals to run?
  #if last run ended in exceed query, run 0!
  jp<-(max(jp)+1):(max(jp)+runs)
  
  if (tq$status_code==200){
  
  dat<-foreach(x=jp,.errorhandling = "pass",.packages=c("httr","XML","proto","reshape2","plyr","dplyr","chron","stringr"),.export="journaldf") %dopar% {
    #get functions
    source("Funtions.R")
    
    #get articles from a journal and parse it
    q<-paste("source-id(",journaldf$ID[x],")",sep="")
    
    #call query
    responses<-allyears(query=q,yearrange=1995:2014)
    
    #parse result
    return(responses)
  }
  
  stopCluster(cl)
  
  #if we ran out of calls, figure out where, using a test query
  tq<-scquery("AUK","2014")
  
  if(!tq$status_code==200){
    dat<-dat[sapply(dat,function(x){
      max(as.numeric(as.character(x$Year)))
    })==2014]
  }
  
  #bind journals, remove no matched
  #correct runs have length of 6
  df<-rbind_all(dat[lapply(dat,length)==6])
  
  #Standardize capitalization
  df$Journal<-sapply(df$Journal,.simpleCap)
  
  #turn unknowns to NA, it was just a place holder
  df[df$Author %in% "Unknown","Author"]<-NA
  
  #write.table(df,"C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",append=T,col.names=F,row.names=F,sep=",")
  df<-df[!is.na(df$DOI),]
  
  print(dim(df))
  
  #write journal and DOI table to the JA table
  towrite<-df %>%  distinct(DOI) %>% select(DOI,Journal)
  db_insert_into(con=d$con,table="JA",values=as.data.frame(towrite))
  
  towrite<-df %>%  distinct(DOI) %>% select(DOI,Author,Order,Citations,Year)
  db_insert_into(con=d$con,table="Meta",values=as.data.frame(towrite))
  
  write.table(jp,"Data/JournalSection.txt")
  
  #manually remove objects to be sure
  rm(df,towrite,dat)
  gc()
  
  return(TRUE)} else {return(FALSE)}

}
