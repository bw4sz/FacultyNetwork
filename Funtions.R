#Source functions

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
  f<-"field=affiliation,prism:publicationName,dc:title,dc:creator,citedby-count,prism:coverDate,author,dc:identifier"
  
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
  
  ##Affiliation

  #All affiliations
  allaff<-list()
  for (x in 1:lresponse){
    #make an xpath statement
    xpath<-paste("//xmlns:entry[",x,"]//xmlns:affiliation/xmlns:affilname",sep="")
    allaff[[x]]<-as.list(xpathSApply(xmltop,xpath,xmlValue,namespaces=ns))
  }
  
  #fill any null positions with NA
  allaff[sapply(allaff,length)==0]<-"Unknown"
  
  #Name by DOI
  names(allaff)<-xpathSApply(xmltop,"//xmlns:entry//dc:identifier",xmlValue,namespaces=ns)
  
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
  allaff<-melt(allaff)
  
  #if the journal doesn't provide affiliation, there will be no order
  if(ncol(allaff)==2){
    allaff$Order<-NA
    allaff<-allaff[,c(1,3,2)]
  }
  colnames(allaff)<-c("Affiliation","Order","DOI")
  
  #merge
  authdf<-merge(allauthors,allaff,by=c("Order","DOI"))
  
  #Match journal to classification
  #legacy name change
  artmatch<-artdf
  if(nrow(artmatch)==0){ artmatch<-artdf[paste("The",artdf$Journal) %in% j_class$Publication,] }

  #merge into final table
  dat<-droplevels(merge(authdf,artmatch))
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
        #build in break for now
      }
      
      #remove blank
      yeardat<-yeardat[!sapply(yeardat,length)==0]
      
      #remove if only one hit
      yeardat<-yeardat[!lapply(yeardat,currentCount)==1]
    
      #bind the yeardat
      out[[x]]<-rbind_all(lapply(yeardat,sc_parse))
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
