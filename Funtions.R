#Source functions

##Make Query
scquery<-function(inquery,year){
  
  #format string
  query<-paste("query=SUBJAREA(AGRI)+OR+SUBJAREA(ENVI)",inquery,sep="+AND+")
  
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
    xpath<-paste("//xmlns:entry[",x,"]//xmlns:author/xmlns:authname",sep="")
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
  #article match to classifier
  artmatch<-artdf[artdf$Journal %in% j_class$Publication,]
  if(nrow(artmatch)==0){ artmatch<-artdf[paste("The",artdf$Journal) %in% j_class$Publication,] }

  #merge into final table
  dat<-droplevels(merge(authdf,artmatch))
  return(dat)
}

#run for each year
allyears<-function(query,yearrange){
  out<-list()
  for(x in 1:length(yearrange)){
    out[[x]]<-scquery(query,yearrange[x])
  }
  return(out)
}