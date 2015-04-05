#Source functions

##Make Query
scquery<-function(inquery){
  
  #Basic Query Search
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
  str<-"https://api.elsevier.com/content/search/scopus?&httpAccept=application/xml&count=20&view=complete"
  
  #fields
  f<-"field=affiliation,prism:publicationName,dc:title,dc:creator,citedby-count,prism:coverDate,author")
  
  toget<-paste(str,queryF,sep="&")
  
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
#Parse
  
  xml <- xmlInternalTreeParse(response)
  xmltop = xmlRoot(xml) #gives content of root
  class(xmltop)
  xmlName(xmltop) #give name of node
  xmlSize(xmltop) #how many children in node
  xmlName(xmltop[[1]]) #name of root's children
  
  #set namespaces
  #define name spaces
  nsDefs<-xmlNamespaceDefinitions(xmltop)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  
  names(ns)[1] <- "xmlns"
  
  #Format Results
  
  ##Get journal
  
  #get
  journal<-xpathSApply(xmltop,"//prism:publicationName",xmlValue,namespaces=ns)
  
  ##Get first author
  
  #List with a position for each article
  
  authors<-xpathSApply(xmltop,"//dc:creator",xmlValue,namespaces=ns)
  
  ##All authors
  The requestor is not authorized to access this resource yet?
  
  #how many articles are there?
  lresponse<-length(getNodeSet(xmltop,"//xmlns:entry",namespaces=ns,xmlValue))
  
  #loop through each article and get author list
  allauthors<-list()
  for (x in 1:lresponse){
    #make an xpath statement
    xpath<-paste("//xmlns:entry[",x,"]//xmlns:author/xmlns:authname",sep="")
    allauthors[[x]]<-as.list(xpathSApply(xmltop,xpath,xmlValue,namespaces=ns))
  }
  
  
  ##Affiliation
  
  #first author
  aff<-xpathSApply(xmltop,"//xmlns:entry//xmlns:affilname[1]",xmlValue,namespaces=ns)
  
  #All affiliations
  allaff<-list()
  for (x in 1:lresponse){
    #make an xpath statement
    xpath<-paste("//xmlns:entry[",x,"]//xmlns:affiliation/xmlns:affilname",sep="")
    allaff[[x]]<-as.list(xpathSApply(xmltop,xpath,xmlValue,namespaces=ns))
  }
  
  #fill any null positions with NA
  allaff[sapply(allaff,length)==0][[1]]<-NA
  
  ##Citation Count
  citation<-xpathSApply(xmltop,"//xmlns:entry//xmlns:citedby-count",xmlValue,namespaces=ns)
  
  ##Year
  Year<-years(xpathSApply(xmltop,"//xmlns:entry//prism:coverDate",xmlValue,namespaces=ns))
  
  #Bind article level statistics
  artdf<-data.frame(First_Author=authors,Journal=journal,Citations=citation,Year=Year)
  
  #melt and combine
  allauthors<-melt(allauthors)
  colnames(allauthors)<-c("Author","Order","Article")
  allaff<-melt(allaff)
  colnames(allaff)<-c("Affiliation","Order","Article")
  authdf<-merge(allauthors,allaff)
}
