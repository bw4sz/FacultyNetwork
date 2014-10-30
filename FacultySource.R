### Source Functions for getting journal titles 
#source functions for getting publication history.
getAbstracts <- function(author,university, dFrom, dTill, nRecs)
{
  #For more details about Pubmed queries see: http://www.ncbi.nlm.nih.gov/books/NBK25500/
  
  #Text search - basic URL
  eSearch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="
  #Data record download - basic URL
  eDDownload <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id="
  
  #In case of multiple words (e.g., first and the last name), add "+" sign in between them 
  aL <- str_replace_all(author, " ", "+")
  #Add the search keyword - author
  aQ <- paste(aL, "[author]", sep = "")
  
  #add institution?
  
  if(exists("university")){
    aU <- str_replace_all(university, " ", "+")
    #Add the search keyword - affiliation
    aUU <- paste(aL, "[ad]", sep = "")
  }
  
  #Format the publication date and add the search keyword - pdat
  #If only one year is provided, use that year, otherwise use year_1:year_2
  dQ <- ""
  
  if ((str_length(dFrom) > 0) & (str_length(dTill) > 0))
  {
    d1 <- paste(dFrom, dTill, sep = ":")
    dQ <- paste(d1, "[pdat]", sep = "")
  }
  
  if ((str_length(dFrom) > 0) & (str_length(dTill) == 0))
    dQ <- paste(dFrom, "[pdat]", sep = "")
  
  if ((str_length(dTill) > 0) & (str_length(dFrom) == 0))
    dQ <- paste(dTill, "[pdat]", sep = "")
  
  #Add two seqrch queries together
  hlpQ1 <- aQ  
  
  if (str_length(dQ) > 0)    
    hlpQ1 <- paste(aQ, dQ, sep = "+")
  
  #Add the max number of retrieved articles at the end of the query
  rmQ <- paste("&retmax=", nRecs, sep="")
  hlpQ2 <- paste(hlpQ1, rmQ, sep="")
  
  if(exists("university")){
    hlpQ3 <- paste(hlpQ2, aUU, sep = "+")
  }
  
  #Finalize the query and serch Pubmed
  searchUrl <- paste(eSearch, hlpQ2, sep = "" )
  #Wait - to ensure that all requests will be processed
  Sys.sleep(3)    
  hlpURL <- getURL(searchUrl)
  #The result is in form of XML document - you can paste the searchUrl in the browser to see/download it
  doc <- xmlTreeParse(hlpURL, asText = TRUE)     
  IdlistHlp = xmlValue(doc[["doc"]][["eSearchResult"]][["IdList"]])
  
  #I am sure there is more elegant way (i.e., a function) to proccess this, but I was lazy to search for it
  if (length(IdlistHlp) > 0)
  {
    Idlist <- c()
    
    #Each ID is 8 digits long
    for(k in 1:(str_length(IdlistHlp)/8))
      Idlist <- c(Idlist, str_sub(IdlistHlp, start = 8*(k-1) + 1, end = k*8))
    
    #Once we retrieved articles' IDs for the author/dates, we can process them and get abstracts             
    Sys.sleep(2)
    hlp1 <- paste(eDDownload, paste(Idlist, collapse = ",", sep = ""), sep = "")
    hlp2 <- paste(hlp1, "&rettype=abstract", sep = "")
    testDoc <- xmlTreeParse(hlp2, useInternalNodes = TRUE)
    topFetch <-xmlRoot(testDoc)
    abst <- xpathSApply(topFetch, "//Abstract", xmlValue)
  }
  
  #In case that nothing was found
  if (length(IdlistHlp) == 0)
    abst = c("Zero", "Articles", "Found")
  
  abst
}

plotWC <- function(abstracts, nc, cs)
{
  #Once we have abstracts, we can create a document corpus
  abstTxt <- Corpus(VectorSource(abstracts))
  
  text2.corpus = tm_map(abstTxt, removePunctuation)
  text2.corpus = tm_map(text2.corpus, tolower)
  myCorpus = tm_map(text2.corpus, removeNumbers)
  text2.corpus = tm_map(text2.corpus, removeWords, stopwords("english"))
  
  #Transform it into a matrix and sort based on the total word occurence
  tdm <- TermDocumentMatrix(text2.corpus, control = list(minWordLength = 3))
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  #Select the color scheme
  pal2 <- brewer.pal(nc, cs)
  
  #And plot the cloud
  wordcloud(d$word,d$freq, scale=c(8,.2), min.freq = 5, max.words=50, random.order = FALSE, rot.per=.15, color = pal2, vfont=c("sans serif","plain"))
}

##Get journals
### Source Functions for getting journal titles 
#source functions for getting publication history.

getJournals <- function(university, dFrom, dTill,nRecs=5)
{
  #For more details about Pubmed queries see: http://www.ncbi.nlm.nih.gov/books/NBK25500/
  
  #Text search - basic URL
  eSearch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="
  #Data record download - basic URL
  eDDownload <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id="
  
  #Format String
    aU <- str_replace_all(university, " ", "+")
    #Add the search keyword - affiliation
    aUU <- paste(aU, "[ad]", sep = "")

  
  #Format the publication date and add the search keyword - pdat
  #If only one year is provided, use that year, otherwise use year_1:year_2
  dQ <- ""
  
  if ((str_length(dFrom) > 0) & (str_length(dTill) > 0))
  {
    d1 <- paste(dFrom, dTill, sep = ":")
    dQ <- paste(d1, "[pdat]", sep = "")
  }
  
  if ((str_length(dFrom) > 0) & (str_length(dTill) == 0))
    dQ <- paste(dFrom, "[pdat]", sep = "")
  
  if ((str_length(dTill) > 0) & (str_length(dFrom) == 0))
    dQ <- paste(dTill, "[pdat]", sep = "")
  
  #Add two seqrch queries together
  hlpQ1 <- aUU  
  
  if (str_length(dQ) > 0)    
    hlpQ1 <- paste(aUU, dQ, sep = "+")
  
  rmQ <- paste("&retmax=", nRecs, sep="")
  hlpQ2 <- paste(hlpQ1, rmQ, sep="")
  
  #Finalize the query and serch Pubmed
  searchUrl <- paste(eSearch, hlpQ2, sep = "" )
 
  hlpURL <- getURL(searchUrl)
  #The result is in form of XML document - you can paste the searchUrl in the browser to see/download it
  doc <- xmlTreeParse(hlpURL, asText = TRUE)     
  IdlistHlp = xmlValue(doc[["doc"]][["eSearchResult"]][["IdList"]])
  
  #I am sure there is more elegant way (i.e., a function) to proccess this, but I was lazy to search for it
  if (length(IdlistHlp) > 0)
  {
    Idlist <- c()
    
    #create a frame to spit out results
    toreturn<-list()
    
    #Each ID is 8 digits long
    for(k in 1:(str_length(IdlistHlp)/8))
      Idlist <- c(Idlist, str_sub(IdlistHlp, start = 8*(k-1) + 1, end = k*8))
    
    #Once we retrieved articles' IDs for the author/dates, we can process them and get abstracts             
    hlp1 <- paste(eDDownload, paste(Idlist, collapse = ",", sep = ""), sep = "")
    
    hlp2 <- paste(hlp1, "&rettype=abstract", sep = "")
    testDoc <- xmlTreeParse(hlp2, useInternalNodes = TRUE)
    
    r <-xmlRoot(testDoc)
    
    #get the right node
    xmlChildren(r[[2]][[1]]), xpathApply,"//Author")
    
    aut_nameL <- sapply(xmlChildren(r[[2]][[1]])$Article,xmlValue)
    
xmlChildren(r[[2]][[1]]), xpathApply,"//Author")

  sapply(r[[1]][[1]][[3]][["AuthorList"]],
       
    #get author last names
    aut_nameL <- xpathApply(topFetch, "//Author/LastName",xmlValue)
    
    #get author first names
    aut_nameF <- xpathSApply(topFetch, "//Author/ForeName",xmlValue)
    
    #get affiliations
    aut_aff <- xpathSApply(topFetch, "//Author/Affiliation",xmlValue)
    
    #get journal
    jour_name <- xpathSApply(topFetch, "//Article/Journal/Title",xmlValue)
    
    #data.frame of results
    res<-data.frame(FirstName=aut_nameF,LastName=aut_nameL,University=aut_aff,jour_name)
    
    #find records that match our input university
    aut<-res[str_detect(res$University,university),]
    
    #bind to list
    toreturn[[k]]<-aut
      }
  
  #In case that nothing was found
  if (length(IdlistHlp) == 0)
    abst = c("Zero", "Articles", "Found")
  
  return(toreturn)
}