
ft_search("Ben G. Weinstein")

g<-ft_search("Ben G. Weinstein",from="crossref")
g$data$  
getj<-cr_journals(issn="2167-8359", query='ecology', works=TRUE, sort='score', order="desc")
getj$data$title
getj$data$author

ft_search("Ben G. Weinstein")
