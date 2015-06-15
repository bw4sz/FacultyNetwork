require(dplyr)
library(RSQLite)
#Create database
my_db<-src_sqlite(path = "C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3",create=T)

tocompare<-read.table("C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",row.names=NULL,header=T,sep=",",fill=T)
#copy_to(my_db, tocompare, "metadata", temporary = F)

#create index

dbGetQuery(my_db$con,"CREATE INDEX Journal ON journal_scopus(title)")
dbGetQuery(my_db$con,"CREATE INDEX Publication_index ON j_class(Publication)")

my_db %>% tbl("metadata") %>% head()
my_db %>% tbl("j_class") %>% head()

#test query
dbGetQuery(my_db$con,"SELECT * FROM journal_scopus
          JOIN j_class AS a ON title = Publication
           WHERE a.Discipline = 'chm'")

#The first table is going to be Journal and DOI
my_db %>% tbl("metadata") %>% collect() %>% distinct(DOI) %>% select(DOI,Journal) %>% copy_to(dest=my_db,name="JA",indexes=list("DOI"),temporary=F)
  
#The second table is DOI Order and Author Citation Year
my_db %>% tbl("metadata") %>% collect() %>% select(DOI,Order,Author,Citations,Year) %>% copy_to(dest=my_db,name="Meta",indexes=list("DOI"),temporary=F)

#The third table is Author Affiliation
my_db %>% tbl("metadata") %>% collect() %>% distinct(Author) %>% select(Author,Affiliation) %>% copy_to(dest=my_db,name="Aff",indexes=list("Author"),temporary=F)

