require(dplyr)
library(RSQLite)
library(stringr)
#Create database
my_db<-src_sqlite(path = "C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3",create=T)

tocompare<-read.table("C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",row.names=NULL,
                      header=T,sep=",",fill=T)

copy_to(my_db, tocompare, "metadata", temporary = T)

#The first table is going to be Journal and DOI
my_db %>% tbl("metadata") %>% collect() %>% distinct(DOI) %>% select(DOI,Journal) %>% copy_to(dest=my_db,name="JA",indexes=list("Journal"),temporary=F)
  
#The second table is DOI Order and Author Citation Year
my_db %>% tbl("metadata") %>% collect() %>% select(DOI,Order,Author,Citations,Year) %>% copy_to(dest=my_db,name="Meta",indexes=list("DOI"),temporary=F)

db_drop_table(my_db$con,"metadata")
db_drop_table(my_db$con,"aff")

#drop any journal that mentions biological chemistry

dbGetQuery(d$con, "DELETE FROM JA WHERE Journal LIKE '%Biological Chemistry%'")
dbGetQuery(d$con, "DELETE FROM Meta WHERE Year LIKE '%Biological Chemistry%'")

b<-dbGetQuery(d$con,"SELECT Journal, COUNT(*) as Publications FROM JA GROUP BY Journal")

y<-1995:2015
sq<-"SELECT Year FROM (SELECT Year, COUNT (*) AS p FROM Meta GROUP BY Year HAVING p > 0)"
b<-dbGetQuery(d$con,sq)

sq<-"DELETE FROM Meta WHERE Year in (SELECT Year FROM (SELECT Year, COUNT (*) AS p FROM Meta GROUP BY Year HAVING p < 20))"
b<-dbGetQuery(d$con,sq)


sq<-" DELETE FROM JA WHERE Journal in (SELECT JOURNAL From JA WHERE DOI NOT LIKE '%SCOPUS%')"

sq<-"SELECT COUNT(*) as p FROM JA"

b<-dbGetQuery(d$con,sq)

sq<-"SELECT DISTINCT Journal, COUNT(*) FROM JA GROUP BY Journal"

b<-dbGetQuery(d$con,sq)

sq<-"SELECT title FROM journal_scopus limit 200"

dbGetQuery(d$con,"SELECT * From m WHERE DOI=='SCOPUS_ID:0034126157'")


a<-dbGetQuery(d$con,sq)

b[which(b$Journal %in% a$title),]

which(a$title %in% b$Journal)
head(b)

which(a$title %in% b$Journal)


d %>% tbl("JA") %>% filter(Journal %in% journaldf$title[jp]) %>% head()

shead("meta")

copy_to(d,"t",df=as.data.frame(journaldf[jp,]),temporary=T)

dbGetQuery(d$con,"SELECT Count(*) FROM JA")

dbGetQuery(d$con,"SELECT * FROM JA,t WHERE UPPER(JA.Journal) =UPPER(t.title)")
dbGetQuery(d$con,"DELETE FROM JA,t WHERE UPPER(JA.Journal) =UPPER(t.title)")

TA<-JA[!toupper(JA$Journal) %in% toupper(t$title),]

copy_to(d,"JA",df=TA,temporary=F)
db_drop_table(d$con,"JA")
dbGetQuery(d$con,"DELETE FROM JA FROM JA,t WHERE UPPER(Journal) =UPPER(t.title)")
