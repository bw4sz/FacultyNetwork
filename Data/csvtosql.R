require(dplyr)
library(RSQLite)
library(stringr)
#Create database
my_db<-src_sqlite(path = "C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3",create=T)

tocompare<-read.table("C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",row.names=NULL,
                      header=T,sep=",",fill=T,colClasses = c(rep(NA,4),"NULL",rep(NA,5)))

copy_to(my_db, tocompare, "metadata", temporary = F)

#The first table is going to be Journal and DOI
my_db %>% tbl("metadata") %>% collect() %>% distinct(DOI) %>% select(DOI,Journal) %>% copy_to(dest=my_db,name="JA",indexes=list("Journal"),temporary=F)
  
#The second table is DOI Order and Author Citation Year
my_db %>% tbl("metadata") %>% collect() %>% select(DOI,Order,Author,Citations,Year) %>% copy_to(dest=my_db,name="Meta",indexes=list("DOI"),temporary=F)

db_drop_table(my_db$con,"metadata")
db_drop_table(my_db$con,"aff")
