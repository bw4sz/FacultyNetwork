require(dplyr)

#Create database
my_db <- src_sqlite( "Data/Meta.sqlite3", create = TRUE)     

tocompare<-read.table("C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",row.names=NULL,header=T,sep=",",fill=T,nrows = 1000)
copy_to(my_db, tocompare, "metadata", temporary = T)
