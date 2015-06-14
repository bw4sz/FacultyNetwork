require(dplyr)

#Create database
my_db<-src_sqlite(path = "C:/Users/Ben/Dropbox/FacultyNetwork/Meta.sqlite3",create=T)

tocompare<-read.table("C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",row.names=NULL,header=T,sep=",",fill=T)
copy_to(my_db, tocompare, "metadata", temporary = F)
