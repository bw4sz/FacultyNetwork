require(dplyr)

#Create database
my_db <- src_sqlite( "Data/Meta.sqlite3", create = TRUE)     

#journal class
journaldf<-read.csv("C:/Users/Ben/Dropbox/FacultyNetwork/JournalID.csv",row.names=1)
copy_to( my_db, journaldf, "journal", temporary = FALSE)

#
tocompare<-read.table("C:/Users/Ben/Dropbox/FacultyNetwork/ParsedDataID.csv",row.names=NULL,header=T,sep=",",fill=T)
copy_to( my_db, tocompare, "metadata", temporary = FALSE)
