# FacultyNetworks
Ben Weinstein  
Saturday, November 01, 2014  

#Aim

Using tools from network ecology, we can begin to examine patterns of colloboration, specialization and compartamentalisation across academic disciplines. Using the google scholar's journal classification (go to scholar.google.com and select metrics), we can query the scopus database to find all article metadata from those journals for the last 20 years. 

#Approach

1. Get list of journals with subheadings and disciplines from google to create similarity lists.
2. Get the scopus ID for those journals
4. Query the scopus archive for all metadata on articles and authors between 1995-2014.
5. Compute betadiversity among fields
6. Visualize network.

#Code

The general workflow goes

* JournalClass.R - downloads the journal names and queries scopus for the IDS
* ByJournal.R - downloads metadata from the scopus api for each of those journal IDs
* Network.Rmd - compute betadiversity and visualize the network.
* To view the key script and results, check out network.html
