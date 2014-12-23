# FacultyNetworks
Ben Weinstein  
Saturday, November 01, 2014  

#Aim

Using tools from network ecology, we can begin to examine patterns of colloboration, specialization and compartamentalisation within academic departments. Using journal publications as a metric of similiarity among faculty members, we can compare the relative specialization of each department and compare academic niche breadth as a function of size, location and other measures of group interactions. I will begin with departments in Ecology and Evolution.

#Approach

1. Get list of journals with subheadings and disciplines from google to create similarity lists.
2. Get names of top academic departments in the US with a program in Ecology and Evolution.
3. Search the pubmed archives for journal title and affiliation.
4. Decompose API results into R metadata.
5. Perform network analysis on academic departments as a function of similarity in journal publications.



# List of journals and disciplines

Scrape the subcategory data from google scholar page, not the search engine, they don't allow that!
[Link](http://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio)




Category                               N
-----------------------------------  ---
Agronomycropscience                   20
Animalbehavior                        20
Animalhusbandry                       20
Atmosphericsciences                   20
Biochemistry                          20
Biodiversityconservationbiology       20
Biogeneral                            20
Bioinformatics                        20
Biophysics                            20
Biotechnology                         20
Birds                                 20
Botany                                20
Cellbiology                           20
Developmentalbiologyembryology        20
Ecology                               20
Environmentalgeologicalengineering    20
Environmentalsciences                 20
Evolutionarybiology                   20
Foodsciencetechnology                 20
Forestsforestry                       20
Geochemistrymineralogy                20
Geology                               20
Hydrology                             20
Insectsarthropods                     20
Marinesciencesfisheries               20
Microbiology                          20
Molecularbiology                      20
Mycology                              20
Oceanography                          20
Paleontology                          20
Pestcontrolpesticides                 20
Plantpathology                        20
Proteomicspeptides                    20
Soilsciences                          20
Sustainabledevelopment                20
Sustainableenergy                     20
Virology                              20
Woodsciencetechnology                 20
Zoology                               20


#Lists of Academic Departments

Scraping from [here](http://www.nescent.org/eog/graduateprogdatabase.php?orderby=PhD&order=asc)

125 Departments in the US.




  X  Institution                                        City           State   Program                                       
---  -------------------------------------------------  -------------  ------  ----------------------------------------------
  1  Boise State University                             Boise          ID      Biology                                       
  2  California State Polytechnic University - Pomona   Pomona         CA      Biological Sciences                           
  3  College Of Staten Island, CCNY                     New York       NY      Biology                                       
  4  College Of William And Mary                        Williamsburg   VA      Biology                                       
  5  Colorado State University                          Fort Collins   CO      Biology                                       
  6  Columbia University                                New York       NY      Ecology, Evolution, and Environmental Biology 

#Associate faculty with a given department

I could either do this by scraping individual faculty pages (hard) or using crossref to better describe affilication.

#Search pubmed (scopus?) for articles

Let's begin slowly. I want to search an affiliation for any of the journal titles i have in my categories. I found the rentrez package (thanks RopenSCI!) and i want to give it a try. 



A couple annoying things i've found so far

* Pubmed just gives first author affiliation.
* The coverage of ecology of journals is weak.

# Create network

* Associate each journal article with a faculty member, then a department. Use the categories from step 1 to describe each faculty member as to which discipline they fall into. 

# Network analysis

It would be interesting to look at a number of ideas.

Within each department.

* Degree of niche overlap and compartmentalization in departments. How similiar are faculty members? 

* Forbidden links: are there certain kinds of people which are never in the same faculty, this could highlight areas of potential growth. Like wood science and evolutionary biology. Test co-occurrence rates against a null model of richness and find which combinations occur less often then chance.

* I could build a interesting little shiny server which shows a network and allows people to add themselves into each department. Great for a job talk.

* Specialization compared to the overall pool of researchers. Where do we see aggregations of like-minded scientists? Does chicago really have more theoriticians than elsewhere? 


