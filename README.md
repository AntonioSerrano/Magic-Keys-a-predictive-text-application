# Magic Keys #

[Magic Keys](https://antonioserrano.shinyapps.io/Magic_Keys_a_Coursera_Data_Science_Capstone_Project/) is a simple predictive text application that works pretty much like a cell phone's keyboard, making suggestion about the next word to be entered when writing an email or replaying a message. It is written in R and its [Shiny package](https://shiny.rstudio.com/) in order to build the interactive web application. This project was developed to complete the Capstone Project from the [Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science) at Coursera. To see more details about the n-gram model employed, you can have a look to [this short presentation](https://rstudio-pubs-static.s3.amazonaws.com/225005_19b7682c106044ef9a50e636a81ee0ae.html#/).

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=2592000)](https://github.com/AntonioSerrano/Magic-Keys-a-predictive-text-application/blob/master/LICENSE.txt)

----
    
## Getting Started ##

1. Install the dependencies for R:
    1. utils: to unzip files  
    `suppressWarnings(install.packages("utils")`
    2. qdapRegex: regular expression removal, extraction, and replacement tools to clean training.  
    `setsuppressWarnings(install.packages("qdapRegex"))`
    3. tm: basic framework for text mining applications within R.  
    `suppressWarnings(install.packages("tm"))`
    4. slam: to compute frequencies from tm Term-Document Matrices.  
    `suppressWarnings(install.packages("slam"))`
    5. textreg: to convert tm corpus into character vector.  
    `suppressWarnings(install.packages("textreg"))`
    6. parallel: for parallel computation.  
    `suppressWarnings(install.packages("parallel"))`
    7. RWeka: to tokenize words from text.  
    `suppressWarnings(install.packages("RWeka"))`
    8. stringr: to split columns from matrix as part of the process to make ngrams.  
    `suppressWarnings(install.packages("stringr"))`
    9. digest: to apply cryptographical hash functions to benchmark text.  
    `suppressWarnings(install.packages("digest"))`
    10. data.table: for faster data manipulation.  
    `suppressWarnings(install.packages("data.table"))`
    11. shiny: for compile web apps on R Studio servers.  
    `suppressWarnings(install.packages("shiny"))`
    12. DT: to display R dataframes as tables on HTML pages.  
    `suppressWarnings(install.packages("DT"))`

2. About RWeka and Mac OS. There seem to be a little problem between RWeka and Java on Mac OS. To solve it try this:
    1. On your terminal:  
    `sudo R CMD javareconf`
    2. On R:  
    `install.packages("rJava",type='source')`
    3. On terminal:  
    `sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/li`
    
## Future work ##

There is still much work to be done in relation to the n-gram model. Firs of all, the corpus should be augmented with more texts from areas beyond news. Second, state-of-the-art models are nowadays based on (Deep Learning)[https://ieeexplore.ieee.org/abstract/document/6472238]. It is worth to explore such DL models.
