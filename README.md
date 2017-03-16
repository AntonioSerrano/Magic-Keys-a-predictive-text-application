# Magic Keys #

Magic Keys is a predictive text application developed to complete the Capstone Project in the Data Science Specialization offered by Johns Hopkins University in collaboration with Swiftkey via Coursera.org.

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

2. About RWeka and Mac OS:
There seem to be a little problem between RWeka and Java on Mac OS. To solve it try this:
    1. On your terminal:
    `sudo R CMD javareconf`
    2. On R:
    `install.packages("rJava",type='source')`
    3. On terminal:
    `sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/li`
    
