### PredictionModel.R

## 1. READ AND GENERATE SAMPLE DATA

# We are going to sample 2% of the total lines in the three text files.
# Furthermore, we will split this sample in training (80%), validation (10%)
# and test data (10%).

# Create "usePackage" function to install and load packages automatically
usePackage <- function(p) 
{
        if (!is.element(p, installed.packages()[,1]))
                install.packages(p, dep = TRUE)
        require(p, character.only = TRUE)
}

# Load required packages
usePackage("utils") # To unzip files
usePackage("qdapRegex") # Regular expression removal, extraction, and
# replacement tools to clean training set
usePackage("tm") # Basic framework for text mining applications within R
usePackage("slam") # To compute frequencies from tm Term-Document Matrices
usePackage("textreg") # To convert tm corpus into character vector
usePackage("parallel") # For parallel computation
usePackage("RWeka") # To tokenize words from text
usePackage("stringr") # To split columns from matrix as part of the process to
# make ngrams
usePackage("digest") # To apply cryptographical hash functions to benchmark text
usePackage("data.table") # For faster data manipulation

# Display software enviroment-useful for reproducible research purpose.
sessionInfo()

# R version 3.3.1 (2016-06-21)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)

# locale:
#   [1] LC_COLLATE=Spanish_Spain.1252  LC_CTYPE=Spanish_Spain.1252    LC_MONETARY=Spanish_Spain.1252
# [4] LC_NUMERIC=C                   LC_TIME=Spanish_Spain.1252    

# attached base packages:
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#   [1] data.table_1.9.6 digest_0.6.10    stringr_1.1.0    RWeka_0.4-29     textreg_0.1.3   
# [6] slam_0.1-38      tm_0.6-2         NLP_0.1-9        qdapRegex_0.6.0 

# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.7       chron_2.3-47      grid_3.3.1        plyr_1.8.4        gtable_0.2.0     
# [6] magrittr_1.5      scales_0.4.0      ggplot2_2.1.0     stringi_1.1.2     RWekajars_3.9.0-1
# [11] tools_3.3.1       munsell_0.4.3     colorspace_1.2-7  rJava_0.9-8 

# Download and decompress zip file. Corpora downloaded from HS Corpora:
# http://www.corpora.heliohost.org/ . More info about the Corpus in
# http://www.corpora.heliohost.org/aboutcorpus.html
usePackage("utils") # To unzip files
if(!file.exists("./Coursera-SwiftKey.zip")){
        download.file(url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      destfile= "./Coursera-SwiftKey.zip",
                      method = "curl")
        unzip("./Coursera-SwiftKey.zip")
}

# Function to create subsample of txt file
sampleText <- function(infile, outfile, seed, inlines, percent, readmode) {
        conn.in <- file(infile, readmode)  # readmode = "r" or "rb"
        conn.out <- file(outfile,"w")
        # for each line, flip a coin to decide whether to put it in sample
        set.seed(seed)
        in.sample <- rbinom(n=inlines, size=1, prob=percent)
        i <- 0  # have to use for-loop, not while-loop, bec of in.sample array
        num.out <- 0
        for (i in 1:(inlines+1)) {
                # read in one line at a time
                currLine <- readLines(conn.in, n=1, encoding="UTF-8", skipNul=TRUE)
                # if reached end of file, close all conns
                if (length(currLine) == 0) {
                        close(conn.out)
                        close(conn.in)
                        return(num.out)
                }
                # while not end of file, write out the selected line to file
                if (in.sample[i] == 1) {
                        writeLines(currLine, conn.out)
                        num.out <- num.out + 1
                }
        }
}

# Considering the size of the three text files (583 Mb) and my computer
# limitations, I have estimated that a sample of 2% should be sufficient for
# the goal of creating a predictive text app
myPercent <- 0.02
# Set seed for reproducible research
mySeed <- 3012

# Calculate number of lines of each file
testcon <- file("./final/en_US/en_US.blogs.txt", open = "r")
readsizeof <- 20000
nooflines <- 0
(while((linesread <- length(readLines(testcon, readsizeof))) > 0 ) 
        nooflines <- nooflines + linesread )
close(testcon)
blogNumLines <- nooflines
blogNumLines

testcon <- file("./final/en_US/en_US.news.txt", open = "rb")
readsizeof <- 20000
nooflines <- 0
(while((linesread <- length(readLines(testcon, readsizeof))) > 0 ) 
        nooflines <- nooflines + linesread )
close(testcon)
newsNumLines <- nooflines
newsNumLines

testcon <- file("./final/en_US/en_US.twitter.txt",open = "r")
readsizeof <- 20000
nooflines <- 0
(while((linesread <- length(readLines(testcon, readsizeof))) > 0 ) 
        nooflines <- nooflines + linesread )
close(testcon)
twitterNumLines <- nooflines
twitterNumLines

# Calculate number of lines of each file invoking OS command wc -l (Unix, i.e
# Linux and Mac OS)
# blogNumLines <- as.numeric(gsub('[^0-9]', '', system("wc -l ./final/en_US/en_US.blogs.txt", intern = TRUE)))
# newsNumLines <- as.numeric(gsub('[^0-9]', '', system("wc -l ./final/en_US/en_US.news.txt", intern = TRUE)))
# twitterNumLines <- as.numeric(gsub('[^0-9]', '', system("wc -l ./final/en_US/en_US.twitter.txt", intern = TRUE)))

# Generate subsamples, store them in txt files and get the number of sample
# lines
if (!file.exists("./blogSample.txt")) {
        blogSampleNumLines <- sampleText(infile = "./final/en_US/en_US.blogs.txt",
                                         outfile = "blogSample.txt",
                                         seed = mySeed,
                                         inlines = blogNumLines,
                                         percent = myPercent,
                                         readmode = "r")
}

if (!file.exists("./newsSample.txt")) {
        newsSampleNumLines <- sampleText(infile = "./final/en_US/en_US.news.txt",
                                         outfile = "newsSample.txt",
                                         seed = mySeed,
                                         inlines = newsNumLines,
                                         percent = myPercent,
                                         readmode = "rb") # must use readmode "rb" here, otherwise it breaks on a special character
}

if (!file.exists("./twitterSample.txt")) {
        twitterSampleNumLines <- sampleText(infile = "./final/en_US/en_US.twitter.txt",
                                            outfile = "twitterSample.txt",
                                            seed = mySeed,
                                            inlines = twitterNumLines,
                                            percent = myPercent,
                                            readmode = "r")
}

# Read sample txt files and join them in one object called "overallSample"
blogSample <- readLines(file("./blogSample.txt", open = "r"), encoding="UTF-8")
newsSample <- readLines(file("./newsSample.txt", open = "r"), encoding="UTF-8")
twitterSample <- readLines(file("./twitterSample.txt", open = "r"), encoding="UTF-8")
overallSample <- c(blogSample, newsSample, twitterSample)

# Get training data (80%)
numTrainingLines <- round(length(overallSample) * 0.8, 0)
trainingLines <- sort(sample(1:length(overallSample), numTrainingLines, replace = FALSE))
training <- overallSample[trainingLines]

# Get validation data (10%)
nonTrainingLines <- (1:length(overallSample))[-trainingLines]
numValidLines <- round(length(nonTrainingLines) * 0.5, 0)
validLines <- sort(sample(nonTrainingLines, numValidLines, replace = FALSE))
valid <- overallSample[validLines]

# Get testing data (10%)
testLines <- (1:length(overallSample))[-c(trainingLines, validLines)]
testing <- overallSample[testLines]

# Remove previous objetcs to save memory and save data
rm(sampleText,
   linesread,
   blogNumLines,
   newsNumLines,
   twitterNumLines,
   blogSample,
   newsSample,
   twitterSample,
   overallSample,
   myPercent,
   mySeed,
   testcon,
   readsizeof,
   nooflines,
   numTrainingLines,
   trainingLines,
   nonTrainingLines,
   numValidLines,
   validLines,
   testLines)
save.image(file = "./predictionModel1.RData")

## 2. APPLY TEXT TRANSFORMATIONS AND GENERATE UNIGRAM

# We will combine basic transformation functions included in tm plus some others
# from the qdapRegex package.

# Convert training character vector between encodings. It replaces
# non-convertible bytes in corpus with strings showing their hex codes
trainingNewEncoding <- sapply(training, function(x) iconv(enc2utf8(x), sub = "byte")) 

# Create cleanCorpus function to apply transformations to text
usePackage("qdapRegex") # Regular expression removal, extraction, and
# replacement tools to clean training set
cleanCorpus <- function(x) {  # input should be a tm corpus object
        x <- tm_map(x, content_transformer(tolower))
        x <- tm_map(x, content_transformer(removeNumbers))
        x <- tm_map(x, content_transformer(removePunctuation), preserve_intra_word_dashes = TRUE)
        x <- tm_map(x, content_transformer(stripWhitespace))
        x <- tm_map(x, content_transformer(rm_white_lead_trail))
        x <- tm_map(x, content_transformer(rm_hash))
        x <- tm_map(x, content_transformer(rm_tag))
        x <- tm_map(x, content_transformer(rm_twitter_url))
        x <- tm_map(x, content_transformer(rm_url))
        x <- tm_map(x, content_transformer(rm_emoticon))
        return(x)
}

# Create tm corpus from training set
usePackage("tm") # To clean training set
trainingCorpus <- Corpus(VectorSource(trainingNewEncoding))

# Clean training set. Be patient, it takes a while...
cleanTraining <- cleanCorpus(trainingCorpus)

# Create Term Document Matrix from clean corpus
tdmUnigram <- TermDocumentMatrix(cleanTraining)

# Generate unigram and save it as dataframe with 2 columns <freq> <word1>
usePackage("slam") # To compute frequencies from tm Term-Document Matrices
n1 <- data.frame(row_sums(tdmUnigram))
n1$word1 <- rownames(n1)
rownames(n1) <- NULL
colnames(n1) <- c("freq", "word1")
write.csv(n1, "n1.csv", row.names = FALSE) # Write n1.csv file

# Convert clean training corpus to character vector to save memory
usePackage("textreg") # To convert tm corpus into character vector
cleanTrainingText <- convert.tm.to.character(cleanTraining)

# Remove previous objetcs to save memory and save data
rm(trainingNewEncoding,
   cleanCorpus,
   trainingCorpus,
   cleanTraining,
   tdmUnigram)
save.image(file = "./predictionModel2.RData")

## 3. TREAT UNKNOWN WORDS

# To treat unknown words, we will follow the strategy of turning the problem
# back into a closed vocabulary one (Jurafsky & Martin 2015, Chapter 4, p. 12).

# Create vector with all rare words (i.e. those words with frequency less than
# 3)
rare <- subset(n1, freq < 3)
rare <- rare$word1
rare <- as.character(rare) # char vec

# Replace all rare words in corpus with UNK. To do so, create replaceRareWords
# function to check if each word belongs to rare set for each line in training.

replaceRareWords <- function(x, rarewords) {
        words <- unlist(strsplit(x, " "))
        match <- function(x, matches) {
                if (x %in% matches) {
                        x <- "UNK"
                } else {
                        x
                }
        }
        rv <- lapply(words, match, matches = rarewords)
        paste(unlist(rv), collapse = " ")
}

usePackage("parallel") # For parallel computation
numCores <- detectCores()
cl <- makeCluster(numCores)

results <- parLapply(cl, cleanTrainingText, replaceRareWords, rarewords = rare)
stopCluster(cl)

results <- unlist(results)
writeLines(results, "trainingUnk.txt")
trainingUnk <- readLines("./trainingUnk.txt")

# Check how many text chunks contain the word UNK
length(grep("UNK", trainingUnk)) 

# Remove previous objetcs to save memory and save data
rm(rare,
   replaceRareWords,
   numCores,
   cl,
   cleanTrainingText,
   results)
save.image(file = "./predictionModel3.RData")

## 4. GENERATE N-GRAMS

# Convert training with unknown words replaced characer vector into tm corpus
trainingUnkCorpus <- Corpus(VectorSource(trainingUnk))

# Make bigram tokens and Term-Document Matrix
usePackage("RWeka") # To tokenize words from text
usePackage("tm") # To split columns from matrix as part of the process to
# make ngrams
delim <- ' \r\n\t.,;:"()?!'
bigramTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min=2, max=2, delimiters=delim))
}
bigramTdmizer <- function(x) {
        tdm <- TermDocumentMatrix(x, control=list(tokenize=bigramTokenizer))
        return(tdm)
}
bigramTdm <- bigramTdmizer(trainingUnkCorpus)

# Change dtm object to dataframe with 3 columns <freq> <word1> <word2>
usePackage("slam") # To compute frequencies from tm Term-Document Matrices
n2 <- data.frame(row_sums(bigramTdm))
n2$term <- rownames(n2)
rownames(n2) <- NULL
usePackage("stringr") # To split columns from matrix as part of the process to
# make ngrams
words <- str_split_fixed(n2$term, " ", 2)  # split col2 by space into 2
n2 <- cbind(n2[ ,1], words)
colnames(n2) <- c("freq", "word1", "word2")

# Write n2.csv file and remove previous objetcs to save memory
write.csv(n2, "n2.csv", row.names = FALSE)
rm(bigramTokenizer,
   bigramTdmizer,
   bigramTdm,
   words,
   n2) # re-import later

# Make trigram tokens and Term-Document Matrix
trigramTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min=3, max=3, delimiters=delim))
}
trigramTdmizer <- function(x) {
        tdm <- TermDocumentMatrix(x, control=list(tokenize=trigramTokenizer))
        return(tdm)
}
trigramTdm <- trigramTdmizer(trainingUnkCorpus)

# Change dtm object to dataframe with 4 columns <freq> <word1> <word2> <word3>
n3 <- data.frame(row_sums(trigramTdm))
n3$term <- rownames(n3)
rownames(n3) <- NULL
colnames(n3) <- c("freq","term")
n3 <- subset(n3, n3$freq > 1)
words <- str_split_fixed(n3$term, " ", 3)  # split col2 by space into 3
n3 <- cbind(n3$freq, words)
colnames(n3) <- c("freq", "word1", "word2", "word3")

# Write n3.csv file and remove previous objetcs to save memory
write.csv(n3, "n3.csv", row.names = FALSE)
rm(trigramTokenizer,
   trigramTdmizer,
   trigramTdm,
   words,
   n3) # re-import later

# Make fourgram tokens and Term-Document Matrix
fourgramTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min=4, max=4, delimiters=delim))
}
fourgramTdmizer <- function(x) {
        tdm <- TermDocumentMatrix(x, control=list(tokenize=fourgramTokenizer))
        return(tdm)
}
fourgramTdm <- fourgramTdmizer(trainingUnkCorpus)

# Change dtm object to dataframe with 5 columns <freq> <word1> <word2> <word3>
# <word4>
n4 <- data.frame(row_sums(fourgramTdm))
n4$term <- rownames(n4)
rownames(n4) <- NULL
colnames(n4) <- c("freq","term")
n4 <- subset(n4, n4$freq > 1)
words <- str_split_fixed(n4$term, " ", 4)  # split col2 by space into 4
n4 <- cbind(n4$freq, words)
colnames(n4) <- c("freq", "word1", "word2", "word3", "word4")

# Write n4.csv file and remove previous objetcs to save memory
write.csv(n4, "n4.csv", row.names = FALSE)
rm(fourgramTokenizer,
   fourgramTdmizer,
   fourgramTdm,
   words,
   n4) # re-import later

# Make fivegram tokens and Term-Document Matrix
fivegramTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min=5, max=5, delimiters=delim))
}
fivegramTdmizer <- function(x) {
        tdm <- TermDocumentMatrix(x, control=list(tokenize=fivegramTokenizer))
        return(tdm)
}
fivegramTdm <- fivegramTdmizer(trainingUnkCorpus)

# Change dtm object to dataframe with 6 columns <freq> <word1> <word2> <word3>
# <word4> <word5>
n5 <- data.frame(row_sums(fivegramTdm))
n5$term <- rownames(n5)
rownames(n5) <- NULL
colnames(n5) <- c("freq","term")
n5 <- subset(n5, n5$freq > 1)
words <- str_split_fixed(n5$term, " ", 5)  # split col2 by space into 5
n5 <- cbind(n5$freq, words)
colnames(n5) <- c("freq", "word1", "word2", "word3", "word4", "word5")

# Write n5.csv file, remove previous objetcs to save memory and save data
write.csv(n5, "n5.csv", row.names = FALSE)
rm(trainingUnkCorpus,
   delim,
   fivegramTokenizer,
   fivegramTdmizer,
   fivegramTdm,
   words,
   n5) # re-import later
save.image(file = "./predictionModel4.RData")

## 5. BUILD PREDICTIVE MODEL

# The stupid backoff smoothing technique is chosen to build the following
# predictive model. In essence, stupid backoff find if n-gram has been seen,
# if not, multiply by alpha and back off to lower gram model.When training sets
# are considerably big, it performs at the same level than other more refined
# smoothing techniques like Kneser-Ney or Katz backoff.

# For more information about stupid backoff go to:
# - Brants et al. (2007): http://www.aclweb.org/anthology/D07-1090.pdf
# - Phil Ferriere's JHU Data Science Capstone Final Report:
# https://rpubs.com/pferriere/dscapreport

# Load profanity list with more than 1,300 bad words obtained from
# https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
if(!file.exists("./swearWords.txt")){
        download.file(url = "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",
                      destfile= "./swearWords.txt",
                      method = "curl")
}
profanity <- scan("./swearWords.txt", what = "character", sep = "\n", encoding = "UTF-8")

# Load ngrams
if (!exists("n5")) {
        n5 <- read.csv("n5.csv", stringsAsFactors=FALSE)
}
if (!exists("n4")) {
        n4 <- read.csv("n4.csv", stringsAsFactors=FALSE)
}
if (!exists("n3")) {
        n3 <- read.csv("n3.csv", stringsAsFactors=FALSE)
}
if (!exists("n2")) {
        n2 <- read.csv("n2.csv", stringsAsFactors=FALSE)
}

# Check ngrams size in disk
ngramsSize <- function(x) {
        format(object.size(x), units="MB") # x is a ngram with data.frame class
}

ngramsSize(n1)
ngramsSize(n2)
ngramsSize(n3)
ngramsSize(n4)
ngramsSize(n5)

# Here is when the algorithm starts with several pre-process functions
# Function that cleans a phrase
usePackage("tm") # Basic framework for text mining applications within R
cleanPhrase <- function(x) {  # input should be a phrase
        x <- tolower(x)
        x <- removePunctuation(x, preserve_intra_word_dashes = TRUE)
        x <- stripWhitespace(x)
        return(x)
}
# Example of use:
# cleanPhrase("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")

# Function that returns the last N words of cleaned phrase, in a character
# vector
getLastWords <- function(x, n) {
        x <- cleanPhrase(x)
        words <- unlist(strsplit(x, " "))
        len <- length(words)
        if (n < 1) {
                stop("GetLastWords() error: number of words  < 0")
        }
        if (n > len) {
                n <- len
        }
        if (n==1) {
                return(words[len])
        } else {
                rv <- words[len]
                for (i in 1:(n-1)) {
                        rv <- c(words[len-i], rv)
                }
                rv
        }
}
# Example of use:
# getLastWords("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", n = 4)

# Functions to check n-gram for x. Returns df with cols: [nextword] [MLE]
check5Gram <- function(x, n5, getNrows) {
        words <- getLastWords(x, 4)
        match <- subset(n5, word1 == words[1] & word2 == words[2]
                        & word3 == words[3] & word4 == words[4])
        match <- subset(match, select = c(word5, freq))
        match <- match[order(-match$freq), ]
        sumfreq <- sum(match$freq)
        match$freq <- round(match$freq / sumfreq * 100)
        colnames(match) <- c("nextword","n5.MLE")
        if (nrow(match) < getNrows) {
                getNrows <- nrow(match)
        }
        match[1:getNrows, ]
}
# Example of use:
# words <- getLastWords(cleanPhrase("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"), n = 4)
# check5Gram(words, n5 = n5, getNrows = 5)
# So there is no fivegram with the combination "and a case of (predicted word)"

check4Gram <- function(x, n4, getNrows) {
        words <- getLastWords(x, 3)
        match <- subset(n4, word1 == words[1] & word2 == words[2]
                        & word3 == words[3])
        match <- subset(match, select = c(word4, freq))
        match <- match[order(-match$freq), ]
        sumfreq <- sum(match$freq)
        match$freq <- round(match$freq / sumfreq * 100)
        colnames(match) <- c("nextword","n4.MLE")
        if (nrow(match) < getNrows) {
                getNrows <- nrow(match)
        }
        match[1:getNrows, ]
}
# Example of use:
# words <- getLastWords(cleanPhrase("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"), n = 3)
# check4Gram(words, n4 = n4, getNrows = 5)
# So there are several fourgrams with the combination "a case of (predicted word)". The one who gets a higher score is "a case of the" (score = 43) followed by "bombers" (14), corona (14), etc.

check3Gram <- function(x, n3, getNrows) {
        words <- getLastWords(x, 2)
        match <- subset(n3, word1 == words[1] & word2 == words[2])
        match <- subset(match, select = c(word3, freq))
        match <- match[order(-match$freq), ]
        sumfreq <- sum(match$freq)
        match$freq <- round(match$freq / sumfreq * 100)
        colnames(match) <- c("nextword","n3.MLE")
        if (nrow(match) < getNrows) {
                getNrows <- nrow(match)
        }
        match[1:getNrows, ]
}
# Example of use:
# words <- getLastWords(cleanPhrase("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"), n = 2)
# check3Gram(words, n3 = n3, getNrows = 5)
# So there are several trigrams with the combination "case of (predicted word)". The one who gets a higher score is "case of the" (score = 22) followed by "UNK" (unknown word) (19), a (16), etc.

check2Gram <- function(x, n2, getNrows) {  # n4 df should already exist
        words <- getLastWords(x, 1)
        match <- subset(n2, word1 == words[1])
        match <- subset(match, select = c(word2, freq))
        match <- match[order(-match$freq), ]
        sumfreq <- sum(match$freq)
        match$freq <- round(match$freq / sumfreq * 100)
        colnames(match) <- c("nextword","n2.MLE")
        if (nrow(match) < getNrows) {
                getNrows <- nrow(match)
        }
        match[1:getNrows, ]
}
# Example of use:
# words <- getLastWords(cleanPhrase("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"), n = 1)
# check2Gram(words, n2 = n2, getNrows = 5)
# So there are several bigrams with the combination "of (predicted word)". The one who gets a higher score is "of the" (score = 22) followed by "UNK" (6), a (4), etc. 

# Function that computes stupid backoff score (following Brants et al. (2007),
# alpha = 0.4). See the "scoreNgrams" first
SBScore <- function(alpha = 0.4, x5, x4, x3, x2) { # x5 to x1 vectors of the data frame created in the next function "scoreNgrams"
        score <- 0
        if (x5 > 0) {
                score <- x5
        } else if (x4 >= 1) {
                score <- x4 * alpha
        } else if (x3 > 0) {
                score <- x3 * alpha * alpha
        } else if (x2 > 0) {
                score <- x2 * alpha * alpha * alpha
        }
        return(round(score,1))
}
# Example of use:
# SBScore(alpha = 0.4, 0, 43, 22, 22)

# Example of use:
# SBScore(alpha = 0.4, 0, 14, 19, 6)

# Finally, we get to the final function that estimates the candidates and their
# stupid backoff scores
myPredictiveModel <- function(x, alpha = 0.4, nrows = 20, showNresults = 5, removeProfanity = TRUE, outputType = TRUE) {
        
        # - "x" is the phrase whose next word is going to be predicted so it has
        # to be a character vector (between quotation marks) of length 1
        # - "alpha" is the penalty rate set beforehand (normally equal to 0.4)
        # - "nrows" is the number of candidates that you want to be
        # considered in the prediction
        # "showNresults" is the number of candidates that you want to show as
        # result
        # - "removeProfanity" allows you to remove bad words or don't 
        # - "outputType" allows you to get a character vector-if equeal to TRUE-
        # with the proposed candidates or a table-if equeal to FALSE-with those
        # candidates and their scores. It will useful to design the Shiny App
        
        # get dfs from parent env
        n5.match <- check5Gram(x, n5, nrows)
        n4.match <- check4Gram(x, n4, nrows)
        n3.match <- check3Gram(x, n3, nrows)
        n2.match <- check2Gram(x, n2, nrows)
        # merge dfs, by outer join (fills zeroes with NAs)
        merge5n4 <- merge(n5.match, n4.match, by="nextword", all=TRUE)
        merge4n3 <- merge(merge5n4, n3.match, by="nextword", all=TRUE)
        merge3n2 <- merge(merge4n3, n2.match, by="nextword", all=TRUE)
        df <- subset(merge3n2, !is.na(nextword))  # rm any zero-match results
        
        # Define empty result
        nextwords <- ""
        if (x == "") {
                return(c(" ", " ", " ", " ", " "))
        }
        
        # No sense words treatment: return ????
        if (nrow(df) == 0 & outputType == TRUE) {
                return(c("????", " ", " ", " ", " "))
        }
        else if (nrow(df) == 0 & outputType == FALSE) {
                col1 <- c("????", " ", " ", " ", " ")
                col2 <- c(" ", " ", " ", " ", " ")
                df <- data.frame(col1, col2)
                colnames(df) <- c("Next word candidates","Stupid backoff score")
        }
        
        else if (nrow(df) > 0) {
                df <- df[order(-df$n5.MLE, -df$n4.MLE, -df$n3.MLE, -df$n2.MLE), ]
                df[is.na(df)] <- 0  # replace all NAs with 0
                # add in scores
                df$score <- mapply(SBScore, alpha = alpha, df$n5.MLE, df$n4.MLE,
                                   df$n3.MLE, df$n2.MLE)
                df <- df[order(-df$score), ]
                df <- df[df[1] != "unk", ] # remove unk. Becareful, after toekenization, the term "UNK" has been converted to lower case "unk"
                swearWordsIndex <- df[, 1] %in% profanity
                if (removeProfanity) {
                        if (any(swearWordsIndex) == TRUE) {
                                df[swearWordsIndex,][1] <- "<CENSORED>"
                        }
                }
                df <- df[,-(2:5)]
                rownames(df) <- NULL
                colnames(df) <- c("Next word candidates", "Stupid backoff score")
        }
        if (outputType == TRUE) {
                nextwords <- as.character(df[,1])[1:showNresults]
                return(nextwords) # character vector
        }
        else if (outputType == FALSE) {
                nextwords <- df[1:showNresults, ]
                return(nextwords) # dataframe     
        }
}

# Example of use:
myPredictiveModel("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")

# Save data
save.image(file = "./predictionModel5.RData")

## 6. MODEL PERFORMANCE ANALYSIS

# A "Next word prediction benchmark" was developed by alumni and its script
# "benchmark.R" can be download from https://github.com/hfoffani/dsci-benchmark.
# Password for data zip file: capstone4.

# Load required packages
usePackage("digest") # To apply cryptographical hash functions to benchmark text
usePackage("stringi") # For common string operations
usePackage("data.table") # To call rbindlist to makes one data.table from a list
# of many

################################################################################################
#
# 01. Loading of benchmark data sets
#
################################################################################################

# 01b. Get text from randomly selected tweets
################################################################################################

tweets <- readLines('data/tweets.txt', encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(tweets, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
        "7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4"

# 01c. Get text from randomly selected blog descriptions
################################################################################################

# make sure we can read it back in
blogs <- readLines('data/blogs.txt', encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(blogs, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
        "14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2"

################################################################################################
#
# 02. Define the functions used for benchmarking
#
################################################################################################

# 02a. Pre-processing functions
################################################################################################

# split.sentence
#  Returns a matrix containing in column i the part of the line before the ith word (sentence) 
#  and the ith word (nextWord).
#  The function is used in benchmark to generate and evaluate predictions for the partial lines.
split.sentence <- compiler::cmpfun(function(line) {
        require(stringi)
        # append a space to the sentence (to make sure we always create one result with only the 
        # last word missing)
        sent <- paste0(line, ' ')
        
        sep <- stri_locate_all_regex(line, 
                                     pattern = '[^\\w\'@#\u2018\u2019\u201b]+', 
                                     omit_empty=T, 
                                     case_insensitive=T)[[1]]
        sapply(seq_len(nrow(sep)), 
               function(i) {
                       c(sentence=ifelse(i>1, substr(line, 1, sep[i-1,2]), ''), 
                         nextWord=tolower(substr(line, max(sep[i-1,2]+1, 1), min(nchar(line), sep[i,1]-1)))
                       )
               })
}, options=list(optimize=3))


# 02b. Benchmarking function
################################################################################################

# benchmark
#  Evaluates the performance of a next word prediction algorithm based on the provided test data-
#  set(s).
#
#  Parameters
#   FUN         Function that produces the next word prediction. The function should take a single 
#               character value as first input and return a vector of character values represen-
#               ting the top-3 predictions (with the 1st value being the first prediction).
#   ...         Additional parameters to pass to FUN.
#   sent.list   Named list of character vectors containing the text lines used for the benchmark.
#   ext.output  If TRUE, return additional details about the R environment and loaded packages 
#               after completing the benchmark.

benchmark <- compiler::cmpfun(function(FUN, ..., sent.list, ext.output=T) {
        require(stringi)
        require(digest)
        require(data.table)
        
        result <- rbindlist(lapply(names(sent.list), 
                                   function(list.name) {  
                                           sentences <- sent.list[[list.name]]
                                           
                                           score <- 0
                                           max.score <-0
                                           hit.count.top3 <- 0
                                           hit.count.top1 <- 0
                                           total.count <- 0
                                           time <- system.time({
                                                   for (sent in sentences) {
                                                           split <- split.sentence(sent[1])
                                                           max.score <- max.score + ncol(split)*3
                                                           total.count <- total.count + ncol(split)
                                                           rank <- sapply(seq_len(ncol(split)),
                                                                          function(i) {
                                                                                  min(which(FUN(split[1,i], ...)==split[2,i]),4)
                                                                          })
                                                           score <- score + sum(4-rank)
                                                           hit.count.top3 <- hit.count.top3 + sum(rank<4)
                                                           hit.count.top1 <- hit.count.top1 + sum(rank==1)
                                                   }
                                           })
                                           
                                           list('list.name' = list.name,
                                                'line.count' = length(sentences),
                                                'word.count' = sum(stri_count_words(sentences)),
                                                'hash' = digest(paste0(sentences, collapse = '||'), algo='sha256', serialize=F),
                                                'score' = score,
                                                'max.score' = max.score,
                                                'hit.count.top3' = hit.count.top3,
                                                'hit.count.top1' = hit.count.top1,
                                                'total.count' = total.count,
                                                'total.runtime' = time[3]
                                           )               
                                   }), use.names=T)
        
        setkey(result, list.name)
        
        # The overall scores are calculated weighting each data set equally (independent of the 
        # number of lines in each dataset).
        overall.score.percent = 100 * result[,sum(score/max.score)/.N]
        overall.precision.top3 = 100 * result[,sum(hit.count.top3/total.count)/.N]
        overall.precision.top1 = 100 * result[,sum(hit.count.top1/total.count)/.N]
        average.runtime = 1000 * result[,sum(total.runtime)/sum(total.count)]
        number.of.predictions = result[,sum(total.count)]
        total.mem.used = sum(unlist(lapply(ls(.GlobalEnv),
                                           function(x) {
                                                   object.size(get(x,
                                                                   envir = .GlobalEnv,
                                                                   inherits = FALSE))
                                           })))/(1024^2)
        cat(sprintf(paste0('Overall top-3 score:     %.2f %%\n',
                           'Overall top-1 precision: %.2f %%\n',
                           'Overall top-3 precision: %.2f %%\n',
                           'Average runtime:         %.2f msec\n',
                           'Number of predictions:   %d\n',
                           'Total memory used:       %.2f MB\n'),
                    overall.score.percent,
                    overall.precision.top1,
                    overall.precision.top3,
                    average.runtime,
                    number.of.predictions,
                    total.mem.used
        ))
        
        cat('\nDataset details\n')
        for (p.list.name in result$list.name) {
                res <- result[list(p.list.name)]
                cat(sprintf(paste0(' Dataset "%s" (%d lines, %d words, hash %s)\n',
                                   '  Score: %.2f %%, Top-1 precision: %.2f %%, Top-3 precision: %.2f %%\n'
                ),
                p.list.name,
                res$line.count,
                res$word.count,
                res$hash,
                100 * res$score/res$max.score,
                100 * res$hit.count.top1/res$total.count,
                100 * res$hit.count.top3/res$total.count
                ))
        }
        
        if (ext.output==T) {
                packages <- sort(stri_replace_first_fixed(search()[stri_detect_regex(search(), 
                                                                                     '^package:')], 
                                                          'package:', ''))
                
                cat(sprintf(paste0('\n\n%s, platform %s\n', 
                                   'Attached non-base packages:   %s\n',
                                   'Unattached non-base packages: %s'
                ),
                sessionInfo()$R.version$version.string,
                sessionInfo()$platform,
                paste0(sapply(sessionInfo()$otherPkgs, 
                              function(pkg) {
                                      paste0(pkg$Package, ' (v', pkg$Version, ')')
                              }), 
                       collapse = ', '),
                paste0(sapply(sessionInfo()$loadedOnly, 
                              function(pkg) { 
                                      paste0(pkg$Package, ' (v', pkg$Version, ')')
                              }), 
                       collapse = ', ')
                ))
        }
}, options=list(optimize =3))

################################################################################################
#
# 03. Define the wrapper function to be called by benchmark
#
################################################################################################

# As an example, we create a very simple baseline algorithm which always returns
# the three most frequent English words.
predict.baseline <- function(x){c('the', 'on', 'a')}


################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
benchmark(predict.baseline, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)

benchmark(myPredictiveModel, showNresults = 3,
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)

# Benchmark results
# Overall top-3 score:     14.38 %
# Overall top-1 precision: 10.48 %
# Overall top-3 precision: 17.60 %
# Average runtime:         27.65 msec
# Number of predictions:   28464
# Total memory used:       53.99 MB

# Dataset details
#  Dataset "blogs" (599 lines, 14587 words, hash 14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2)
#   Score: 14.09 %, Top-1 precision: 10.26 %, Top-3 precision: 17.28 %
#  Dataset "tweets" (793 lines, 14071 words, hash 7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4)
#   Score: 14.67 %, Top-1 precision: 10.70 %, Top-3 precision: 17.92 %

# R version 3.3.1 (2016-06-21), platform x86_64-w64-mingw32/x64 (64-bit)
# Attached non-base packages:   stringi (v1.1.2), data.table (v1.9.6), digest (v0.6.10), stringr (v1.1.0), RWeka (v0.4-29), textreg (v0.1.3), slam (v0.1-38), tm (v0.6-2), NLP (v0.1-9), qdapRegex (v0.6.0)
# Unattached non-base packages: Rcpp (v0.12.7), chron (v2.3-47), grid (v3.3.1), plyr (v1.8.4), gtable (v0.2.0), magrittr (v1.5), scales (v0.4.0), ggplot2 (v2.1.0), RWekajars (v3.9.0-1), tools (v3.3.1), munsell (v0.4.3), compiler (v3.3.1), colorspace (v1.2-7), rJava (v0.9-8)

# Repeat the process with testing data
# Get 700 randomly selected lines from testing data
set.seed(3012)
testingSample <- sample(testing, 700, replace = FALSE)
writeLines(testingSample, "testingSample.txt")
testingSample <- readLines("./testingSample.txt", encoding = 'UTF-8')

# Verify checksum of loaded lines
digest(paste0(testingSample, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
        "568133be7f6b1f9c150f476c4278c75ef493775ff5a26e4e7625ce2832950a67"

# Perform the benchmark again
benchmark(myPredictiveModel, showNresults = 3,
          sent.list = list('testingSample' = testingSample), 
          ext.output = T)

# Benchmark results
# Overall top-3 score:     16.11 %
# Overall top-1 precision: 12.01 %
# Overall top-3 precision: 19.63 %
# Average runtime:         27.74 msec
# Number of predictions:   17471
# Total memory used:       54.12 MB

# Dataset details
# Dataset "testingSample" (700 lines, 17514 words, hash fa504de72f5ac78745760703d11fd8823edd36a103fd0cda8827d14406931616)
# Score: 16.11 %, Top-1 precision: 12.01 %, Top-3 precision: 19.63 %


# R version 3.3.1 (2016-06-21), platform x86_64-w64-mingw32/x64 (64-bit)
# Attached non-base packages:   stringi (v1.1.2), data.table (v1.9.6), digest (v0.6.10), stringr (v1.1.0), RWeka (v0.4-29), textreg (v0.1.3), slam (v0.1-38), tm (v0.6-2), NLP (v0.1-9), qdapRegex (v0.6.0)
# Unattached non-base packages: Rcpp (v0.12.7), chron (v2.3-47), grid (v3.3.1), plyr (v1.8.4), gtable (v0.2.0), magrittr (v1.5), scales (v0.4.0), ggplot2 (v2.1.0), RWekajars (v3.9.0-1), tools (v3.3.1), munsell (v0.4.3), compiler (v3.3.1), colorspace (v1.2-7), rJava (v0.9-8)

# Save data
save.image(file = "./predictionModel6.RData")

## 7. PREPARE DATA FOR SHINY APP

# Remove useless objects
rm(training,
   valid,
   testing,
   trainingUnk,
   blogs,
   tweets,
   testingSample,
   benchmark,
   ngramsSize,
   predict.baseline,
   split.sentence)

# Save final data for the Shiny app as "inputData"
save.image(file = "./inputData.RData")





