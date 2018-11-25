options(java.parameters = "-Xmx8000m")
library(tm)
library(stylo)
library(data.table)
library(RWeka)
library(dplyr)
library(stringr)
con <- file("en_US.blogs.txt")
blogs <- readLines(con)
close(con)

con <- file("en_US.news.txt", open="rb")
news <- readLines(con)
close(con)

con <- file("en_US.twitter.txt")
twitter <- readLines(con)
close(con)
rm(con)

source('functions/sampleData.R')

# These percentages were chosen to give us roughly 80,000 thousands samples in each variable
# This action was performed due to memory limitations
set.seed(1738)
blogs_Sample <- sampleData(blogs,percent = 0.1)
news_Sample <- sampleData(news,percent = 0.089)
twitter_Sample <- sampleData(twitter,percent = 0.035)

save(blogs_Sample, news_Sample, twitter_Sample, file = "mySampledData.RData")


rm(list=ls())

load("mySampledData.RData")

source("functions/cleanUserInput.R")

# Clean data, save file
blogsClean <- cleanUserInput(blogs_Sample)
newsClean <- cleanUserInput(news_Sample)
twitterClean <- cleanUserInput(twitter_Sample)

save(blogsClean, newsClean, twitterClean, file = "mycleanData.RData")

rm(list = ls())



load("mycleanData.RData")

# Create word list from cleaned data, save file
# These three files will be used to create a unigram list
dataBlogWords <- txt.to.words(blogsClean)
dataNewsWords <- txt.to.words(newsClean)
dataTwitterWords <- txt.to.words(twitterClean)

save(dataBlogWords, dataNewsWords, dataTwitterWords, file = "mytxt2words.RData")

 rm(dataBlogWords, dataNewsWords, dataTwitterWords)

# Convert cleaned data into a corpus
blogsCorpus <- VCorpus(VectorSource(blogsClean))
newsCorpus <- VCorpus(VectorSource(newsClean))
twitterCorpus <- VCorpus(VectorSource(twitterClean))

save(blogsCorpus, newsCorpus, twitterCorpus, file = "my_corpus234gram.RData")

rm(blogsClean, newsClean, twitterClean)

# Create word list from corpus variables, these variables will be used to create 2,3,4 gram tables
dataBlogWords <- txt.to.words(blogsCorpus)
dataNewsWords <- txt.to.words(newsCorpus)
dataTwitterWords <- txt.to.words(twitterCorpus)

save(dataBlogWords, dataNewsWords, dataTwitterWords, file = "mytxt2wordsCorpus.RData")

rm(blogsCorpus, newsCorpus, twitterCorpus)

source("functions/getNGrams.R")


# Create raw 2gram variables
raw2gramB <- getNGrams(dataBlogWords, 2)
raw2gramN <- getNGrams(dataNewsWords, 2)
raw2gramT <- getNGrams(dataTwitterWords, 2)

save(raw2gramT, raw2gramN, raw2gramB, file = "myraw2gram.RData")

rm(raw2gramT, raw2gramN, raw2gramB)


# Create raw 3gram variables
raw3gramB <- getNGrams(dataBlogWords, 3)
raw3gramN <- getNGrams(dataNewsWords, 3)
raw3gramT <- getNGrams(dataTwitterWords, 3)

save(raw3gramT, raw3gramN, raw3gramB, file ="myraw3gram.RData")

rm(raw3gramT, raw3gramN, raw3gramB)


# Create raw 4gram variables
load("mytxt2wordsCorpus.RData")
rm(dataBlogWords, dataNewsWords)
raw4gramT <- getNGrams(dataTwitterWords, 4)
rm(dataTwitterWords)
save(raw4gramT, file = "myraw4gram.RData")
rm(raw4gramT)

load("mytxt2wordsCorpus.RData")   
rm(dataTwitterWords, dataNewsWords) 
   
raw4gramB <- getNGrams(dataBlogWords, 4)
rm(dataBlogWords)
load("myraw4gram.RData")
save(raw4gramB, raw4gramT, file = "myraw4gram.RData")
rm(raw4gramB, raw4gramT)

load("mytxt2wordsCorpus.RData")   
rm(dataTwitterWords, dataBlogWords) 

raw4gramN <- getNGrams(dataNewsWords, 4)
rm(dataNewsWords)
load("myraw4gram.RData")
save(raw4gramT, raw4gramN, raw4gramB, file = "myraw4gram.RData")

rm(list=ls())

#######################################################################


# Create tables for 2,3,4 gram word counts
load("myraw2gram.RData")
table2gram <- table(c(raw2gramB, raw2gramN, raw2gramT))
save(table2gram, file = "myraw2gramTable.RData")
rm(list=ls())

load("myraw3gram.RData")
table3gram <- table(c(raw3gramB, raw3gramN, raw3gramT))
save(table3gram, file = "myraw3gramTable.RData")
rm(list=ls())

load("myraw4gram.RData")
table4gram <- table(c(raw4gramB, raw4gramN, raw4gramT))
save(table4gram, file = "myraw4gramTable.RData")
rm(list=ls())

######################################################################

# Create filtered tables

load("mytxt2words.RData")
raw1gram <- table(c(dataBlogWords, dataNewsWords, dataTwitterWords))
wf1gram <- data.frame(word=names(raw1gram), freq=as.numeric(raw1gram), stringsAsFactors = FALSE)
wf1gram <- filter(wf1gram, freq != 1)
wf1gram <- arrange(wf1gram, desc(freq))
save(wf1gram, file = "myraw1gramTableFiltered.RData")
rm(list = ls())

load("myraw2gramTable.RData")
wf2gram <- data.frame(word=names(table2gram), freq=as.numeric(table2gram), stringsAsFactors = FALSE)
wf2gram <- filter(wf2gram, freq != 1)
wf2gram <- arrange(wf2gram, desc(freq))
save(wf2gram, file = "myraw2gramTableFiltered.RData")
rm(list=ls())

load("myraw3gramTable.RData")
wf3gram <- data.frame(word=names(table3gram), freq=as.numeric(table3gram), stringsAsFactors = FALSE)
wf3gram <- filter(wf3gram, freq != 1)
wf3gram <- arrange(wf3gram, desc(freq))
save(wf3gram, file = "myraw3gramTableFiltered.RData")
rm(list=ls())

load("myraw4gramTable.RData")
wf4gram <- data.frame(word=names(table4gram), freq=as.numeric(table4gram), stringsAsFactors = FALSE)
wf4gram <- filter(wf4gram, freq != 1)
wf4gram <- arrange(wf4gram, desc(freq))
save(wf4gram, file = "myraw4gramTableFiltered.RData")
rm(list=ls())

###################################################################

# Create index for separating filtered tables after they've been concatenated

load("myraw1gramTableFiltered.RData")
load("myraw2gramTableFiltered.RData")
load("myraw3gramTableFiltered.RData")
load("myraw4gramTableFiltered.RData")

gram1 <- c(1,nrow(wf1gram))
gram2 <- c(gram1[2]+1, gram1[2]+nrow(wf2gram))
gram3 <- c(gram2[2]+1, gram2[2]+nrow(wf3gram))
gram4 <- c(gram3[2]+1, gram3[2]+nrow(wf4gram))
save(gram1, gram2, gram3, gram4, file = "divideNGram.RData")
rm(gram1, gram2, gram3, gram4)

# Concatenate tables, write .txt file
tableNGram <- rbind(wf1gram, wf2gram, wf3gram, wf4gram)
rm(wf1gram, wf2gram, wf3gram, wf4gram)

save(tableNGram, file = "mywriteTableFINAL.RData")

write.table(tableNGram, file = "NGramSortedFinal.txt", sep = "\t", row.names = FALSE)

