# List of the packages
library(dplyr)
library(data.table)
library(qdap)
library(ngram)
library(tm)
library(RWeka)
library(stringr)
library(stringi)
library(NLP)

# Download and load the swiftkey file.
fileUrl <-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("Coursera-SwiftKey.zip")){
   download.file(fileUrl, destfile = "Coursera-SwiftKey.zip")
}
unzip("Coursera-SwiftKey.zip")
blogs <- readLines("final/en_US/en_US.blogs.txt",skipNul = TRUE, warn = TRUE)
news <- readLines("final/en_US/en_US.news.txt",skipNul = TRUE, warn = TRUE)
twitter <- readLines("final/en_US/en_US.twitter.txt",skipNul = TRUE, warn = TRUE)

# Generate sample data of size 5000 each from metadata
set.seed(123)
blogs_sample <- sample(blogs, size = 10000, replace = TRUE)
news_sample <- sample(news, size = 10000, replace = TRUE)
twitter_sample <- sample(twitter, size = 10000, replace = TRUE)

# Generete the sample total and write to a file
sampleTotal <- c(blogs_sample, news_sample, twitter_sample)
writeLines(sampleTotal, "./sampleTotal.txt")

# Remove old varibles (memory management)
rm(blogs,news,twitter,fileUrl)

# Clean the sample_set, create corpus for sample data
## Convert Character Vector between Encodings
corpus <- VCorpus(VectorSource(sampleTotal))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
corpus <- tm_map(corpus, content_transformer(tolower), lazy = TRUE)
corpus <- tm_map(corpus, content_transformer(removePunctuation), preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
apostropies <- function(x) gsub( "[^[:alnum:]']", " ", x)
corpus <- tm_map(corpus, content_transformer(apostropies))

## removing URLs and stop words (at, a, as, etc.)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, removeWords, stopwords("english")) 
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)
changetospace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, changetospace, "/|@|\\|")

# Removing Profanity Words
profanityWords = readLines('./profanity_words.txt')
corpus <- tm_map(corpus,removeWords, profanityWords)

## Print and save corpus data as plain text document
## showing some lines of the textcorpus
## for (i in 1:10){
##   print(corpus[[i]]$content)
## }
## Saving the final corpus
## saveRDS(corpus, file = "./corpus.RData")

## data framing finalcorpus
corpus_df <-data.frame(text=unlist(sapply(corpus,`[`, "content")),stringsAsFactors = FALSE)
head(corpus_df)

# N-Gram Tokenizer
# Unigrams----------- 
unigram <- NGramTokenizer(corpus_df, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
unigram <- data.frame(table(unigram))
unigram <- unigram[order(unigram$Freq,decreasing = TRUE),]
names(unigram) <- c("word1", "freq")
unigram$word1 <- as.character(unigram$word1)
write.csv(unigram[unigram$freq > 1,],"unigram.csv",row.names=F)
unigram <- read.csv("unigram.csv",stringsAsFactors = F)
saveRDS(unigram, file = "unigram.RData")
#--------------------

# Bigram ------------
bigram <- NGramTokenizer(corpus_df, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
bigram <- data.frame(table(bigram))
bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]
names(bigram) <- c("words","freq")
bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,
                     word2 = bigram$two,
                     freq = bigram$freq,
                     stringsAsFactors=FALSE)
write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")
#--------------------

# trigrams ----------
trigram <- NGramTokenizer(corpus_df, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
trigram <- data.frame(table(trigram))
trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]
names(trigram) <- c("words","freq")
trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
trigram <- data.frame(word1 = trigram$one,
                      word2 = trigram$two,
                      word3 = trigram$three,
                      freq = trigram$freq,
                      stringsAsFactors=FALSE)
write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")
#--------------------

# Quadgram ----------
quadgram <- NGramTokenizer(corpus_df, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
quadgram <- data.frame(table(quadgram))
quadgram <- quadgram[order(quadgram$Freq,decreasing = TRUE),]
names(quadgram) <- c("words","freq")
quadgram$words <- as.character(quadgram$words)
str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))
quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, 
                       stringsAsFactors=FALSE)
write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")
#--------------------