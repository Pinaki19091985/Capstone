
library(knitr); 
library(stringi); library(tm); library(ggplot2); 
library(wordcloud); library(SnowballC)

#Importing the data from specified link :
  
setwd("C:/Users/Pinaki Bose/Documents/Capstone/Next_Word_Predict")
if(!file.exists("Coursera-SwiftKey.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}
blogs <- readLines("final/en_US/en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("final/en_US/en_US.news.txt", warn = FALSE, encoding = "UTF-8")
twitter <- readLines("final/en_US/en_US.twitter.txt", warn = FALSE, encoding = "UTF-8")

#Sampling the data based on which corpus will be created in later stage.Random sampling method (2% per file) is justified because of the large corpus text data and my limited computation power:
  
set.seed(2626) # Make subsampling reproducible
sample_size <- 0.02 # Subsample to 2%

blogs_index <- sample(seq_len(length(blogs)),length(blogs)*sample_size)
news_index <- sample(seq_len(length(news)),length(news)*sample_size)
twitter_index <- sample(seq_len(length(twitter)),length(twitter)*sample_size)

blogs_sub <- blogs[blogs_index[]]
news_sub <- news[news_index[]]
twitter_sub <- twitter[twitter_index[]]

library(tm) # Load Text Mining library

#Corpus built of subsampled data :
  
corpus <- VCorpus(VectorSource(c(blogs_sub, news_sub, twitter_sub)), readerControl=list(reader=readPlain,language="en")) # Make corpus
rm(list= ls()[!(ls() %in% 'corpus')])
gc()

#Cleansing Corpus and remove non-ASCII.Then the following clean up steps are performed.
#Convert all words to lowercase.Eliminate punctuation.Eliminate numbers.Strip whitespace. 

corpus <- VCorpus(VectorSource(sapply(corpus, function(row) iconv(row, "latin1", "ASCII", sub="")))) 
corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
corpus <- tm_map(corpus, stripWhitespace) # Remove unneccesary white spaces
corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removeNumbers) # Remove numbers

# Convert corpus to words
# Can not convert corpus straigt to string vector as it will include metadata
corpus.df <- data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
corpus.str <- corpus.df[, 1]
corpus.words <- lapply(corpus.str, function(x) strsplit(x, " ", fixed = T)[[1]]) # split into words
rm(list= ls()[!(ls() %in% 'corpus.words')])
gc()

# Create quadgrams
library(NLP)
corpus.quadgrams = lapply(1:length(corpus.words), function(i) vapply(ngrams(corpus.words[[i]], 4), paste, "", collapse = " "))
corpus.quadgrams.ul <- unlist(corpus.quadgrams)
rm(corpus.quadgrams)
corpus.quadgram.counts <- as.data.frame(xtabs(~corpus.quadgrams.ul))
rm(corpus.quadgrams.ul)
colnames(corpus.quadgram.counts)[1] <- "Ngram"
corpus.quadgram.counts.sorted <- corpus.quadgram.counts[order(corpus.quadgram.counts$Freq, decreasing = T),]
rm(corpus.quadgram.counts)
corpus.quadgram.counts.sorted.pruned <- corpus.quadgram.counts.sorted[corpus.quadgram.counts.sorted$Freq>=2, ] 
# Not considering infrequent Ngrams
rm(corpus.quadgram.counts.sorted)
gc()
corpus.quadgram.counts.sorted.pruned$Ngram <- as.character(corpus.quadgram.counts.sorted.pruned$Ngram)

# Create trigrams
corpus.trigrams = lapply(1:length(corpus.words), function(i) vapply(ngrams(corpus.words[[i]], 3), paste, "", collapse = " "))
corpus.trigrams.ul <- unlist(corpus.trigrams)
rm(corpus.trigrams)
corpus.trigram.counts <- as.data.frame(xtabs(~corpus.trigrams.ul))
rm(corpus.trigrams.ul)
colnames(corpus.trigram.counts)[1] <- "Ngram"
corpus.trigram.counts.sorted <- corpus.trigram.counts[order(corpus.trigram.counts$Freq, decreasing = T),]
rm(corpus.trigram.counts)
corpus.trigram.counts.sorted.pruned <- corpus.trigram.counts.sorted[corpus.trigram.counts.sorted$Freq>=2, ] 
# Not considering infrequent Ngrams
rm(corpus.trigram.counts.sorted)
gc()
corpus.trigram.counts.sorted.pruned$Ngram <- as.character(corpus.trigram.counts.sorted.pruned$Ngram)

# Create bigrams
corpus.bigrams = lapply(1:length(corpus.words), function(i) vapply(ngrams(corpus.words[[i]], 2), paste, "", collapse = " "))
corpus.bigrams.ul <- unlist(corpus.bigrams)
rm(corpus.bigrams)
corpus.bigram.counts <- as.data.frame(xtabs(~corpus.bigrams.ul))
rm(corpus.bigrams.ul)
colnames(corpus.bigram.counts)[1] <- "Ngram"
corpus.bigram.counts.sorted <- corpus.bigram.counts[order(corpus.bigram.counts$Freq, decreasing = T),]
rm(corpus.bigram.counts)
corpus.bigram.counts.sorted.pruned <- corpus.bigram.counts.sorted[corpus.bigram.counts.sorted$Freq>=2, ] # Not considering infrequent Ngrams
rm(corpus.bigram.counts.sorted)
gc()
corpus.bigram.counts.sorted.pruned$Ngram <- as.character(corpus.bigram.counts.sorted.pruned$Ngram)

# Create unigrams
corpus.unigrams = lapply(1:length(corpus.words), function(i) vapply(ngrams(corpus.words[[i]], 1), paste, "", collapse = " "))
corpus.unigrams.ul <- unlist(corpus.unigrams)
rm(corpus.unigrams)
corpus.unigram.counts <- as.data.frame(xtabs(~corpus.unigrams.ul))
rm(corpus.unigrams.ul)
colnames(corpus.unigram.counts)[1] <- "Ngram"
corpus.unigram.counts.sorted <- corpus.unigram.counts[order(corpus.unigram.counts$Freq, decreasing = T),]
rm(corpus.unigram.counts)
corpus.unigram.counts.sorted.pruned <- corpus.unigram.counts.sorted[corpus.unigram.counts.sorted$Freq>=2, ] # Not considering infrequent Ngrams
rm(corpus.unigram.counts.sorted)
gc()
corpus.unigram.counts.sorted.pruned$Ngram <- as.character(corpus.unigram.counts.sorted.pruned$Ngram)

save(corpus.quadgram.counts.sorted.pruned, corpus.trigram.counts.sorted.pruned, corpus.bigram.counts.sorted.pruned, corpus.unigram.counts.sorted.pruned, file = 'newer_Ngrams.RData')



# Removal of Ngrams with frequencies less than 5 makes the app run faster in Shiny server

corpus.quadgram.counts.sorted.pruned <- corpus.quadgram.counts.sorted.pruned[corpus.quadgram.counts.sorted.pruned$Freq>=4, ]
corpus.quadgram.counts.sorted.pruned$Ngram <- as.character(corpus.quadgram.counts.sorted.pruned$Ngram)

corpus.trigram.counts.sorted.pruned <- corpus.trigram.counts.sorted.pruned[corpus.trigram.counts.sorted.pruned$Freq>=4, ]
corpus.trigram.counts.sorted.pruned$Ngram <- as.character(corpus.trigram.counts.sorted.pruned$Ngram)

corpus.bigram.counts.sorted.pruned <- corpus.bigram.counts.sorted.pruned[corpus.bigram.counts.sorted.pruned$Freq>=4, ]
corpus.bigram.counts.sorted.pruned$Ngram <- as.character(corpus.bigram.counts.sorted.pruned$Ngram)

corpus.unigram.counts.sorted.pruned <- corpus.unigram.counts.sorted.pruned[1, ]

save(corpus.quadgram.counts.sorted.pruned, corpus.trigram.counts.sorted.pruned, corpus.bigram.counts.sorted.pruned, corpus.unigram.counts.sorted.pruned, file = 'newer_Ngrams.RData')