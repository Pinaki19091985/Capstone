# This function predicts the next best word

library(tm)
load("Ngrams_based_on_swiftkey_data.RData") # Load Ngrams

predict <- function(input) {
  if(nchar(input) > 0)
  {
    # Clean up input and split into words
    input <- gsub("[^A-Za-z ]" ,"", input)
    input <- tolower(input)
    input <- strsplit(input, " ")
    input <- unlist(input)
    input <- rev(input)
    
    # Build up queries, e.g. last two words for search against trigrams
    # Longest query will be last three words for search against quadgrams
    qlen <- length(input)
    if (qlen > 3)
    {
      qlen <- 3
    }
    
    alpha <- 0.4 # Factor for adjusting probabilities when backing off from Ngrams to N-1grams
    
    # Query Ngrams, a couple of if statements of customise to query length.
    # If there are no results we will output the most frequent unigram.
    # This does not depend on the query but solely on the corpus.
    unigram.match.index <- 1
    unigram.matches <- corpus.unigram.counts.sorted.pruned[unigram.match.index, ]
    unigram.matches$Prob <- unigram.matches$Freq/sum(unigram.matches$Freq)
    unigram.matches$Evidence <- "Unigrams"
    unigram.matches$AdjProb <- unigram.matches$Prob*alpha*alpha*alpha  
    
    # The input has to be at least one word. So we can always query bigrams.
    search1 <- input[1]
    bigram.match.index <- grep(paste0("^", search1, " "), corpus.bigram.counts.sorted.pruned$Ngram)
    if(length(bigram.match.index) > 0 )
    {
      bigram.matches <- corpus.bigram.counts.sorted.pruned[bigram.match.index, ]
      bigram.matches$Prob <- bigram.matches$Freq/sum(bigram.matches$Freq)
      bigram.matches$Evidence <- "Bigrams"
      bigram.matches$AdjProb <- bigram.matches$Prob*alpha*alpha
    }
    
    if (qlen > 1) # If the input is at least two words we can also query trigrams.
    {
      search2 <- paste(input[2], input[1], sep = " ")
      trigram.match.index <- grep(paste0("^", search2, " "), corpus.trigram.counts.sorted.pruned$Ngram)
      if(length(trigram.match.index) > 0 )
      {
        trigram.matches <- corpus.trigram.counts.sorted.pruned[trigram.match.index, ]
        trigram.matches$Prob <- trigram.matches$Freq/sum(trigram.matches$Freq)
        trigram.matches$Evidence <- "Trigrams"
        trigram.matches$AdjProb <- trigram.matches$Prob*alpha
      }
    }
    
    if (qlen > 2) # If the input is at least three words we can also query quadgrams.
    {  
      search3 <- paste(input[3], input[2], input[1], sep = " ")
      quadgram.match.index <- grep(paste0("^", search3, " "), corpus.quadgram.counts.sorted.pruned$Ngram)
      if(length(quadgram.match.index) > 0 )
      {
        quadgram.matches <- corpus.quadgram.counts.sorted.pruned[quadgram.match.index, ]
        quadgram.matches$Prob <- quadgram.matches$Freq/sum(quadgram.matches$Freq)
        quadgram.matches$Evidence <- "Quadgrams"
        quadgram.matches$AdjProb <- quadgram.matches$Prob
      }
    }
    
    # Combine results from searches against quad-, tri-, bi- and unigrams
    if((qlen > 2) && (length(quadgram.match.index) > 0))
    {
      all.matches <- rbind(quadgram.matches, trigram.matches, bigram.matches)
    } 
    else
    {
      if((qlen > 1) && (length(trigram.match.index) > 0))
      {
        all.matches <- rbind(trigram.matches, bigram.matches)
      }
      else
      {
        if(length(bigram.match.index) > 0)
        {
          all.matches <- bigram.matches
        }
        else
        {
          all.matches <- unigram.matches
        }
      }
    }
    
    # Sort results by adjusted probabilities
    all.matches <- all.matches[order(all.matches$AdjProb, decreasing = T), ]
        return(all.matches)
  } 
}