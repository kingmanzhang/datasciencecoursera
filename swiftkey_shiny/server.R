# This is a Shiny App that predicts fuel efficiency based on transmission type, number of cylinders and weight(lb)

library(shiny)
require(tm)
require(slam)
require(hash)
require(triebeard)
require(tidyverse)

# load predictive model
loadModel <- function(file = "mymodel.rdata"){
  mylist <- list.load(file)
  trie.word <- trie(keys = mylist$keys, values = mylist$words)
  trie.prob <- trie(keys = mylist$keys, values = mylist$prob)
  list(trie.prob = trie.prob, trie.word = trie.word, ngram = mylist$ngram, top.unigram = mylist$top.unigram)
}
savedModel <- loadModel(file = "mymodel.02percent.4gram.rdata")

processSentence <- function(sentence) {
  sentence <- removePunctuation(sentence, preserve_intra_word_contractions = TRUE,
                                preserve_intra_word_dashes = TRUE,
                                ucp = TRUE)
  sentence <- removeNumbers(sentence)
  #sentence <- removeWords(sentence, stopwords("en"))
  sentence <- stemDocument(sentence)
  sentence <- stripWhitespace(sentence)
  sentence <- tolower(sentence)
  sentence
}

trimLeadWord <- function(x) {
  tokens <- unlist(str_split(x, " "))
  LENGTH = length(tokens)
  if (LENGTH <= 1) {
    return ("")
  } else {
    return(str_c(tokens[2:LENGTH], collapse = " "))
  }
  
}

wordCount <- function(x){
  v = str_count(x, " ") + 1
  v
}


predictScalar <- function(model, newData) {
  ## trim leading and trailing white spaces
  newData = trimws(newData)
  ## if this is an empty string, return the most frequent unigram
  if (str_count(newData) == 0) {
    return (model$top.unigram)
  } 
  ## else, use to frequency trie to make a prediction
  trie.prob <- model$trie.prob
  trie.word <- model$trie.word
  N <- model$ngram
  n <- length(unlist(str_split(newData, " ")))
  ## Being an N-gram model, the function will stop if the query word count is not between [1, N-1]
  if (n >= N) {
    stop("new data has more words than model can predict")
  } else {
    ## use regular expression to find candidate n+1 gram
    #candidatePosition <- str_detect(get_keys(wordtrie), paste(c("^", newData, " \\w+$"), collapse = ""))
    #candidatesKey <- get_keys(wordtrie)[candidatePosition]
    #candidatesValue <- get_values(wordtrie)[candidatePosition]
    newData = paste0(newData, " ")
    matchedKeys <- unlist(prefix_match(trie.word, newData))
    matchedValues <- unlist(prefix_match(trie.prob, newData))
    ## if we found at least one candidate, we return the most frequent one
    if (length(matchedKeys) >= 1 & !is.na(matchedKeys[1])) {
      #print(candidates)
      FILTER = str_count(matchedKeys, " ") == n
      ## arrange candidates by their frequency
      candidates <- data.frame(ngram = matchedKeys[FILTER], prob = matchedValues[FILTER]) %>% arrange(-prob)
      return (str_split(candidates[1,1], " ")[[1]][n + 1])
    } else { ## otherwise, we remove one word from the n-gram query, and make a recursive call
      return (predictScalar(model, trimLeadWord(newData)))
    }
  }
}

predictNext <- function(model, sentences) {
  predictions <- vector(mode = "character", length = length(sentences))
  for (i in 1:length(sentences)) {
    processed <- processSentence(sentences[i])
    tokens = unlist(str_split(processed, " "))
    sentence_token_count = length(tokens)
    N = model$ngram
    if (sentence_token_count >= N) {
      tokens = tail(tokens, N - 1)
    }
    query = paste(tokens, collapse = " ")
    prediction = predictScalar(model, query)
    predictions[i] = prediction
  }
  predictions
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$pred <- renderText({
    paste("Prediction", predictNext(model, input$query), sep = ": ")
  })
  
})
