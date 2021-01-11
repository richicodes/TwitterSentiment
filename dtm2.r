#creates DTM

library(tm)
library(SnowballC)
library(wordcloud)
library(textstem)

dtmexp <- function(input) {
  class(input)
  posemojilist <- read.delim("posemojis.txt")
  negemojilist <- read.delim("negemojis.txt")
  
  corpus <- Corpus(VectorSource(input))
  corpus <- tm_map(corpus,content_transformer(tolower))
  
  for (i in 1:nrow(posemojilist)){
    emoji<- paste("&#",posemojilist[i,1], sep="")
    #View(emoji)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = emoji, replacement = " happy ")
  }  
  for (i in 1:nrow(negemojilist)){
    emoji<- paste("&#",negemojilist[i,1], sep="")
    #View(emoji)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = emoji, replacement = " sad ")
  }  
  
 # for (i in 1:nrow(negative)){
#    negrep<- as.character(negative[i,1])  
#    corpus <- tm_map(corpus, content_transformer(gsub), pattern = negrep, replacement = " sad ")
 # }


  
  #replace those emoticons >:(
  
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\(:|:\\)|:-\\)|\\(-:|&lt;3| :D|^_^|;\\)|\\(;|:]|=D", replacement = " happy ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\):|:\\(|:-\\(|\\)-:|-_-|._.| :/|-.-", replacement = " sad ")
  
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "&#x25", replacement = " percent ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "&#xB0", replacement = " degree ")
  #remove le words
  negativelist <- c("isn't", "aren't", "wasn't", "weren't", "hasn't",
                    "haven't", "hadn't", "doesn't", "don't", "didn't",
                    "won't", "wouldn't", "shan't", "shouldn't", "can't",
                    "cannot", "couldn't", "mustn't", "not", "no")
  # negativelist <- c("not", "no")
  removelist <- c(stopwords("english"), "weather", "today", "day", "mention")
  removelist <- setdiff(removelist, negativelist)
  
  #time to gently substitute punctuation first! these lines replace functions with a space instead.
  # toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
  # corpus <- tm_map(corpus, toSpace, "-")
  # corpus <- tm_map(corpus, toSpace, ":")
  # corpus <- tm_map(corpus, toSpace, "'")
  # corpus <- tm_map(corpus, toSpace, "'")
  # corpus <- tm_map(corpus, toSpace, " -")
  
  
  corpus <- tm_map(corpus,removeWords, removelist)
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,lemmatize_strings)
  corpus <- tm_map(corpus,stemDocument)
  corpus <- tm_map(corpus,stripWhitespace)
  
  dtm <- DocumentTermMatrix(corpus)
  dtm <- removeSparseTerms(dtm,0.995)
  dtmsparse <- as.data.frame(as.matrix(dtm))
  colnames(dtmsparse) <- make.names(colnames(dtmsparse))
  
  return(dtmsparse)
}



dtmtfidfexp <- function(input) {
  corpus <- Corpus(VectorSource(input))
  
  corpus <- tm_map(corpus,content_transformer(tolower))
  
  #replace those emoticons >:(
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\(:|:\\)|:-\\)|\\(-:", replacement = " happy ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\):|:\\(|:-\\(|\\)-:", replacement = " sad ")
  
  #remove le words
  negativelist <- c("isn't", "aren't", "wasn't", "weren't", "hasn't",
                    "haven't", "hadn't", "doesn't", "don't", "didn't",
                    "won't", "wouldn't", "shan't", "shouldn't", "can't",
                    "cannot", "couldn't", "mustn't", "not", "no")
  # negativelist <- c("not", "no")
  removelist <- c(stopwords("english"), "weather")
  removelist <- setdiff(removelist, negativelist)
  
  corpus <- tm_map(corpus,removeWords, removelist)
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,stemDocument)
  corpus <- tm_map(corpus,stripWhitespace)
  
  review_dtm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
  dtmsparse <- as.data.frame(as.matrix(dtm))
  colnames(dtmsparse) <- make.names(colnames(dtmsparse))
  
  return(dtmsparse)
}





#WOW WORD CLOUD
wordcloudexp <- function(dtm) {
  word_freqs  <-  sort(colSums(dtm), decreasing=TRUE)
  dm  <-  data.frame(word=names(word_freqs), freq=unname(word_freqs))
  # Plot wordcloud
  wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  return(dm)
}