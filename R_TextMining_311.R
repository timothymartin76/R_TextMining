getwd()
setwd("C:/March_Words")

# Load packages
require("SnowballC")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("slam")

# Read the text file
text <- readLines("March_Words2.txt")

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

# Text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Clean up the text
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
dtm2 <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)
m <- as.matrix(dtm2)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate the word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
max.words=200, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8, "Dark2"))

# Generate barplot of word frequencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
col ="steelblue", main ="Words Most Frequently Used March 2015",
ylab = "Word frequencies")
