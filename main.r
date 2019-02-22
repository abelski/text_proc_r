# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("mongolite")


cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}


m <-  mongo("tickets", url = "mongodb://localhost:27017/agile")
m <- m$find("{}","{}",limit=10)$description
m <- cleanFun(paste(m,collapse=" "))

docs <- Corpus(VectorSource(m))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("wwwepamcom", "epamcomgt","dear","epamcomgt","epamcom"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

# inspect(docs)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

d <- head(d, 20)

print(d)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
max.words=200, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8, "Dark2"))
