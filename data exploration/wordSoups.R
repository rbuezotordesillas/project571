
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

filePath <- "../data preparation/Texts/Fashion.txt"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove spanish common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 50)

set.seed(1234)
png("Fashion.png", width=12,height=8, units='in', res=300)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
