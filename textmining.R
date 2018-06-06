library(XML)
library(RCurl)



library(tm)
library(tmcn)

library(Rwordseg)
getwd()
setwd("/Users/arance/Documents")
d.corpus <- Corpus(DirSource("pilot"), list(language = NA))

d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

inspect(d.corpus)




words <- c("??‡ç??","å¼ çˆ¸","??‘å…¬","??‰è??","?—ª???","??‘å¡","?›¾???",
           "?›¾?’§",
           "??„å›¾",
           "??‘æ?‡ä?é?„å›¾",
           "æ²¡å›¾æ²¡ç?Ÿç›¸",
           "?‰¿è®¤ä? å°±?˜¯",
           "?•²ç¢?",
           "è®©ä?“ä?šç?„æ¥",
           "äº”æ¥¼ä¸“ä??",
           "æ¥¼ä?“ä??",
           "æ¥¼æ­£è§?",
           "æ¥¼ä¸­?‚¯",
           "æ¥¼ä?æŽ¨ä¸è??")
words <- toTrad(words)
insertWords(words)

for(i in 1:380){d.corpus1[i] <- tm_map(d.corpus[i], segmentCN, nature = TRUE)}
d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
d.corpus1 <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) == "n"]
  })
  unlist(noun)
})
d.corpus1 <- Corpus(VectorSource(d.corpus1))
myStopWords <- c(stopwordsCN(), "ç·¨è¼¯", "??‚é??", "æ¨™é??", "?™¼ä¿?", "å¯¦æ¥­", "ä½œè€?")
d.corpus1 <- tm_map(d.corpus1, removeWords, myStopWords)
head(myStopWords, 20)

tdm <- TermDocumentMatrix(d.corpus1, control = list(wordLengths = c(2, Inf)))
inspect(tdm[1:20, 1:10])


library(wordcloud)

m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
par(family=("Heiti TC Light"))
wordcloud(d$word, d$freq, min.freq = 10, random.order = F, ordered.colors = F, 
          colors = rainbow(length(row.names(m1))))


d.dtm <- DocumentTermMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
inspect(d.dtm[1:10, 1:2])

findFreqTerms(d.dtm, 30)

findAssocs(d.dtm, "å°è¡¨å¦¹å? æ²¹", 0.5)