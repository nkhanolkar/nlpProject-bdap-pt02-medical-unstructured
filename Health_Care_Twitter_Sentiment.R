# Installing packages
# CRAN
library(twitteR)
library(ROAuth)
library(SnowballC)
library(ggplot2)
library(RColorBrewer)
library(httr)
library(RCurl)
library(XML)
library(devtools)
library(sqldf)
#download the next
library(BiocGenerics)
library(graph)
library(Rgraphviz)
library(Rstem)
library(slam)
library(tm) #CRAN
library(modeltools) #CRAN
library(sentiment)
library(wordcloud)
#Download gsl dependency first
#sudo yum install gsl-devel
library(topicmodels)
library(shiny)
library(sunburstR)


## Twitter authentication

consumer_key <- "iMmcGRLke8MI6re7RF7WjyDuu"
consumer_secret <-"9pKnpqjGd6e1mYM1Jj1lPRqh13aLx48AEJclM7q26XdLcfIvns"
access_token <- "788589864845512704-U4YhnTbjV6Api9bW7XrBPhnUgWzB9q3"
access_secret <- "ZYBvg6LfDqJzlI4jfvhkvYq6FWPyHvDFgWVWvvBwlS9M5"

setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)
## 1795 is the maximum to retrieve
tweets <- searchTwitter('Health+care', n=3000, lang = "en")

tweets.df <- twListToDF(tweets)
(n.tweet <- length(tweets))

writeLines(strwrap(tweets.df$text[190], 60))

library(tm)

myCorpus <- Corpus(VectorSource(tweets.df$text))


myCorpus <- tm_map(myCorpus, PlainTextDocument)

test.dataframe_before<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), stringsAsFactors=F)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
####
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
test.dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), stringsAsFactors=F)

####
myStopwords <- c(setdiff(stopwords('english'), c("a", "n")),"use", "see", "a", "used", "in", "by", "for")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myCorpus <- tm_map(myCorpus, stripWhitespace)
#################################################################################################################

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpusCopy <- myCorpus
test.dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), stringsAsFactors=F)
#Debugged :P

#SnowBall

myCorpus <- tm_map(myCorpus, stemDocument) # stem words
writeLines(strwrap(myCorpus[[190]]$content, 60))

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))
writeLines(strwrap(myCorpus[[190]]$content, 60))

wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}
n.miner <- wordFreq(myCorpusCopy, "service")
n.mining <- wordFreq(myCorpusCopy, "emergency")
cat(n.miner, n.mining)


#replaceWord <- function(corpus, oldword, newword) {
#  tm_map(corpus, content_transformer(gsub),
#         pattern=oldword, replacement=newword)
#}
#myCorpus <- replaceWord(myCorpus, "vehiclee", "vehicle")
#myCorpus <- replaceWord(myCorpus, "universidad", "university")
#myCorpus <- replaceWord(myCorpus, "scienc", "science")

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

idx <- which(dimnames(tdm)$Terms %in% c("cyrus", "tata", "price"))
as.matrix(tdm[idx, 21:30])

(freq.terms <- findFreqTerms(tdm, lowfreq = 100))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 5)
word_freq <- data.frame(term = names(term.freq), freq = term.freq)

#Debugged :P

#ggplot2
ggplot(word_freq, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
#RColorBrewer
pal <- brewer.pal(9, "BuGn")[-(1:4)]

#wordcloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10, random.order = F, colors = pal)

findAssocs(tdm, "price", 0.2)
findAssocs(tdm, "service", 0.2)

plot(tdm, term = freq.terms, corThreshold = 0.4, weighting = T)


dtm <- as.DocumentTermMatrix(tdm)
#topicmodels
lda <- LDA(dtm, k = 10) # find 10 topics
term <- terms(lda, 10) # first 10 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
term.df <- as.data.frame(term)

newtopic_df <- as.data.frame(c(1:10))
names(newtopic_df)[1] <- "topic"
newtopic_df <- cbind(term.df,newtopic_df)

topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.Date(tweets.df$created), topic=topics)
ggplot(topics, aes(date, fill = term[topic])) + geom_density(position = "stack")

#httr
#RCurl
#XML
#devtools
#install_github("okugami79/sentiment140")

#sentiment
#emotions_classified <- classify_emotion(tweets.df, algorithm = "bayes", prior = 1.0, verbose = FALSE)
#emotions_df <- as.data.frame(emotions_classified)
polarity_classified <- classify_polarity(tweets.df$text, algorithm= "bayes", pstrong = 0.5, pweak= 1.0 , prior = 1.0, verbose = FALSE)
polarity_classified_df <- as.data.frame(polarity_classified)
results_polarity <- as.data.frame(polarity_classified_df$BEST_FIT)
tweet_text <- as.data.frame(tweets.df$text)


#VISUAL SUNBURST
combined_result <- cbind(tweet_text,results_polarity)
Visual_DF <- as.data.frame(combined_result)
head(Visual_DF)
names(Visual_DF)[1] <- "Tweet"
names(Visual_DF)[2] <- "Sentiment"
Visual_DF$title <- "HEALTH CARE"
Visual_DF$count <- rep(1, nrow(Visual_DF))

combinedtopic_terms <- sqldf("select * from newtopic_df INNER JOIN topics ON topics.topic = newtopic_df.topic;")
combinedtopic_terms[4] <- NULL
newVisual_DF <- cbind(Visual_DF, combinedtopic_terms$term)
names(newVisual_DF)[5] <- "Topics"

newVisual_DF$path <- paste(newVisual_DF$title, newVisual_DF$Topic, newVisual_DF$Sentiment, sep="-")

yellow <- c("#FFFF00")
blue <- c("#5687d1")
brown <- c("#7b615c")
orange <- c("#de783b")
l_green <- c("#6ab975")
purple <- c("#a173d1")
l_yellow <- c("#FFFFCC")
aqua <- c("#00CC99")
d_aqua <- c("#009999")
occur <- c("#CC6600")
pink <- c("#CC33FF")
red <- c("#FF3300")
green <- c("#339900")
grey <-c("#999999")

#x= list(c9)
x=list(yellow, blue , orange , red , grey , green , aqua , d_aqua , occur , purple , l_green , brown , l_yellow , pink)
typeof(x)
head(x)
sunburst(data.frame(xtabs(count~path,newVisual_DF)), colors= x, count = TRUE)

#PIECHART
#slices <- newpie$value
#lbls <- newpie$group
#pct <- round(slices/sum(slices)*100)
#lbls <- paste(lbls, slices) # add percents to labels
#lbls <- paste(lbls,sep="") # ad % to labels
#pie(slices,labels = lbls, col=rainbow(length(lbls)),
#    main="Polarity Of Sentiments ")