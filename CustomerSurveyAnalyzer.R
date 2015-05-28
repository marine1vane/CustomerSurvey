#This is Marine1Vane Customer Sentiment Analyzer for OpenSource development of apps
#It falls under the GULA
#    SSSSS  EEEEE N     N TTTTTTT  III M     M E
#   S       E     NN    N    T      I  MM   MM E
#   S       E     N N   N    T      I  M M M M E
#    SSSS   EEEE  N  N  N    T      I  M  M  M E
#        S  E     N   N N    T      I  M  M  M E
#   S    S  E     N    NN    T      I  M  M  M E
#    SSSS   EEEEE N     N    T     III M  M  M E
#----------------------------------------------

#Pull Text
getwd()
setwd('C:/Users/Duke/Documents/R')

#is not working
require(tm)
require(wordcloud)
require(RColorBrewer)
require(XML)
u = "practice.txt"
texty <- read.table(u, fileEncoding = "UTF-8")
t = readHTMLTable(u)[[1]]

corpusie <- Corpus(DirSource("series"))

#Works to read in the document
fileName="practice.txt"
conn=file(fileName,open="r")
linn=readLines(conn)
for (i in 1:length(linn)){
  print(linn[i])
}
close(conn)

data.frame(linn)
ap.corpus <- Corpus(DataframeSource(data.frame(linn)))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
#ap.tdm <- as.TermDocumentMatrix(corpusie)
ap.tdm <- as.DocumentTermMatrix(corpusie)

corpusie <- Corpus(DirSource("series"))
corpusie <- tm_map(corpusie, removePunctuation)
corpusie <- tm_map(corpusie, tolower)
corpusie <- tm_map(corpusie, function(x) removeWords(x, stopwords("english")))
#ap.tdm <- as.TermDocumentMatrix(corpusie)
corpusie <- tm_map(corpusie, PlainTextDocument)
ap.tdm <- TermDocumentMatrix(corpusie, control = list(removePunctuation = TRUE, stopwords = TRUE))


#Count Text

ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)


#Bounce Text off of keyword lists

if (VERBOSE)
  print("Loading Hu & Liu opinion lexicon")

dataDir<- 'C:/Users/Duke/Documents/R'
#Positive Words
hu.liu.pos = scan(file.path(dataDir, 'opinion-lexicon-English', 'positive-words.txt'), what='character', comment.char=';')
#Negative Words
hu.liu.neg = scan(file.path(dataDir, 'opinion-lexicon-English', 'negative-words.txt'), what='character', comment.char=';')

# add a few twitter and industry favorites
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')

ap.scores = score.sentiment(linn, pos.words, neg.words, .progress='text')

head(ap.scores)

ap.scores$very.pos.bool = ap.scores$score >= 2
ap.scores$very.neg.bool = ap.scores$score <= -2

ap.scores$very.pos = as.numeric( ap.scores$very.pos.bool )
ap.scores$very.neg = as.numeric( ap.scores$very.neg.bool )

print(ap.scores$score)

a<-hist(ap.scores$score)
print(a)
#geom_histogram(NULL,ap.scores$score,"bin","stack")
#qplot(rating, data=ap.scores, weight=score, geom="histogram", binwidth=1)

#Return visualization of text
#WordCloud
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
#Customer Survey Scoring
if (VERBOSE)
  print("Plotting score distributions")

# ggplot works on data.frames, always
#g.hist = ggplot(data=ap.scores, mapping=aes(x=score, fill=CustomerSentiment) )
g.hist = ggplot(data=ap.scores, mapping=aes(x=score) )
# add a bar graph layer. Let it bin the data and compute frequencies
# (set binwidth=1 since scores are integers)
g.hist = g.hist + geom_bar( binwidth=1 )

# make a separate plot for each airline
g.hist = g.hist + facet_grid(airline~.)

# plain display, nice colors
g.hist = g.hist + theme_bw() + scale_fill_brewer() 

print(g.hist)
ggsave(file.path(outputDir, 'twitter_score_histograms.pdf'), g.hist, width=6, height=5.5)





