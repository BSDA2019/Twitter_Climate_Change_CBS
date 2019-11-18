# prefer is All_variables_Extract
prefer <- All_variables_Extract
rm(All_variables_Extract)
#prefer$text <- gsub("Kim Jong Un|Kim-Jong Un|kimjungun|KimJongUn","kimjongun",prefer$text)


library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(prefer$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#Remove Numbers 
myCorpus <- tm_map(myCorpus,content_transformer(removeNumbers))

#Remove Punctuations
myCorpus <- tm_map(myCorpus, removePunctuation)


# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later

myCorpus <- tm_map(myCorpus, stemDocument) # stem words 

#Removing the stop words like our, me, i, ourselves  etc. 
new_stops <- c("rt","climate","climat","chang","change","global","warm","http","climatechang",stopwords("en"))
myCorpus <- tm_map(myCorpus, removeWords,new_stops)

myCorpusCopy <- myCorpus



#Build Term Document Matrix

tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
#sparse
#sparse <- removesparseTerms(sparse, 0.97)
#sparse
#Now you are removing those terms which don't appear too often in your data. We will remove any element that doesn't appear in atleast 3% of the entries (or documents). Relating to the above created DTM we are basically removing those columns whose entries are 1 in least number of documents.
#The output signifies that DTM has 4801 entries which has over 9523 terms which have appeared at least once.
#This reads like 45640932 cells in sparse are 0, 78991 have non-zero values. 100% of all cells are zero (which is 45640932/(45640932+78991))


# TOP frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 200000))
sparse <- removeSparseTerms(tdm, 0.99)
term.freq <- rowSums(as.matrix(sparse),)
term.freq <-sort(term.freq,decreasing = T)
term.freq <- subset(term.freq, term.freq >= 200000)
df <- data.frame(term = names(term.freq), freq = term.freq)
#1th plot
library(ggplot2)
# ggplot(df, aes(x=reorder(term,freq), y=freq)) + geom_bar(stat = 'identity', fill = "#00aced", alpha = 0.7) +
#      xlab("Terms") + ylab("Count") + ggtitle("Most Used Words") +
#      coord_flip() + theme_bw() + 
#      theme(axis.text = element_text(size = 12, family = "Verdana", color = "#333333"),
#                     axis.title = element_text(size = 11.5, family = "Verdana", face = "bold"),
#                      plot.title = element_text(size = 14, family = "Verdana", face = "bold"))
#                                                                        

#WORDCLOUD
m <- as.matrix(sparse)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)

# plot word cloud
library(wordcloud)
# wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10000,
#           color=brewer.pal(8,"Set2"),random.order = F)


#Associations


# # which words are associated with 'warm'?
# findAssocs(sparse, "", 0.8)
# 
# # which words are associated with 'data'?
# findAssocs(sparse, "change", 0.8)
# findAssocs(sparse, "climate", 0.8)
# findAssocs(sparse, "trump", 0.2)
# 

##################################
#######################
################
############
#######
####
##
#

#Network of Terms

# library(graph)
# library(Rgraphviz)
# plot(sparse, term = freq.terms, corThreshold = 0.8, weighting = T)
# 

#topic modelling     here we put the tdm matrix. let-see if it works

dtm <- as.DocumentTermMatrix(tdm)
######################
######################
######################
######################
#Remove empty documents from DocumentTermMatrix in R topicmodels?

#Consider a corpus of documents and a dictionary 
#of terms contain all the words that appear in the
# documents. The document-term matrix then is a two-dimensional
# matrix whose rows are the documents and columns are the terms, 
# so each entry (i, j) represents the frequency of term i in 
# document j.

# The error means that sparse matrix contain a row without
# entries(words). one Idea is to compute the sum of words by row

# rowTotals <- apply(text_dtm , 1, sum) #Find the sum of words in each Document
# text_dtm.new   <- text_dtm[rowTotals> 0, ]           #remove all docs without words


########
# BETTER SOLUTION

# A document-term-matrix created by the tm package contains the 
# names i and j , which are indices for where entries are in the
# sparse matrix. If dtm$i does not contain a particular row
# index p, then row p is empty.
# library(tictoc)
# 
# 
# tic()
# ui <- unique(dtm$i)
# #ui is just a vector
# dtm.new <- dtm[ui,]
# toc()


#0.03 sec elapsed
# ui contains all the non-zero indices, and since dtm$i 
# is already ordered, dtm.new will be in the same order as dtm.
# The performance gain may not matter for smaller 
# document term matrices, but may become significant with
# larger matrices.

# library(topicmodels)
# # set a seed so that the output of the model is predictable
# lda <- LDA(dtm.new, k = 4, control = list(seed=1234)) # find 10 topics
# #lda
# #inspect(dtm.new)
# #install.packages("tidytext")
# library(tidytext)
# 
# topics <- tidy(lda, matrix = "beta")
#topics
#For each combination, the model computes the probability of that term being generated from that topic.

# When we fit a topic model, the result is an LDA model object. 
#It contains two matricies: beta and gamma
#beta contains probabilites of words in topics

#----------------------------------
#We could use dplyr's top_n() to find the 10 terms that are most
#common within each topic. As a tidy data frame, this lends itself well to a ggplot2 visualization
# library(ggplot2)
# library(dplyr)
# 
# top_terms <- topics %>%
#   group_by(topic) %>%
#   top_n(15, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# top_terms %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip() +
#   scale_x_reordered()





#################
###############
##########
# term <- terms(lda, 10) # first 7 terms of every topic
# (term <- apply(term, MARGIN = 2, paste, collapse = ", "))
# library(data.table)
# topics <- topics(lda) # 1st topic identified for every document (tweet)
# topics <- data.frame(date=(prefer$created_at), topic=topics)
# ggplot(topics, aes(date, fill = term[topic])) +
#   geom_density(position = "stack")

##################################
#######################
################
############
#######
####
##
#

#SENTIMENT ANALYSIS 

# install.packages("sentiment140")
require(devtools)
# install_github("sentiment140", "okugami79")
# sentiment analysis
library(sentimentr)   #WE ARE USING SENTIMENTR PACKAGE 
#removing alphanumeric words
prefer$text <- gsub("[^0-9A-Za-z///' ]","",prefer$text)
#removing http link 
prefer$text <- gsub("http\\w+", "",prefer$text)
#removing rt
prefer$text <- gsub("rt","",prefer$text)
#removing at
prefer$text <- gsub("@\\w+","",prefer$text)
# setting lower case
prefer$text <- tolower(prefer$text)

#removing empty rows
which(prefer$text=="")
length( which(prefer$text=="")) #length = 852 rows

prefer[which(!prefer$text==""),]
View(prefer[which(!prefer$text==""),])
#The key idea is you form a set of the rows you want to remove, and keep the complement of that set.

prefer<-prefer[which(!prefer$text==""),]  # YES!!!
#########################
#########################
#########################
#########################

#Computing the sentiments
sentiments <- sentiment(prefer$text)  
View(sentiments)
table(sentiments$sentiment)
View(prefer)

##
## neutral positive
## 428 20
# sentiment plot
sentiments$polarity <- "neutral"
sentiments$polarity[sentiments$sentiment>=1] <- "strongly positive"
sentiments$polarity [sentiments$sentiment>=0.5 & sentiments$sentiment<1] <- "positive"
sentiments$polarity [sentiments$sentiment>=0.25 & sentiments$sentiment<0.5] <- "weakly positive"
sentiments$polarity [sentiments$sentiment<=-0.25 & sentiments$sentiment>-0.5] <-"weakly negative"
sentiments$polarity [sentiments$sentiment<=-0.5 & sentiments$sentiment>-1] <-"negative"
sentiments$polarity [sentiments$sentiment<=-1] <-"strongly negative"
# ggplot(df, aes(x=reorder(term,freq), y=freq))

prefer$sentiments <- sentiments$polarity 


View(sentiments)

###################################
###############################
###########################
####################
term.freq2 <-rowSums(as.matrix(table(sentiments$polarity)))
df2 <- data.frame(Sentiment = names(term.freq2), freq2 = term.freq2)

library(ggplot2)
ggplot(df2, aes(x=reorder(Sentiment,freq2),    y=freq2)) + geom_bar(stat="identity",alpha=0.3,aes(color=Sentiment, fill=Sentiment)) +
  xlab("Sentiment") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

#View(prefer)
##################################
#######################
################
############
#######
####
##
#
#Top Retweeted Tweets

# # select top retweeted tweets
# table(prefer$retweet_count)
# selected <- which(prefer$retweet_count >= 50000)
# # plot them
# dates <- strptime(prefer$created_at, format="%d/%m/%Y %H:%M:%S")
# plot(x=dates, y=prefer$retweet_count, type="l", col="grey",
#      xlab="Date", ylab="Times retweeted")
# colors <- rainbow(10)[1:length(selected)]
# points(dates[selected], prefer$retweet_count[selected],
#        pch=19, col=colors)
# text(dates[selected], prefer$retweet_count[selected],
#      prefer$text[selected], col=colors, cex=.9)


##################################################################################################
####################################################
###############################################
########################################
#SIMPLIFIED VERSION FOR WORDCLOUD

sentiments$sim.pol <- "neutral"
sentiments$sim.pol [sentiments$sentiment>=0.25] <- "positive"
sentiments$sim.pol [sentiments$sentiment<=-0.25]  <-"negative"


prefer$sim.pol <- sentiments$sim.pol 







##################################################################################################
####################################################
###############################################
########################################



setwd("C:\\Users\\Administrator\\Desktop\\TWITTER SENTIMENT")


#SENTIMENT ANALYSIS WITH POSITIVE - NEGATIVE - NEUTRAL DATA
library(wordcloud)
###########################################################
positive_tweets <- prefer[prefer$sim.pol=="positive",c(17,36)]
#positive_tweets 

write.table(positive_tweets$text,file = "positive.txt",sep = "",na="NA")

####
####

neutral_tweets <- prefer[prefer$sim.pol=="neutral",c(17,36)]
neutral_tweets 

write.table(neutral_tweets$text,file = "neutral.txt",sep = "",na="NA")

####
####

negative_tweets <- prefer[prefer$sim.pol=="negative",c(17,36)]
negative_tweets 

write.table(negative_tweets$text,file = "negative.txt",sep = "",na="NA")

pos_neutr_neg_tweets <- c(positive_tweets$text, neutral_tweets$text,negative_tweets$text)
####
####
library(tm)

tweet_corpus <- Corpus(DirSource(directory=getwd()))
summary(tweet_corpus)

#####
clean_tweet_corpus <- tm_map(tweet_corpus, content_transformer(tolower))
clean_tweet_corpus <- tm_map(clean_tweet_corpus,content_transformer(removeNumbers))
clean_tweet_corpus <- tm_map(clean_tweet_corpus, removePunctuation)
clean_tweet_corpus <- tm_map(clean_tweet_corpus, removeWords,stopwords("en"))
clean_tweet_corpus <- tm_map(clean_tweet_corpus, stripWhitespace)

new_stops <- c("rt","climate","climat","chang","change","global","warm","http","climatechang",stopwords("en"))
clean_tweet_corpus <- tm_map(clean_tweet_corpus, removeWords,new_stops)

clean_tweet_corpus  <- tm_map(clean_tweet_corpus, stemDocument)

ctc_sparse <- TermDocumentMatrix(clean_tweet_corpus)
#typeof(ctc_sparse)
#View(ctc_sparse)
ctc_matrix <- as.matrix(ctc_sparse)
#head(ctc_matrix)
#View(ctc_matrix)
colnames(ctc_matrix) <- c("Negative","Neutral","Positive")
comparison.cloud(ctc_matrix,max.words = 300,random.order = F)


prefer$score  <-sentiments$sentiment

sentiment_analysis <- data.frame(prefer$text,prefer$created_at,prefer$sentiments,prefer$score)
View(sentiment_analysis)
write.csv(sentiment_analysis,"C:\\Users\\Administrator\\Desktop\\CLIMATECHANGEONLYFORSENTIMENTANALYSIS.csv")


