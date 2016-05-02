setwd("D:Oht/R/Twitter API")
library("twitteR")
library("wordcloud")
library("tm")
require (twitteR)
require (RCurl)
library(stringr)
library(ggplot2)
library(reshape2)
library(reshape)
library(stringr)
library(data.table)
library(qdapRegex)
library (sqldf)
library(plyr)

consumer_key <- 'xxxxxxx'
consumer_secret <- 'xxxxxxx'
access_token <- 'xxxxxxx'
access_secret <- 'xxxxxxx'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# download tweets to a variable
tgif <- searchTwitter("#FridayReads", n=5000, since='2016-04-08')


df <- do.call("rbind", lapply(tgif, as.data.frame))

df2 <- as.data.frame(df$text)
df3 <- df2[grep(" [Bb][Yy] ", df2[,1]),]
df3 <- as.data.frame(df3)
df4 <- df3
colnames(df3) <- c('books')
df3$books <- as.character(df3$books)
df3$books <- gsub("^RT @.*:\\s+", "", df3$books)
df3$books <- gsub("https://.*?$", "", df3$books)

df3$books <- gsub("#(.*?):", "", df3$books)
df3$books <- gsub("#(.*?) ", "", df3$books)

df3$book_name <- gsub("by.*", "\\1", df3$books)
df3$author <- gsub(".*by", "\\1", df3$books)
df3$author <- gsub("#.*", "", df3$author)
df3$author <- as.character(df3$author)
df3$author <- gsub("@", "", df3$author)
df3$book_name <- gsub("#.*", "", df3$book_name)

#n <-gsub("^(\\w+\\s\\w+).*$", "\\1", df3$author)
#df3$au <- gsub("((\\w+\\W+){0,9}\\w+).*", "\\1", df3$author)
df3$author <- gsub("((\\w+\\W+){1}\\w+).*$", "\\1", df3$author)  #wont work with "^"

df3$book_name <- gsub("&amp;", "", df3$book_name)
df3$author <- gsub("&amp", "", df3$author)

df3$book_name <- gsub("@the_author_", "", df3$book_name)
df3$book_name <- gsub("@(\\w+|\\W+)\\s", "", df3$book_name)

#df3$book_name <- gsub("<.*", "", df3$book_name) #won't work as these are non-english characters
df3$book_name <- gsub("\\[.*?\\]\\s", "", df3$book_name) #remove '[video]' 

df3 <- df3[!grepl("RT", df3$book_name),] #delete eows with 'RT'
#df3 [df3 ==""] <- NA #fill empty cells with NA

# special case cleaning
df3$author <- gsub("P.S", "P.S. Winn", df3$author)
df3$author <- gsub("M.R", "M.R. Carey", df3$author)
df3$author <- gsub("A.J", "A.J. Jacobs", df3$author)
df3$author <- gsub("A.J", "A.J. Jacobs", df3$author)
df3$author <- gsub(".*lorenahathaway.*", "Loren Hathaway", df3$author)
df3$author <- gsub(".*TieryasXu.*", "Peter Tieryas", df3$author)
df3$author <- gsub(".*M.R. Carey*", "T.E. AVERY", df3$author)
df3$author <- gsub(".*arianahuff*", "Arianna Huffington", df3$author)
df3$book_name <- gsub(".*Thrive*", "Thrive", df3$book_name)
df3$book_name <- gsub(".*MURDER BY PLANE By T.E. AVERY,*", "MURDER BY PLANE", df3$book_name)
df3$author <- gsub(".*Cynthia*", "Cynthia Sweeney", df3$author)
df3$book_name <- gsub("My is ", "", df3$book_name)
df3$book_name <- gsub("is ", "", df3$book_name)
# # special case cleaning
df3 <- df3[!grepl("AlysWestYork", df3$author),] #delete rows with 'AlysWestYork'
df3 <- df3[!grepl("tweeting to", df3$author),] #delete rows with 'tweeting to'
df3 <- df3[!grepl("My -)", df3$author),] #delete rows with 'My)'
df3 <- df3[!grepl(".*http..*", df3$author),] #delete rows with 'http.'
df3 <- df3[!grepl("In honor", df3$author),] #delete rows with 'In honor'
df3 <- df3[!grepl(".*Empowering the Status of Women..*", df3$books),] #delete rows 
df3 <- df3[!grepl(".*xslyang.*", df3$author),] #delete rows 
df3 <- df3[!grepl(".*Sartre.*", df3$author),] #delete rows 

#extract string between quotes
df3$book_name <- gsub('.*"([^"]*)"', "\\1", df3$book_name) #  .* in begining to not leave that text untouched
df3$book_name <- gsub(".*'([^']*)'", "\\1", df3$book_name) #single quotes not working

df3$book_name <- gsub(".*Valley.*", "Mystic Valley", df3$book_name)


df3$b6 <- gsub("[(A-Z\\s)]+\\s", "\\1", df3$book_name)
df3$b6 <- gsub("[A-Z ]+\\s", "\\1", df3$book_name) #removes all Uppercase words

# delete rows not needed
for (i in 1:10){                        #extra loop to re-run for blank removal
  for (i in 1:nrow(df3)){
    if(nchar(df3$books[i]) < 5){
      df3 = df3[-i,]
}}}
for (i in 1:10){                       #extra loop to re-run for NA removal
  for (i in 1:nrow(df3)){
    if (is.na(df3$book_name[i])){
      df3 = df3[-i,]
}}}


# plot authors
table_author <- table(df3$author)
table_author <- subset(table_author, table_author >4)
op <- par(mar = c(4,15,4,2) + 0.1)
barplot(table_author, las = 2, horiz=TRUE,xlab = 'Trending authors (frequency of 5000 tweets)')
par(op)

#plot books
table_book <- table(df3$book_name)
table_book <- subset(table_book, table_book >4)
op <- par(mar = c(4,15,4,2) + 0.1)
barplot(table_book, las = 2,  horiz=TRUE,xlab = 'Top books read (frequency of 5000 tweets)')
par(op)

# cleaning function
cleanFunc <- function(datax){
  datax_clean <- tm_map(datax, removePunctuation)
  datax_clean <- tm_map(datax_clean, content_transformer(tolower))
  datax_clean <- tm_map(datax_clean, removeWords, stopwords("english"))
  datax_clean <- tm_map(datax_clean, removeNumbers)
  datax_clean <- tm_map(datax_clean, stripWhitespace)
  datax_clean <- tm_map(datax_clean, removeWords, c("fridayreads"))
  return(datax_clean)
}

#book cloud
book_name_corpus <- Corpus(VectorSource(df3$book_name))
book_clean <- cleanFunc(book_clean)
par(mar = rep(2, 4))
wordcloud(book_clean)

#author cloud
author_corpus <- Corpus(VectorSource(df3$author))
author_clean <- cleanFunc(author_clean)
wordcloud(author_clean)


# timeline
tweet_time <- as.data.frame(df$created)
colnames(tweet_time) <- c('daytime')
tweet_time$daytime <- strptime(tweet_time$daytime, format="%Y-%m-%d %H:%M:%S")
tweet_time$timee <- format(tweet_time$daytime, "%H:%M:%S")

#plot time
table_twtime <- table(tweet_time$timee)
table_twtime <- subset(table_twtime, table_twtime >3)
op <- par(mar = c(10,4,4,2) + 0.1)
barplot(table_twtime, typ='l', las = 2, ylab="Number of Tweets")
par(op)

#
#
#
#
hashtag <- as.data.frame(df[,1])
colnames(hashtag) <- c('raw')
hashtag$raw <- as.character(hashtag$raw)
hashtag$tags <- gsub(".*(#\\S+)", "\\1", hashtag[,1])
hashtag$tags <- gsub("https://.*?$", "", hashtag$tags)
hashtag$tags <- gsub(("\\s.*"), "\\1", hashtag$tags)
hashtag$tags <- gsub(".*riday.*", "#FridayReads", hashtag$tags)

#plot hashtags
table_hashtag <- table(hashtag$tags)
table_hashtag <- subset(table_hashtag, table_hashtag >50)
op <- par(mar = c(4,15,4,2) + 0.1)
barplot(table_hashtag, typ ='l', las = 2, horiz=TRUE,xlab = 'Top hashtags (frequency of 5000 tweets)')
par(op)

#
#
#
#raw cloud
df$text <- gsub("https://.*?$", "", df$text)
raw_corpus <- Corpus(VectorSource(df$text))
raw_clean <- cleanFunc(raw_corpus)
wordcloud(raw_clean,min.freq=50)


#
#
#
# screen name retweet
retwscr <- data.frame()
retscr <- vector()
rettw <- vector()
for(i in (1:nrow(df))){
  if (df$retweetCount[i] > 350){ #change this number if needed
    retscr <- c(retscr, df$screenName[i])
    rettw <- c(rettw, df$retweetCount[i])
  }
}
retwscr <- melt(data.frame(retscr, rettw))
retwscr = retwscr[,-2]
#plot retweets
ggplot(retwscr)+
  aes(x=retscr, y = rettw)+
  geom_point(size = 5, alpha = 0.5,aes(color=rettw, wsize = rettw))+
  xlab ("Screen name") +
  ylab ("Number of retweets")+
  ggtitle ("Top #FridayReads retweeters \nDate: 2016-04-15, Time: 15:16:59 to 23:53:10")+
  theme(axis.text.x = element_text(size = 12, angle=0, vjust=1, hjust = 1),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

#
#
#
# location for geogrphical distribution
"""
getLocation<-function(x){
  y <- getUser(x)
  z <- location(y)
  return(z)
}

loc <- vector();
for (i in (1:500)){
loc <- c(loc, getLocation(df$screenName[i]))
}
"""

