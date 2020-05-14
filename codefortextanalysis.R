library("rvest")
library("plyr")
library("dplyr")
library("xml2")
library("tidyr")
library("ggplot2")
library("data.table")
library("Rserve")
library("rlang")
library("tibble")
library("tidytext")
library("tidyverse")
library("textdata")
library("scales")
library("janeaustenr")
library("reshape2")
library("wordcloud")
library("RColorBrewer")

# This loop scrapes all the information for mock drafts from 2009-2020 on NBAdraft.net

mockdraft<-data.frame()

for(j in 2009:2020){

  url <-paste0("https://www.nbadraft.net/nba-mock-drafts/?year-mock=",j,"", sep="")
  url <-read_html(url)
  
  yearmockdraft<- data.frame(url %>% html_nodes(xpath='//*[@id="nba_mock_consensus_table"]') %>% html_table())
  yearmockdraft$year<-paste(j)
  yearmockdraft2<- data.frame(url %>% html_nodes(xpath='//*[@id="nba_mock_consensus_table2"]') %>% html_table())
  yearmockdraft2$year<-paste(j)
  mockdraft<- rbind(mockdraft,yearmockdraft, yearmockdraft2)
}

#this puts a dash between players first and last name
mockdraft$Player <- gsub(" ", "-", mockdraft$Player)

#This scrapes all the names from the first and second round of mock drafts from 2010-2020
names<-data.frame()
names2<-data.frame()
for(j in 2010:2020){
  for (i in 30) {
  
  url <-paste0("https://www.nbadraft.net/nba-mock-drafts/?year-mock=",j,"", sep="")
  url <-read_html(url)
  
  nameyear<- data.frame(url %>% html_nodes(xpath='//*[@id="nba_mock_consensus_table"]/tbody/tr["i"]/td[3]') %>% html_text())
  nameyear2<- data.frame(url %>% html_nodes(xpath='//*[@id="nba_mock_consensus_table2"]/tbody/tr["i"]/td[3]') %>% html_text())
  names<- rbind(names,nameyear)
  names2<-rbind(names2,nameyear2)
  }
}

names(names)[1]<-"name"
names(names2)[1]<-"name"

FinalNames<-rbind(names,names2)

#I put a dash in between all players first and last name becuase this is how the analysis portion of the website handles names
FinalNames$name <- gsub(" ", "-", FinalNames$name)

#I then cleaned the names by taking out any extra spaces 
FinalNames$name <- gsub("(^\\s+)|(\\s+$)", "", FinalNames$name)
FinalNames$name<- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", FinalNames$name, perl=TRUE)

#This scrapes  the analysis section for each player. I used a tryCatch function in case a URL did not work
analysis<-data.frame()
for(j in FinalNames$name){
  tryCatch({
  url <-paste0("https://www.nbadraft.net/players/",j,"/", sep="")
  url <-read_html( url)
  
  strengths<- data.frame(url %>% html_nodes(xpath='//*[@id="analysis"]/div[2]/div') %>% html_text ())
  strengths$player<-paste(j)
  analysis<- rbind(analysis,strengths)
  },error = function(e){NA})
}


names(analysis)[2] <- "Player"

#merge the analysis will all player info from the draft (e.i. There height and weight)
text_df<- merge(analysis,mockdraft, by = "Player")
names(text_df)[2]<- "text"


text_df$text <- as.character(text_df$text) 

#count how many times the text was over 200. Because some players were missing analysis
countwords<- sum((nchar(text_df$text)>200))

#create a column that is the word countcout for each players analysis
names(text_df)[c(2,3)]<-c("word","linenumber")
text_df$wordocunt<- nchar(text_df$word)

#make every single word into its own observation for each player
text_df <- text_df %>%
  ungroup() %>%
  unnest_tokens(text, word)

#this is the stop text data(filler words to be deleted)
data(stop_words)
stop_words

#delete all stop words from the text analysis
text_df<- anti_join(text_df,stop_words, by=c("text"="word"))




############ 
#ggplot of all words used over 50 times
text_df %>%
  count(text, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(text, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#######
#sentiment
#show all the words from the NRC lexicon that are in the "joy" category
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
names(nrc_joy)[1]<- "text"

#show all the words from the bing lexicon that are in the "positive" category
bing <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

#show which words from the text are "joy"
text_df %>%
  inner_join(nrc_joy) %>%
  count(text, sort = TRUE)

names(text_df)[11]<- "word"

#this shows the sentiment of the aggregated 60 draft positions. linenumber = draft picl
senttext_df <- text_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(index = linenumber %/% 1, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#plot the aggregated darft picks
ggplot(senttext_df, aes(index, sentiment)) +
  geom_col(show.legend = FALSE,color='red')

#average amout of words for each draft pick
wordcountfix<- aggregate(text_df[, 10], list(text_df$linenumber), mean)

#create some rate sentiment statisitcs
namesandsent<- merge(text_df,senttext_df, by.x = "linenumber", by.y = "index" )
namesandsent<- merge(namesandsent ,wordcountfix, by.x = "linenumber", by.y = "Group.1" )
namesandsent <- namesandsent[!duplicated(namesandsent$Player),]
namesandsent$negrate<- namesandsent$negative/ namesandsent$x
namesandsent$posrate<- namesandsent$positive/ namesandsent$x
namesandsent$sentrate<- namesandsent$sentiment/ namesandsent$x

#get all the words in the bing lexicon
bing_word_counts <- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#show top 25 positive and negatives words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme(text = element_text(size = 25))+
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#create some basic plots
plot(namesandsent$posrate,namesandsent$linenumber, xlim = c(0.07,.13))
plot(namesandsent$positive,namesandsent$linenumber)


##############
#I am about to create several different graphs. 

text_df$year<-as.numeric(text_df$year)


#all picks and rounds analysis for the the NRC and Bing lexicon.  Note that Index =  year. I can switch it to linenumber and it would show the sentiment for each draft pick instead of year
bing_and_nrc0 <- bind_rows(text_df %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          text_df %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = year %/% 1, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_and_nrc0<-subset(bing_and_nrc0, method == "NRC")

#create bar graph for yearly sentiment using NRC. 
bind_rows( bing_and_nrc0) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  xlab("Draft Position") +
  ylab("Positive/Wordcount") +
  facet_wrap(~method, ncol = 1, scales = "free_y")

##### NRC for top 10 players
top10<-subset(text_df,linenumber<11)

bing_and_nrc <- bind_rows(top10 %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          top10 %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = year %/% 1, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_and_nrc<-subset(bing_and_nrc, method=="NRC")

bind_rows( bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE, fill="darkgreen") +
  xlab("Draft Position") +
  ggtitle("Picks 1-10")+
  facet_wrap(~method, ncol = 1, scales = "free_y")
#random new chart for top ten picks each year
sentexclude2020<- subset(bing_and_nrc,index<2020)
ggplot(data=sentexclude2020, aes(x=index,y=sentiment,fill= method)) +
  geom_bar(position="dodge",stat="identity", fill = "darkblue") + 
  coord_flip() +
  ggtitle("Draft Class Sentiment")

#nrc for 11-30
top1130<-subset(text_df,linenumber<31 & linenumber >10)

bing_and_nrc2 <- bind_rows(top1130 %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          top1130 %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = year %/% 1, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_and_nrc2<-subset(bing_and_nrc2, method=="NRC")

bind_rows( bing_and_nrc2) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  xlab("Draft Position") +
  ggtitle("Picks 11-30") +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#nrc for first round
toprd<-subset(text_df,linenumber<31)

bing_and_nrc3 <- bind_rows(toprd %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          toprd %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = year %/% 1, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows( bing_and_nrc3) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

NRC<-subset(bing_and_nrc, method == "NRC")

#create some rate stats
namesandNRC<- merge(text_df,NRC, by.x = "linenumber", by.y = "index" )
namesandNRC <- namesandNRC[!duplicated(namesandNRC$Player),]
namesandNRC$negrate<- namesandNRC$negative/ namesandNRC$wordocunt
namesandNRC$posrate<- namesandNRC$positive/ namesandNRC$wordocunt
namesandNRC$sentrate<- namesandNRC$sentiment/ namesandNRC$wordocunt


par(mfrow=c(1,1))

#positive and negative words
text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 200)
#words from all the text
text_df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 300))
