setwd("D:/R/WhatsappChatAnalysis")
library(rwhatsapp)
library(ggplot2)
library(lubridate)
library(ggimage)
library(tidytext)
library(stopwords)
library(tidyverse)
library(tidymodels)

chat <- rwa_read("D:/R/WhatsappChatAnalysis/TextMessage.txt") %>%  filter(!is.na(author))
head(chat,3)

chat$Date <- as.Date(chat$time)
chat$Time <- format(as.POSIXct(chat$time),format="%H:%M:%S")
names(chat)[names(chat) == "time"] <- "DateAndTime"
head(chat)



chat %>% 
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n, fill=author)) +
  geom_bar(stat = "identity") + xlab("Author") + ylab("Number Of Messages") +
  coord_flip() + theme_bw() +
  ggtitle("Relation between the messages sent by Each Person")



chat %>%
  count(Date) %>%
  ggplot(aes(x = Date, y = n)) + aes(fill=Date) +
  geom_bar(stat = "identity") + theme_bw() +
  ylab("Number Of Messages") + xlab("Months") + ggtitle("Chat Timeline")




to_remove <- c(stopwords(language = 'en'),"media","omitted","ref","3","yes","yess","stu","us","go",
               "bro","one","well","get","just","idk","fuck")
chatx <- chat%>% unnest_tokens(input = text,output = word) %>%
  filter(!word %in% to_remove)

chatx %>% count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")  

library(wordcloud)
wordcloud(chatx$word,max.words = 80,random.order = F,colors=brewer.pal(8,"Set1"),scale=c(2,1))


library(syuzhet)
sentimentcheck <- get_nrc_sentiment(chat$text)
head(sentimentcheck)
textdf <- cbind(chat$text,sentimentcheck)
head(textdf)
sentimentdf <- data.frame(colSums(textdf[,2:11]))
head(sentimentdf)
names(sentimentdf) <- "SentimentCount"
sentimentdf <- cbind("sentiment"=rownames(sentimentdf),sentimentdf)
head(sentimentdf)


cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

x <-ggplot(sentimentdf) +
  aes(x=sentiment,y=SentimentCount)  +
  geom_bar(stat='identity') +  aes(fill=sentiment) + theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))
x

