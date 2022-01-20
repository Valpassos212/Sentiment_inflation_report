library(tidyverse)
library(tidytext)
library(ggraph)
library(ggridges)
library(pdftools)
library(forcats)
library(reshape2)
library(tidyr)
library(igraph)
library(widyr)
library(viridis)
library(wordcloud)
library(topicmodels)
library(forecast)
library(qdap)
library(lubridate)
library(chron)


copom_links<- c("https://www.bcb.gov.br/content/publications/inflationreport/200703/INFREP200703-ri200703I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200706/INFREP200706-ri200706I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200709/INFREP200709-ri200709I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200712/INFREP200712-ri200712I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200803/INFREP200803-ri200803I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200806/INFREP200806-ri200806I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200809/INFREP200809-ri200809I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200812/INFREP200812-ri200812I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200903/INFREP200903-ri200903I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200906/INFREP200906-ri200906I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200909/INFREP200909-ri200909I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/200912/INFREP200912-ri200912I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201003/INFREP201003-ri201003I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201006/INFREP201006-ri201006I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201009/INFREP201009-ri201009I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201012/INFREP201012-ri201012I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201103/INFREP201103-ri201103I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201106/INFREP201106-ri201106I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201609/INFREP201609-ri201609I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201612/INFREP201612-ri201612I.pdf",
                
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201703/INFREP201703-ri201703I.pdf",
                
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201706/INFREP201706-ri201706I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201709/INFREP201709-ri201709I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201712/INFREP201712-ri201712I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201803/INFREP201803-ri201803I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201806/INFREP201806-ri201806I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201809/INFREP201809-ri201809I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201812/INFREP201812-ri201812I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201903/INFREP201903-ri201903I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201906/ri201906I.pdf",
                
                "https://www.bcb.gov.br/content/publications/inflationreport/201909/ri201909i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/201912/ri201912i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202003/ri202003i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202006/ri202006i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202009/ri202009i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202012/ri202012i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202103/ri202103i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202106/ri202106i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202109/ri202109i.pdf",
                
                "https://www.bcb.gov.br/content/ri/inflationreport/202112/ri202112i.pdf"
                
                )


# date stamps

dates1<- as.Date(seq.Date(from = as.Date("2007/3/1"), to = as.Date("2011/6/1"), by = "quarter"))


dates2<- as.Date(seq.Date(from = as.Date("2016/9/1"), to = as.Date("2021/12/1"), by = "quarter"))

dates<- c(as.character(dates1),as.character(dates2))

# creating the data frame with the words from the reports

df_copom <- data.frame(report=dates,stringsAsFactors = FALSE) %>%

  mutate(text= map(copom_links,pdf_text)) %>% 
  
  unnest(text) %>% 
  
  group_by(report) %>% mutate(page=row_number()) %>%
  
  ungroup() %>%
  
  mutate(text=strsplit(text,"\r")) %>% 
  
  unnest(text) %>% 
  
  mutate(text=gsub("\n","",text)) %>%
  
  group_by(report) %>% 
  
  mutate(line=row_number()) %>% 
  
  ungroup() %>% 
  
  select(report,line,page,text)

# counting words

copom_words<- df_copom %>%
  
  unnest_tokens(word, text)%>%
  
  count(report, word, sort = TRUE)%>%
  
  ungroup()

total_words<- copom_words %>%
  
  group_by(report) %>%
  
  summarize(total = sum(n))

#spllting word counts

ggplot(data = total_words, aes( x = dates, y = total)) +
  
  geom_line( color = "blueviolet") + 
  
  geom_point( shape = 21, fill = "grey83", color ="red4", size = 3, stroke = 1.1) +
  
  scale_y_continuous( labels = scales::comma) +
  
  theme_ridges( font_family = "Roboto") +
  
  labs(x = "Quarter", y = "Number of Words", title = "Number of Words BACEN Inflation Report") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  

# merging stop words with some "useless" words on the reports

custom_stop_words <- 
  bind_rows(tibble(word = c(tolower(month.abb), "one","two","three","four","five","six",
                                "seven","eight","nine","ten","eleven","twelve","mam","ered",
                                "produc","ing","quar","ters","sug","quar",'fmam',"sug",
                                "cient","thirty","pter",
                                "pants","ter","ening","ances","www.bcb.gov.br",
                                "tion","fig","ure","figure","src"), 
                       lexicon = c("custom")), 
            stop_words)

custom_stop_words2 <- 
  bind_rows(tibble(word = c("debt",
                                "gross",
                                "crude",
                                "well",
                                "maturity",
                                "work",
                                "marginally",
                                "leverage"), 
                       lexicon = c("custom")), 
            custom_stop_words)



# words simple frequency


copom_text<- 
  
  df_copom%>%
  
  select(report,page,line,text) %>%
  unnest_tokens(word,text)

copom_text %>%
  
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%
  
  filter(word!="") %>%
  
  anti_join(custom_stop_words) %>%
  
  group_by(report) %>%
  
  count(word, sort = TRUE) %>%
  
  mutate(rank = row_number()) %>%
  
  ungroup() %>%
  
  arrange(rank, report) %>%
  
  filter(rank < 10) %>%
  
  ggplot(aes(y=n, x = fct_reorder(word,n))) +
  
  geom_col(fill = "orange") +
  
  facet_wrap(~report, scales = "free", ncol = 5) +
  
  coord_flip() +
  
  theme_ridges(font_family = "Roboto", font_size = 10) +
  
  labs(x = "", y = "", title = "Frequent Words Inflation Reports",
       
       subtitle = "Clean Data"
       
       )
 



# words tf-idf frequency 
  
copom_text_v2<- 
  
  copom_text %>%
  
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%
  
  filter(word!="") %>%
  
  count(report,word, sort = TRUE) %>%
  
  bind_tf_idf(word, report, n) %>%
  
  arrange(desc(tf_idf))
  
copom_text_v2 %>%
  
  anti_join(custom_stop_words, by = "word") %>%
  
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  
  group_by(report) %>%
  
  mutate(id = row_number()) %>%
  
  ungroup() %>%
  
  filter(id<10) %>%
  
  ggplot(aes(word, tf_idf, fill = report)) +
  
  geom_col(show.legend = FALSE) +
  
  labs( x = NULL, y = "tf-idf") +
  
  facet_wrap(~report, scales = "free", ncol = 5) +
  
  coord_flip() +
  
  
  theme_ridges(font_family = "Roboto", font_size = 10) +
  
  theme(axis.text.x = element_blank()) +
  
  labs(x = "", y = "tf-idf", title = "Words tf-idf")
  

# sentiment accross CBB reports


copom_sentiment<- 
  
  copom_text%>%
  
  anti_join(custom_stop_words2) %>%
  
  inner_join(get_sentiments("bing")) %>%
  
  count(report, index = line %/%240, sentiment)%>%
  
  spread(sentiment,n, fill = 0) %>%
  
  mutate(sentiment = positive - negative)

ggplot(copom_sentiment, aes(index, sentiment, fill = sentiment >0)) +
  
  geom_col(show.legend = FALSE) +
  
  scale_fill_manual(values = c("red","blue4")) +
  
  facet_wrap(~report, ncol = 5, scales = "free_x") +
  
  theme_ridges(font_family = "Roboto") +
  
  labs(x= "index", y = "Sentiment", title  = "Sentiment Accross CBB Reports")
  

#sentiment on each CBB report

copom_sentiment_by_report<- 
  
  copom_text %>% 
  
  anti_join(custom_stop_words2) %>%
  
  inner_join(get_sentiments("bing")) %>%
  
  count(report, sentiment)%>%
  
  spread(sentiment, n, fill =0) %>%
  
  mutate(sentiment = positive - negative)

g1<-ggplot(copom_sentiment_by_report, aes(factor(dates), sentiment/(negative+positive), fill = sentiment>0)) +
  
  geom_col(show.legend = FALSE) + scale_fill_manual(values = c("red","blue4"))+
  
  theme_ridges(font_family = "Roboto", font_size = 10) +
  
  labs(x= "Reports", y = "Sentiment index", title  = "Sentiment CBB Inflation Reports")+
  
 theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
  


  
  



