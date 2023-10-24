### Aggregating sentiment over words3

# Add sentence index before splitting the data set
sona_sentence  = unnest_tokens(sona,sentence,speech,token = "sentences")
sona_sentence = sona_sentence  %>% mutate(index = c(1:nrow(sona_sentence)))

# Tokenize the sona sentence
tidy_words = unnest_tokens(sona_sentence,word,sentence,token = "words") %>%
          filter(!word %in% stop_words$word, str_detect(word, '[A-Za-z]')) # remove stop words
tidy_words = tidy_words %>% select(index,year, pres_num, index,word)


# sentiments per sentence
senti_word =  tidy_words %>% left_join(bing) %>%                              
              select(index,word, sentiment,year) %>% 
              mutate(sentiment = ifelse(is.na(sentiment),'neutral',sentiment)) 


# Done for
sentiments_per_sentence <- senti_word %>% group_by(index) %>%
                           summarize(net_sentiment = (sum(sentiment == 'positive') - sum(sentiment == 'negative')),
                           year =  year)

# convert year to character so that it matches the sona-year
sentiments_per_year$year = as.character(sentiments_per_year$year)

# 2020 top negative  and positive sentences
sona_sentence %>% left_join(sentiments_per_year) %>% 
              arrange(desc(net_sentiment)) %>%
       select(sentence, net_sentiment,index) 

neg_senti = sentiments_per_sentence %>%
  group_by(year) %>%
  summarize(prop_neg=sum(net_sentiment < 0)/n()) 

pos_senti = sentiments_per_sentence %>%
  group_by(year) %>%
  summarize(prop_pos=sum(net_sentiment > 0)/n()) 

s1 = ggplot(neg_senti,aes(x=as.numeric(year), y=prop_neg))+
    geom_line(col='red',lwd=1)+geom_smooth()+xlab('year') + ylab('proportion of negative sentiments')+
    theme(axis.text.x = element_text(size=10),
          axis.text.y = element_text(size = 10))



s2 = ggplot(pos_senti,aes(x=as.numeric(year), y=prop_pos))+
  geom_line(col='green',lwd=1)+geom_smooth(col="purple")+xlab('year') + ylab('proportion of positive sentiments')+
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size = 10))

grid.arrange(s1,s2)

