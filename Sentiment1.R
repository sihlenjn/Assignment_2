library(tidyverse)
library(tidytext)
library(textdata) 
library(stringr)
library(gridExtra)
library(lubridate)

load('data.RData')
# correct the date on rama speech
sona$date[36] = '9-02-2023'

# tokenize the data into words and select the presidents and remove stop words
tidy_sona = unnest_tokens(sona,word,speech,token = "words") %>%
              filter(!word %in% stop_words$word, str_detect(word, '[A-Za-z]')) # remove stop words

# Most positive in the entire speeches #####
speech_senti =  tidy_sona %>% left_join(bing) %>%                              
                select(word, sentiment,year,date) %>% 
                mutate(sentiment = ifelse(is.na(sentiment),'neutral',sentiment)) 
              # make all the word neutral not in the lexicon neutral

pos = speech_senti %>% filter(sentiment == 'positive') %>% count(word) %>%
        arrange(desc(n)) %>% filter(rank(desc(n)) <= 15) %>%
        ggplot(aes(reorder(word,n),n))+geom_col(fill = 'green')+
        coord_flip()+xlab('') + ylab('n')+ggtitle("positive")+
        theme(axis.text.y = element_text(size = 10))

neg = speech_senti %>% filter(sentiment == 'negative') %>% count(word) %>%
          arrange(desc(n)) %>% filter(rank(desc(n)) <= 15) %>%
          ggplot(aes(reorder(word,n),n))+geom_col(fill = 'red')+
          coord_flip()+xlab('') + ylab('n')+ggtitle("negative")+
          theme(axis.text.y = element_text(size = 10))
grid.arrange(pos,neg,ncol=2)


# Most pos and neg by presidents ####

pres_words = list()
for (i in 1:6) {
  pres_words[[i]] = tidy_sona  %>% filter(pres_num==i) %>% select(word,date,year)
}
names(pres_words) = unique(sona$president_13)

# load the bing lexicon
# load 

pres_sentiment <- list()
for (i in 1:6) {
  pres_sentiment[[i]] = pres_words[[i]] %>% left_join(bing) %>%                              
                        select(word, sentiment, everything()) %>% 
                        mutate(sentiment = ifelse(is.na(sentiment),'neutral',sentiment)) 
    # make all the word neutral not in the lexicon neutral
}
names(pres_sentiment) = unique(sona$president_13)

# Look top 5  positive for each president
n1 =pres_sentiment[[1]] %>% filter(sentiment == 'positive') %>% count(word) %>%
  arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
  ggplot(aes(reorder(word,n),n))+geom_col(fill = 1)+
  coord_flip()+xlab('') + ylab('n')+
  theme(axis.text.y = element_text(size = 10))
n2 = pres_sentiment[[2]] %>% filter(sentiment == 'positive') %>% count(word) %>%
  arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
  ggplot(aes(reorder(word,n),n))+geom_col(fill =2)+
  coord_flip()+xlab('') + ylab('n')+
  theme(axis.text.y = element_text(size = 10))
n3 = pres_sentiment[[3]]%>% filter(sentiment == 'positive') %>% count(word) %>%
  arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
  ggplot(aes(reorder(word,n),n))+geom_col(fill = 3)+
  coord_flip()+xlab('') + ylab('n')+
  theme(axis.text.y = element_text(size = 10))
n4 = pres_sentiment[[4]] %>% filter(sentiment == 'positive') %>% count(word) %>%
  arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
  ggplot(aes(reorder(word,n),n))+geom_col(fill = 4)+
  coord_flip()+xlab('') + ylab('n')+
  theme(axis.text.y = element_text(size = 10))
n5 = pres_sentiment[[5]]  %>% filter(sentiment == 'positive') %>% count(word) %>%
  arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
  ggplot(aes(reorder(word,n),n))+geom_col(fill =5)+
  coord_flip()+xlab('') + ylab('n')+
  theme(axis.text.y = element_text(size = 10))
n6 = pres_sentiment[[6]]  %>% filter(sentiment == 'positive') %>% count(word) %>%
  arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
  ggplot(aes(reorder(word,n),n))+geom_col(fill =6 )+
  coord_flip()+xlab('') + ylab('n')+
  theme(axis.text.y = element_text(size = 10))
grid.arrange(n1,n2,n3,n4,n5,n6,ncol=2,nrow=3)


# Look top 5  negative for each president
p1 = pres_sentiment[[1]] %>% filter(sentiment == 'negative') %>% count(word) %>%
        arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
        ggplot(aes(reorder(word,n),n))+geom_col(fill = 1)+
        coord_flip()+xlab('') + ylab('n')+
        theme(axis.text.y = element_text(size = 10))
p2 = pres_sentiment[[2]] %>% filter(sentiment == 'negative') %>% count(word) %>%
        arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
        ggplot(aes(reorder(word,n),n))+geom_col(fill =2)+
        coord_flip()+xlab('') + ylab('n')+
        theme(axis.text.y = element_text(size = 10))
p3 = pres_sentiment[[3]]%>% filter(sentiment == 'negative') %>% count(word) %>%
      arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
      ggplot(aes(reorder(word,n),n))+geom_col(fill = 3)+
      coord_flip()+xlab('') + ylab('n')+
      theme(axis.text.y = element_text(size = 10))
p4 = pres_sentiment[[4]] %>% filter(sentiment == 'negative') %>% count(word) %>%
        arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
        ggplot(aes(reorder(word,n),n))+geom_col(fill = 4)+
        coord_flip()+xlab('') + ylab('n')+
        theme(axis.text.y = element_text(size = 10))
p5 = pres_sentiment[[5]]  %>% filter(sentiment == 'negative') %>% count(word) %>%
        arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
        ggplot(aes(reorder(word,n),n))+geom_col(fill =5)+
        coord_flip()+xlab('') + ylab('n')+
        theme(axis.text.y = element_text(size = 10))
p6 = pres_sentiment[[6]]  %>% filter(sentiment == 'negative') %>% count(word) %>%
        arrange(desc(n)) %>% filter(rank(desc(n)) <= 5) %>%
        ggplot(aes(reorder(word,n),n))+geom_col(fill =6 )+
        coord_flip()+xlab('') + ylab('n')+
        theme(axis.text.y = element_text(size = 10))
grid.arrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)


#  Changes in sentiment over time  ####

# Remove neutral words as they are dominant

# all speeches
senti_per_year <- speech_senti %>% group_by(year, sentiment) %>%
                        summarize(n = n())

ggplot(filter(senti_per_year , sentiment != 'neutral'),aes(x=year,y=n,fill=sentiment)) +
  geom_col() + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=10))

# plot the proportion
senti.prop <-senti_per_year  %>% left_join(senti_per_year  %>% 
                          group_by(year) %>% 
                          summarise(total = sum(n))) %>%
                          mutate(freq = n/total) 


# sentiment over all speeches
senti.prop %>% filter(sentiment != 'neutral') %>%
                ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
                geom_line() + geom_smooth(aes(colour = sentiment))+
                xlab('year') + ylab('freq')+
                theme(axis.text.x = element_text(angle = 90, hjust = 1,size=10),
                      axis.text.y = element_text(size = 10))

# individual presidents
year_sentiments = list()
for (i in 1:6) {
  year_sentiments[[i]] <- pres_sentiment[[i]] %>% group_by(year, sentiment) %>%
                              summarize(n = n())
}

# Proportion of sentiments for each president
prop_year = list()
for (i in 1:6) {
  prop_year[[i]] <-year_sentiments[[i]] %>% left_join(year_sentiments[[i]]%>% 
                                              group_by(year) %>% 
                                              summarise(total = sum(n))) %>%
                                              mutate(freq = n/total) 
}
# Look at Zuma and Mbeki
# Mbeki
pp1 = prop_year[[3]] %>% filter(sentiment != 'neutral') %>%
          ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
          geom_line() + geom_smooth(aes(colour = sentiment))+
          xlab('year') + ylab('freq')+ggtitle('Mbeki')
          theme(axis.text.x = element_text(angle = 90, hjust = 1,size=10),
                axis.text.y = element_text(size = 10))
# Zuma
pp2 =prop_year[[4]] %>% filter(sentiment != 'neutral') %>%
        ggplot(aes(x = year, y = freq, colour = sentiment,group=sentiment)) +
        geom_line() + geom_smooth(aes(colour = sentiment))+
        xlab('year') + ylab('freq')+ggtitle('Zuma')+
        theme(axis.text.x = element_text(angle = 90, hjust = 1,size=10),
              axis.text.y = element_text(size = 10))

grid.arrange(pp1,pp2,ncol=2)

# fit1 <- glm(freq ~ year, data = senti.prop %>% filter(sentiment == 'negative'),
#             family = binomial(link ="logit"))
save.image('sentiment1.RData')

