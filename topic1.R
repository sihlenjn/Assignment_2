library(topicmodels)
library(reshape)
library(reshape2)

load('data.RData')

sona_topic = sona %>% select(speech,pres_num)
sona_topic$pres_num = factor(sona_topic$pres_num)
sona_topic = sona_topic %>% mutate(speech_id = 1:nrow(sona_topic))


topic_tidy <- sona_topic %>% 
  unnest_tokens(word,speech, token = 'words', to_lower = T) %>%
  filter(!word %in% stop_words$word)

# remove numbers
d1 = topic_tidy  %>%  arrange(word)
topic_tidy = d1[-c(1:2407),] 

# count
  topic_tdf <- topic_tidy %>%
  group_by(speech_id,word) %>%
  count() %>%  
  ungroup()

dtm_speeches <-  topic_tdf  %>% 
                  cast_dtm(speech_id, word, n)

speeches_lda <- LDA(dtm_speeches, k = 3, control = list(seed = 1234))

# Word-topic probabilities ####
term <- as.character(speeches_lda @terms)

topics = list()
for (i in 1:3) {
  topics[[i]] = speeches_lda @beta[i,]
}

speeches_topics <- tibble(term = term, topic1 = topics[[1]], 
                          topic2 = topics[[2]],topic3=topics[[3]])

# convert to probabilities
speeches_topics <- tidy(speeches_lda, matrix = 'beta')
head(speeches_topics)

top_terms <- speeches_topics  %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip()


beta_spread <- speeches_topics %>%
  mutate(topic = paste0('topic', topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(topic3 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic2))


beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = 'Log2 ratio of beta in topic 2 / topic 1') +
  coord_flip()


# Document-topic probabilities ####
spechees_gamma <- sona_topic %>% 
  left_join(tidy(speeches_lda, matrix = 'gamma') %>% 
              mutate(speech_id= as.numeric(document)) %>%       
              select(-document) %>%
              spread(key = topic, value = gamma, sep = '_'))
spechees_gamma%>% group_by(pres_num) %>% summarize(ntopic1 = sum(topic_1 > 0.5))


spechees_gamma %>% filter(pres_num != 6) %>% arrange(desc(topic_2)) %>% 
                          head(3) %>% select(speech, topic_1, topic_2)
