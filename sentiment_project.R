library(ggplot2)
library(tidytext)
library(dplyr)
library(readr)

service <- read_csv("data.csv")
View(service)

# Change Negative score 0 -> -1
service$score <- ifelse(service$score == 0, -1, 1)

tidy_service <- service %>% unnest_tokens(word, review)

nrc_sentiments <- tidy_service %>% inner_join(get_sentiments("nrc"), by = "word")
bing_sentiments <- tidy_service %>% inner_join(get_sentiments("bing"), by = "word")
afinn_sentiments <- tidy_service %>% inner_join(get_sentiments("afinn"), by = "word")

# nrc lexicion does not have scores. It expresses sentiments by vocabulary such as 'joy', 'positive', 'sad'
nrc_sentiments <- nrc_sentiments[nrc_sentiments$sentiment %in% c('positive', 'negative'),]

nrc_sentiments$score <- ifelse(nrc_sentiments$sentiment == "negative", -1, 1)
bing_sentiments$score <- ifelse(bing_sentiments$sentiment == "negative", -1, 1)

nrc_aggregate <- nrc_sentiments %>% select(review_id, score) %>% group_by(review_id) %>% summarise(nrc_score = sum(score))
bing_aggregate <- bing_sentiments %>% select(review_id, score) %>% group_by(review_id) %>% summarise(bing_score = sum(score))
afinn_aggregate <- afinn_sentiments %>% select(review_id, score) %>% group_by(review_id) %>% summarise(afinn_score = sum(score))

View(afinn_aggregate)
score_compare_service <- merge(x = service, y = nrc_aggregate, all.x = TRUE, by = 'review_id')
score_compare_service <- merge(x = score_compare_service, y= bing_aggregate, all.x = TRUE, by = 'review_id')
score_compare_service <- merge(x = score_compare_service, y= afinn_aggregate, all.x = TRUE, by = 'review_id')

score_compare_service[is.na(score_compare_service)] <- 0

score_compare_service$nrc_judgement <- ifelse(score_compare_service$nrc_score > 0, "positive", ifelse(score_compare_service$nrc_score < 0, "negative", "neutral" ))
score_compare_service$bing_judgement <- ifelse(score_compare_service$bing_score > 0, "positive", ifelse(score_compare_service$bing_score < 0, "negative", "neutral" ))
score_compare_service$afinn_judgement <- ifelse(score_compare_service$afinn_score > 0, "positive", ifelse(score_compare_service$afinn_score < 0, "negative", "neutral" ))

score_compare_service$score <- ifelse(score_compare_service$score > 0, "positive", ifelse(score_compare_service$score < 0, "negative", "neutral"))

table(score_compare_service$score, score_compare_service$nrc_judgement)
table(score_compare_service$score, score_compare_service$bing_judgement)
table(score_compare_service$score, score_compare_service$afinn_judgement)

##############################

incorrect_reviews <- score_compare_service[score_compare_service$nrc_judgement != score_compare_service$score &
                                             score_compare_service$bing_judgement != score_compare_service$score &
                                             score_compare_service$afinn_judgement != score_compare_service$score,]
View(incorrect_reviews)

##############################

# Remove every sentiment words from stop_words & bing

sentiment_words <- stop_words %>% full_join(get_sentiments("bing"), by = "word")
tidy_without_sentiment <- tidy_service %>% anti_join(sentiment_words, by = "word")
tidy_frequeny <- tidy_without_sentiment %>% group_by(word) %>% summarise(frequency = n())

View(tidy_frequeny)
