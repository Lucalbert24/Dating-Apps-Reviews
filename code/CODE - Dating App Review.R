library(MASS)
library(lsa)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tm)
library(textstem)
library(deepMOU)
library(dplyr)

# Data preparation and divison of the dataset based on the App
data=read.csv(file.choose())
data <- na.omit(data)
data$Year=as.integer(substr(data$Date.Time,7,10))
data21=data[data$Year==2021,]

bumble <- data21[data21$App == "Bumble", ]
tinder <- data21[data21$App == "Tinder", ]
hinge  <- data21[data21$App == "Hinge", ]

set.seed(1234)

n_wordsh <- sapply(strsplit(hinge$Review, "\\s+"), length)
h21 <- hinge[n_wordsh >= 7, ]
indexh21=sample(1:nrow(h21),5000,replace=F)
rh21 <- h21$Review[indexh21]
thumbsh21 <- h21$X.ThumbsUp[indexh21]

n_wordst <- sapply(strsplit(tinder$Review, "\\s+"), length)
t21 <- tinder[n_wordst >= 7, ]
indext21=sample(1:nrow(t21),5000,replace=F)
rt21 <- t21$Review[indext21]
thumbst21 <- t21$X.ThumbsUp[indext21]

n_wordsb <- sapply(strsplit(bumble$Review, "\\s+"), length)
b21 <- bumble[n_wordsb >= 7, ]
indexb21=sample(1:nrow(b21),5000,replace=F)
rb21 <- b21$Review[indexb21]
thumbsb21 <- b21$X.ThumbsUp[indexb21]

# Preprocessing function
preprocessing <- function(review){
  corpus_raw <- VCorpus(VectorSource(review))
  corpus <- corpus_raw %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords_en) %>%
    tm_map(content_transformer(lemmatize_strings)) %>%
    tm_map(removeWords, c(stopwords("english"))) %>%
    tm_map(stripWhitespace)
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, 0.995)  
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  df <- data.frame(word = names(word_freqs), freq = word_freqs)
  return(list(
    tdm        = tdm,
    word_freqs = word_freqs,
    corpus = corpus,
    wordcloud  = function() {
      wordcloud(words = df$word, freq = df$freq,
                min.freq     = 5,
                max.words    = 100,
                random.order = FALSE,
                scale        = c(3, 0.5),
                colors       = brewer.pal(8, "Dark2"))},
    Sample = review
  ))
}

prep_hinge <- preprocessing(rh21)
prep_tinder <- preprocessing(rt21)
prep_bumble <- preprocessing(rb21)

par(mar = c(0, 0, 0, 0))
prep_hinge$wordcloud()
prep_bumble$wordcloud()
prep_tinder$wordcloud()

# Sentyment analysis Function
sent_analysis <- function(review, bing_sentiments = get_sentiments("bing")){
  
  rev_df <- tibble(line = 1:length(review), text = review)
  complaints_words <- rev_df %>%
    unnest_tokens(word, text)
  
  data("stop_words")
  complaints_words_clean <- complaints_words %>%
    anti_join(stop_words, by = "word")
  
  complaints_sentiment <- complaints_words_clean %>%
    inner_join(bing_sentiments, by = "word")
  
  complaint_scores <- complaints_sentiment %>%
    count(line, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment_score = positive - negative)
  
  final_sentiment <- rev_df %>%
    left_join(complaint_scores, by = "line") %>%
    mutate(
      sentiment_score = ifelse(is.na(sentiment_score), 0, sentiment_score),
      sentiment_category = case_when(
        sentiment_score > 0 ~ "Positive",
        sentiment_score < 0 ~ "Negative",
        TRUE                ~ "Neutral"))
  
  return(list(
    complaints_token      = complaints_words,
    complaints_sentiment  = complaints_sentiment,
    complaint_scores      = complaint_scores,
    final_sentiment       = final_sentiment,
    
    plot_sentiment_scores = function(){
      ggplot(final_sentiment, aes(x = sentiment_score)) +
        geom_histogram(binwidth = 1, fill = "red", color = "black") +
        theme_minimal() +
        labs(title = "Distribution of Complaint Sentiment Scores",
             x = "Sentiment Score (Positive - Negative)",
             y = "Number of Complaints")},
    
    plot_sentiment_categories = function(){
      ggplot(final_sentiment, aes(x = sentiment_category, fill = sentiment_category)) +
        geom_bar() +
        scale_fill_manual(values = c("Positive" = "green",
                                     "Negative" = "red",
                                     "Neutral"  = "gray")) +
        theme_minimal() +
        labs(title = "Sentiment Category Distribution",
             x = "Sentiment Category",
             y = "Number of Complaints")
    }
  ))
}

sent_hinge <- sent_analysis(rh21)
sent_bumble <- sent_analysis(rb21)
sent_tinder <- sent_analysis(rt21)

sent_hinge$plot_sentiment_scores() +
  labs(title = "Distribution of Complaint Sentiment Score - Hinge 2021")
sent_hinge$plot_sentiment_categories() +
  labs(title = "Sentiment Category Distribution - Hinge 2021")

sent_bumble$plot_sentiment_scores() +
  labs(title = "Distribution of Complaint Sentiment Score - Bumble 2021")
sent_bumble$plot_sentiment_categories() +
  labs(title = "Sentiment Category Distribution - Bumble 2021")

sent_tinder$plot_sentiment_scores() +
  labs(title = "Distribution of Complaint Sentiment Score - Tinder 2021")
sent_tinder$plot_sentiment_categories() +
  labs(title = "Sentiment Category Distribution - Tinder 2021")

# Removing words that aren't useful for the topic analysis
words_to_remove <- c(
  "and", "or", "but", "so", "because", "although", "though",
  "while", "whereas", "since", "unless", "until", "if",
  "when", "whenever", "where", "wherever", "after", "before",
  "once", "whether", "nor", "yet",
  "just", "many", "much", "can", "get", "will", "also",
  "less", "now", "way", "far", "actually", "see", "one", "lot",
  "app", "apps", "hinge", "dont", "ive", "wont", "try",
  "good", "great", "nice", "awesome", "amazing", "excellent",
  "perfect", "fantastic", "wonderful", "brilliant", "cool",
  "decent", "fine", "solid", "useful", "helpful",
  "better", "best", "worse", "worst",
  "bad", "terrible", "awful", "horrible", "poor",
  "annoying", "frustrating", "disappointing", "useless",
  "stupid", "dumb", "ridiculous", "weird", "boring",
  "interesting", "simple", "easy", "hard", "difficult",
  "pretty", "super",
  "love", "loved", "loving", "liked",
  "hate", "hated", "dislike", "disliked",
  "really", "very", "too", "quite", "extremely",
  "totally", "absolutely", "definitely", "probably",
  "cant", "doesnt", "havent", "arent", "didnt",
  "time", "day", "sometimes", "week", "ago", "month",
  "hour", "late", "maybe", "overall",
  "little", "fun", "real", "free", "able",
  "okay", "unique", "genuine", "low", "worth",
  "potential", "disappoint",
  "look", "start", "feel", "allow", "change",
  "please", "thats", "write", "tell", "help",
  "leave", "base", "otherwise", "review",
  "enjoy", "force", "hope", "lose",
  "youre", "theres", "numb", "wish", "annoy",
  "instead", "etc", "thank",'suck','send',
  'set','receive','fix','upload','else','check',
  'mean','cause','anyomre','past','lol','isnt','continue')

txt_mtx_mou <- function(prep, review, thumbs,
                        words_to_remove,
                        sparse = 0.99,
                        min_words = 2) {
  
  corpus <- prep$corpus %>%
    tm_map(removeWords, words_to_remove) %>%
    tm_map(stripWhitespace)
  
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, sparse)
  
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  full <- t(m)
  
  ok <- rowSums(full) > 0
  full <- full[ok, ]
  review_full <- review[ok]
  thumbs_full <- thumbs[ok]
  
  informative_words <- rowSums(full)
  summary_info <- summary(informative_words)
  
  ok_info <- informative_words >= min_words
  
  clean <- full[ok_info, ]
  review_clean <- review_full[ok_info]
  thumbs_clean <- thumbs_full[ok_info]
  
  review_other <- review_full[!ok_info]
  thumbs_other <- thumbs_full[!ok_info]
  
  return(list(
    corpus = corpus,
    tdm = tdm,
    word_freqs = word_freqs,
    clean = clean,
    review_clean = review_clean,
    thumbs_clean = thumbs_clean,
    review_other = review_other,
    thumbs_other = thumbs_other
  ))
}

# Funzione che cerca il numero di k ottimale.
scegli_k_mou <- function(dati, k_min = 2, k_max = 10, seed = 1234,
                         titolo = "Choice of the number of topics - MoU") {
  set.seed(seed)
  k_values <- k_min:k_max
  fit_list <- lapply(k_values, function(k) {
    mou_EM(dati, k = k, seed = seed)
  })
  aic_values <- sapply(fit_list, function(fit) fit$AIC)
  names(aic_values) <- paste0("k=", k_values)
  
  plot(k_values, aic_values,
       type = "b",
       xlab = "Number of topics",
       ylab = "AIC",
       main = titolo)
  
  best_index <- which.min(aic_values)
  best_k <- k_values[best_index]
  fit_best <- fit_list[[best_index]]
  
  return(list(
    AIC = aic_values,
    best_k = best_k,
    fit_best = fit_best
  ))
}

ris <- txt_mtx_mou(
  prep = prep_hinge,
  review = rh21,
  thumbs = thumbsh21,
  words_to_remove = words_to_remove)

cleanh <- ris$clean

ris_mouh <- scegli_k_mou(
  dati = cleanh,
  k_min = 2,
  k_max = 15,
  seed = 1234,
  titolo = "Choice of the number of topics - MoU Hinge- 2021")

ris_mouh$AIC
ris_mouh$best_k
ris_mouh$fit_best

analisi_topic_mou <- function(fit_best, best_k, dati,
                              review_clean, thumbs_clean,
                              n_top_words = 10,
                              n_top_reviews = 5) {
  
  topic <- fit_best$clusters
  omega <- sapply(1:best_k, function(j) {
    wc <- colSums(dati[topic == j, , drop = FALSE])
    wc / sum(wc)
  })
  omega <- t(omega)
  colnames(omega) <- colnames(dati)
  rownames(omega) <- paste0("Topic_", 1:best_k)
  
  top_words <- do.call(rbind, lapply(1:best_k, function(j) {
    x <- sort(omega[j, ], decreasing = TRUE)[1:n_top_words]
    data.frame(
      Topic = j,
      Rank = 1:n_top_words,
      Word = names(x),
      Prob = round(as.numeric(x), 4)
    )
  }))
  
  top_reviews <- do.call(rbind, lapply(1:best_k, function(j) {
    ind <- which(topic == j)
    parole <- top_words$Word[top_words$Topic == j]
    
    score_text <- rowSums(dati[ind, parole, drop = FALSE]) / rowSums(dati[ind, , drop = FALSE])
    score_final <- score_text * (1 + log1p(thumbs_clean[ind]))
    
    ord <- order(score_final, decreasing = TRUE)[1:min(n_top_reviews, length(ind))]
    
    data.frame(
      Topic = j,
      Score = round(score_final[ord], 3),
      ThumbsUp = thumbs_clean[ind][ord],
      Review = review_clean[ind][ord])
  }))
  
  return(list(
    topic_distribution = table(topic),
    word_distribution = omega,
    top_words = top_words,
    top_reviews = top_reviews,
    results = data.frame(
      Review = review_clean,
      ThumbsUp = thumbs_clean,
      Topic = topic)
  ))
}

ris_topich <- analisi_topic_mou(
  fit_best = ris_mouh$fit_best,
  best_k = ris_mouh$best_k,
  dati = cleanh,
  review_clean = ris$review_clean,
  thumbs_clean = ris$thumbs_clean)

ris_topich$topic_distribution
ris_topich$top_words
ris_topich$top_reviews

# After checking the most important words for each topic, we manually aggregate
# the topics described by the same words or by similar words.
ris_topich$results$MacroTopic <- dplyr::case_when(
  ris_topich$results$Topic %in% c(1, 8, 14) ~ "Payments",
  ris_topich$results$Topic %in% c(2, 3) ~ "Match/Messages",
  ris_topich$results$Topic %in% c(5, 6) ~ "Functionalities",
  ris_topich$results$Topic %in% c(7, 9) ~ "Dating",
  ris_topich$results$Topic %in% c(12, 13) ~ "Fake user/Ban",
  ris_topich$results$Topic == 4 ~ "Support",
  ris_topich$results$Topic == 10 ~ "Technical Problems",
  ris_topich$results$Topic == 11 ~ "Money Waste")

# Add the discarded review from the classification and label them as "Not Classified"
hdisc <- data.frame( 
  Review = ris$review_other,
  ThumbsUp = ris$thumbs_other, 
  Topic = NA,
  MacroTopic = "Not Classified")

ris_topich$results_all <- rbind(ris_topich$results,hdisc)
table(ris_topich$results_all$MacroTopic)

sent_df <- sent_hinge$final_sentiment[, c("text", "sentiment_score", "sentiment_category")]
ris_topich$results_all <- ris_topich$results_all %>%
  left_join(sent_df, by = c("Review" = "text"))

tab_macro_sent <- table(ris_topich$results_all$MacroTopic, ris_topich$results_all$sentiment_category)
tab_macro_sent

par(mar = c(9, 4, 4, 2))
barplot(t(tab_macro_sent),
        col = c("red", "grey", "green"),
        las = 2,
        main = "Sentiment per macro-topic HINGE",
        ylab = "Number of reviews")

legend("topright",
       legend = colnames(tab_macro_sent),
       fill = c("red", "grey", "green"),
       bty = "n")

################################################################################
# MOU BUMBLE
################################################################################

ris_b <- txt_mtx_mou(
  prep = prep_bumble,
  review = rb21,
  thumbs = thumbsb21,
  words_to_remove = words_to_remove
)

cleanb <- ris_b$clean

ris_moub <- scegli_k_mou(
  dati = cleanb,
  k_min = 2,
  k_max = 15,
  seed = 1234,
  titolo = "Choice of the number of topics - MoU Bumble - 2021"
)

ris_topicb <- analisi_topic_mou(
  fit_best = ris_moub$fit_best,
  best_k = ris_moub$best_k,
  dati = cleanb,
  review_clean = ris_b$review_clean,
  thumbs_clean = ris_b$thumbs_clean
)

ris_topicb$topic_distribution
ris_topicb$top_words
ris_topicb$top_reviews

ris_topicb$results$MacroTopic <- dplyr::case_when(
  ris_topicb$results$Topic %in% c(1, 8, 14) ~ "Payments",
  ris_topicb$results$Topic %in% c(2, 3)     ~ "Match/Messages",
  ris_topicb$results$Topic %in% c(5, 6)     ~ "Functionalities",
  ris_topicb$results$Topic %in% c(7, 9)     ~ "Dating",
  ris_topicb$results$Topic %in% c(12, 13)   ~ "Fake/Ban",
  ris_topicb$results$Topic == 4             ~ "Support",
  ris_topicb$results$Topic == 10            ~ "Technical Problems",
  ris_topicb$results$Topic == 11            ~ "Money Waste")

bdisc <- data.frame(
  Review = ris_b$review_other,
  ThumbsUp = ris_b$thumbs_other,
  Topic = NA,
  MacroTopic = "Not Classified"
)

ris_topicb$results_all <- rbind(ris_topicb$results, bdisc)
table(ris_topicb$results_all$MacroTopic)
sent_df <- sent_bumble$final_sentiment[, c("text", "sentiment_score", "sentiment_category")]

ris_topicb$results_all <- ris_topicb$results_all %>%
  left_join(sent_df, by = c("Review" = "text"))

tab_macro_sent_b <- table(
  ris_topicb$results_all$MacroTopic,
  ris_topicb$results_all$sentiment_category)

tab_macro_sent_b

par(mar = c(8, 4, 4, 2))
barplot(t(tab_macro_sent_b),
        col = c("red", "grey", "green"),
        las = 2,
        cex.names = 0.8,
        main = "Sentiment per macro-topic BUMBLE",
        ylab = "Number of reviews")

legend("topright",
       legend = colnames(tab_macro_sent_b),
       fill = c("red", "grey", "green"),
       bty = "n")

################################################################################
# MOU TINDER
################################################################################
ris_t <- txt_mtx_mou(
  prep = prep_tinder,
  review = rt21,
  thumbs = thumbst21,
  words_to_remove = words_to_remove
)

cleant <- ris_t$clean

ris_mout <- scegli_k_mou(
  dati = cleant,
  k_min = 2,
  k_max = 15,
  seed = 1234,
  titolo = "Scelta del numero di topic - MoU Tinder - 2021"
)

ris_topict <- analisi_topic_mou(
  fit_best = ris_mout$fit_best,
  best_k = ris_mout$best_k,
  dati = cleant,
  review_clean = ris_t$review_clean,
  thumbs_clean = ris_t$thumbs_clean
)

ris_topict$topic_distribution
ris_topict$top_words
ris_topict$top_reviews

ris_topict$results$MacroTopic <- dplyr::case_when(
  ris_topict$results$Topic %in% c(1, 10, 13) ~ "Payments/Premium",
  ris_topict$results$Topic == 2              ~ "Money Waste",
  ris_topict$results$Topic == 3              ~ "Filter/Search",
  ris_topict$results$Topic == 4              ~ "Login/Account",
  ris_topict$results$Topic == 5              ~ "Match/Messages",
  ris_topict$results$Topic == 6              ~ "Support",
  ris_topict$results$Topic == 7              ~ "Fake/Scam",
  ris_topict$results$Topic == 8              ~ "Profile/Pics/Bugs",
  ris_topict$results$Topic %in% c(9, 12)     ~ "Ban/Account Blocks",
  ris_topict$results$Topic == 11             ~ "Dating")

tdisc <- data.frame(
  Review = ris_t$review_other,
  ThumbsUp = ris_t$thumbs_other,
  Topic = NA,
  MacroTopic = "Not Classified"
)

ris_topict$results_all <- rbind(ris_topict$results, tdisc)

table(ris_topict$results_all$MacroTopic)

sent_df <- sent_tinder$final_sentiment[, c("text", "sentiment_score", "sentiment_category")]

ris_topict$results_all <- ris_topict$results_all %>%
  left_join(sent_df, by = c("Review" = "text"))

tab_macro_sent_t <- table(
  ris_topict$results_all$MacroTopic,
  ris_topict$results_all$sentiment_category
)

tab_macro_sent_t

par(mar = c(8, 4, 4, 2))
barplot(t(tab_macro_sent_t),
        col = c("red", "grey", "green"),
        las = 2,
        cex.names = 0.8,
        main = "Sentiment per macro-topic TINDER",
        ylab = "Number of reviews")

legend("topright",
       legend = colnames(tab_macro_sent_t),
       fill = c("red", "grey", "green"),
       bty = "n")