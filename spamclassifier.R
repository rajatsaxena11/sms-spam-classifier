

> setwd("C:/Users/Rajat Saxena/Desktop")
> sms_spam_df <- read.csv(file="spam_data.csv", stringsAsFactors=F)
> str(sms_spam_df)
> library(tm)
> sms_corpus <- Corpus(VectorSource(sms_spam_df$text))

 # translate all letters to lower case
> clean_corpus <- tm_map(sms_corpus, tolower)
 # remove numbers
> clean_corpus <- tm_map(clean_corpus, removeNumbers)
 # remove punctuation
> clean_corpus <- tm_map(clean_corpus, removePunctuation)
 # remove stopwords
> clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
 # remove extra whitespace
> clean_corpus <- tm_map(clean_corpus, stripWhitespace)

> clean_corpus <- tm_map(clean_corpus, PlainTextDocument)   
> sms_dtm <- DocumentTermMatrix(clean_corpus)
> spam_indices <- which(sms_spam_df$type == "spam")
> ham_indices <- which(sms_spam_df$type == "ham")

> library(wordcloud)
> wordcloud(clean_corpus[ham_indices], min.freq=40)
> wordcloud(clean_corpus[spam_indices], min.freq=40)

> sms_raw_train <- sms_spam_df[1:4169,]
> sms_raw_test <- sms_spam_df[4170:5559,]
> sms_dtm_train <- sms_dtm[1:4169,]
> sms_dtm_test <- sms_dtm[4170:5559,]
> sms_corpus_train <- clean_corpus[1:4169]
> sms_corpus_test <- clean_corpus[4170:5559]

> spam <- subset(sms_raw_train, type == "spam")
> ham <- subset(sms_raw_train, type == "ham")
> five_times_words <- findFreqTerms(sms_dtm_train, 5)

> sms_train <- DocumentTermMatrix(sms_corpus_train, control=list(dictionary = five_times_words))
> sms_test <- DocumentTermMatrix(sms_corpus_test, control=list(dictionary = five_times_words))

> convert_count <- function(x) 
+ {
+   y <- ifelse(x > 0, 1,0)
+   y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
+   y
+ }
> sms_train <- apply(sms_train, 2, convert_count)
> sms_test <- apply(sms_test, 2, convert_count)

> library(e1071)
> sms_classifier <- naiveBayes(sms_train, factor(sms_raw_train$type))
> sms_test_pred <- predict(sms_classifier, newdata=sms_test)
> table(sms_test_pred, sms_raw_test$type)
             
# sms_test_pred  ham  spam
#     ham       1202    31
#    spam         5    152
 
