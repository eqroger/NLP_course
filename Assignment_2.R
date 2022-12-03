###################################################################################################################
#
# Purpose: Assignment 2 for TADA NLP Course
#
# Authored by: Erin Rogers
#
# Last Update: 30 NOV 2022
#
###################################################################################################################

packages <- c("dplyr", "caTools", "rfUtilities", "tm", "mlapi", "e1071", "origami", "caret", 
              "mltest", "nnet", "MLmetrics", "tidyr", "SnowballC", "wordcloud", "tidytext", "ggeasy")
for (package in packages) {
  library(package, character.only=T)
}

# Read in data ------------------------------------------------------------
setwd("/Users/erinrogers/Library/CloudStorage/OneDrive-EmoryUniversity/Desktop/Fall 2022/TADA/")
full_dataset <- read.csv("TADA_annotated_data.csv")
unlabeled_data <- read.csv("TADA_unlabeled_data.csv")
glimpse(full_dataset)

table(full_dataset$class)
# Text Preprocessing ------------------------------------------------------------
all_texts <-full_dataset$text
all_texts_corpus <- VCorpus(VectorSource(all_texts))
#all_texts_corpus <- tm_map(all_texts_corpus, content_transformer(tolower))
all_texts_corpus <- tm_map(all_texts_corpus, removePunctuation)
all_texts_corpus <- tm_map(all_texts_corpus, removeWords,stopwords("english"))
all_texts_corpus <- tm_map(all_texts_corpus, stemDocument)
length(all_texts_corpus)

# Generate N-Grams ------------------------------------------------------------
NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 1:3), paste, collapse = "_"), use.names = FALSE)
}
n_gram_corpus <- tm_map(all_texts_corpus,content_transformer(NLP_tokenizer))

length(n_gram_corpus)
n_gram_corpus[[2]]$content

# Split the data (80/20) ------------------------------------------------------------
set.seed(1234)
split <- sample.split(full_dataset$class,SplitRatio = 0.8)
training_ngram_corpus <- subset(n_gram_corpus, split==TRUE)
eval_ngram_corpus <- subset(n_gram_corpus, split==FALSE)
training_classes <- subset(full_dataset$class, split==TRUE)
eval_classes <- subset(full_dataset$class, split==FALSE)

# Vectorize ------------------------------------------------------------
training_dct_matrix <- DocumentTermMatrix(training_ngram_corpus)
training_dct_matrix_sparse <- removeSparseTerms(training_dct_matrix,0.995)

eval_dct_matrix_sparse <- DocumentTermMatrix(eval_ngram_corpus, list(dictionary=colnames(training_dct_matrix_sparse)))

training_term_matrix_df <- as.data.frame(as.matrix(training_dct_matrix_sparse))
eval_term_matrix_df <- as.data.frame(as.matrix(eval_dct_matrix_sparse))
colnames(training_term_matrix_df) <- make.names(colnames(training_term_matrix_df))
colnames(eval_term_matrix_df) <- make.names(colnames(eval_term_matrix_df))
training_term_matrix_df$class <- training_classes
training_term_matrix_df$class <- as.factor(training_term_matrix_df$class)

# SVM (Radial)  ------------------------------------------------------------
trained_model <- svm(class ~., data=training_term_matrix_df, kernel="radial") 

svm_predictions <- predict(trained_model, newdata=eval_term_matrix_df)

svm_output <- ml_test(svm_predictions, eval_classes, output.as.table=TRUE)
svm_output

# SVM (Linear)
trained_model <- svm(class ~., data=training_term_matrix_df, kernel="linear") 

svm_predictions <- predict(trained_model, newdata=eval_term_matrix_df)

svm_output <- ml_test(svm_predictions, eval_classes, output.as.table=TRUE)
svm_output

# Multinomial Logistic Regression
multi_train <- nnet::multinom(class ~ ., data = training_term_matrix_df, MaxNWts = 10000)
predicted_multi <- multi_train %>% 
  predict(eval_term_matrix_df)
multi_model_output <- mltest::ml_test(predicted_multi,eval_classes, output.as.table = TRUE)
multi_model_output

# Naive Bayes
nb_train <- naiveBayes(class ~ ., data = training_term_matrix_df, laplace = 3)
predict_nb <- predict(nb_train, newdata = eval_term_matrix_df)
nb_output <- ml_test(predict_nb, eval_classes, output.as.table=TRUE)
nb_output

# Random  Forest
rf_train <- randomForest(class ~ ., data=training_term_matrix_df)
predict_rf <- predict(rf_train, newdata = eval_term_matrix_df)
rf_output <- ml_test(predict_rf, eval_classes, output.as.table=TRUE)
rf_output

# Cross Validations  ------------------------------------------------------------
kfoldcv <- trainControl(method="cv", number=10, classProbs = TRUE)
performance_metric <- "Accuracy"

#Classification & Regression Trees (CART)
cart.train <- train(class~., data=training_term_matrix_df, method="rpart",
                   trControl=kfoldcv, preProcess=c("center", "scale"), tuneLength=5)

#Support Vector Machines (SVM)
svm.train <- train(class~., data=training_term_matrix_df, method="svmRadial", metric=performance_metric,
                   trControl=kfoldcv, preProcess=c("center", "scale"), tuneLength=5)

#trained_model <- train(class~., training_term_matrix_df, method="rf")

#Gradient Boosting
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
nrow(gbmGrid)

gbm.train <- train(class ~ ., data = training_term_matrix_df, method = "gbm", trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid)

#Accuracy Summary
results.train <- resamples(list(lda=lda.train, cart=cart.train, svm=svm.train))
summary(results.train)

#unlabeled data 
unlabeled_clean <- unlabeled_data %>% 
  select(text,gender_id, city) %>% 
  mutate(id = row_number())

#preprocessing of the unlabeled text

unlabeled_text <- unlabeled_clean$text

corpus <- VCorpus(VectorSource(unlabeled_text))

corpus <- corpus %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>% 
  tm_map(removeWords,stopwords("en")) %>% 
  tm_map(stemDocument)

length(corpus)

NLP_tokenizer <- function(x){
  unlist(lapply(ngrams(words(x),1:3),paste,collapse = "_"),use.names=FALSE)
}

n_gram_corpus <- tm_map(corpus,content_transformer(NLP_tokenizer))
length(n_gram_corpus)
n_gram_corpus[[2]]$content

dct_matrix <- DocumentTermMatrix(n_gram_corpus, list(dictionary=colnames(training_dct_matrix_sparse)))

dct_matrix_df <- as.data.frame(as.matrix(dct_matrix))

colnames(dct_matrix_df) <- make.names(colnames(dct_matrix_df))

#run train svm (with linear kernel) on external data
svm_lin_prediction <- predict(trained_model, newdata=dct_matrix_df)

unlabeled_clean$label <- svm_lin_prediction
summarize_by_city <- unlabeled_clean %>% 
  group_by(city) %>% 
  summarise(nonmedical = sum(label == 0)) %>% 
  mutate(population = case_when(city == "A" ~ 500000,
                                city == "B" ~ 10000),
         pop_adjusted_reports = nonmedical/population)

summarize_by_city_2 <- unlabeled_clean %>%
  group_by(city) %>%
  summarise(nonmedical = sum(label == 0),
            consumption = sum(label == 1),
            information = sum(label == 2),
            unrelated = sum(label == 3)) 

city <- c("A", "B", "A", "B", "A", "B", "A", "B")
type <- c("nonmedical", "nonmedical", "consumption", "consumption", "information", "information", "unrelated", "unrelated")
n <- c(1249/500000, 260/10000, 4036/500000, 889/10000, 6288/500000, 1294/10000, 816/500000, 168/10000)
n_adjust <- n*1000
df_city <- data.frame(city, type, n, n_adjust)

c <- ggplot(df_city, mapping=aes(x=as.factor(type), y=n_adjust, fill=city)) +
  geom_bar(stat="identity", position=position_dodge(0.75), width=0.75) + 
  geom_text(aes(label=n_adjust), vjust=-0.5, position=position_dodge(0.75)) +
  ggtitle("Figure 3. Population-Adjusted Rates By City") +
  xlab("Class") +
  ylab("Rate per 1,000") +
  theme(axis.text.x = element_text(hjust=1, angle=45, size=12)) +
  theme(legend.position = "bottom") 

c

#gender summary
summarize_by_gender <- unlabeled_clean %>% 
  group_by(gender_id) %>% 
  summarise(nonmedical = sum(label == 0),
            consumption = sum(label == 1),
            information = sum(label == 2),
            unrelated = sum(label == 3)) 


gender <- c("f", "m", "f", "m", "f", "m", "f", "m")
type <- c("nonmedical", "nonmedical", "consumption", "consumption", "information", "information", "unrelated", "unrelated")
n <- c(732, 777, 2456, 2469, 3726, 3856, 480, 504)

df <- data.frame(gender, type, n)

#gender identity analysis
g <- ggplot(df, mapping=aes(x=as.factor(type), y=n, fill=gender)) +
  geom_bar(stat="identity", position=position_dodge(0.75), width=0.75) + 
  geom_text(aes(label=n), vjust=-0.5, position=position_dodge(0.75)) +
  ggtitle("Figure 1. Count of Tweet Classes by Gender") +
  xlab("Class") +
  ylab("Count") +
  theme(axis.text.x = element_text(hjust=1, angle=45, size=12)) +
  theme(legend.position = "bottom") 

g

#subset to just nonmedical use
nonmed <- unlabeled_clean %>% filter(label==0)
female <- nonmed %>% filter(gender_id=="F")
male <- nonmed %>% filter(gender_id=="M")

#processing female data
female_tweets <- female$text
female_corp <- VCorpus(VectorSource(female_tweets))
female_corp <- tm_map(female_corp,content_transformer(tolower))
female_corp <- tm_map(female_corp,removeNumbers)
female_corp <- tm_map(female_corp,removePunctuation)
female_corp <- tm_map(female_corp, stemDocument)
female_corp<- tm_map(female_corp, removeWords,stopwords("english"))

doc_term_matrix_female <- DocumentTermMatrix(female_corp)
wc1 <- wordcloud(female_corp, min.freq = 30, max.words = 100, random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))

female_tweet_df <- data.frame(text = sapply(female_corp, as.character), stringsAsFactors = FALSE)
female_bigrams <- female_tweet_df %>% unnest_tokens(bigrams, text, token="ngrams", n=2)
female_bigrams <- female_bigrams %>% count(bigrams) %>% 
  mutate(prop = n/sum(n)) %>% arrange(desc(prop)) %>% mutate(id = row_number()) 
top_ten_female <- female_bigrams %>% filter(id %in% c(1:5))

f <- ggplot(top_ten_female, aes(x=reorder(bigrams, -n), y = n)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Bigrams') + scale_y_continuous(name = "Count of Bigrams") +
  ggtitle("Top 5 Bigrams Identified in Tweets
          Mentioning Nonmedical Use (Females)") +
  ggeasy::easy_center_title()
f

#processing male data
male_tweets <- male$text
male_corp <- VCorpus(VectorSource(male_tweets))
male_corp <- tm_map(male_corp,content_transformer(tolower))
male_corp <- tm_map(male_corp,removeNumbers)
male_corp <- tm_map(male_corp,removePunctuation)
male_corp <- tm_map(male_corp, stemDocument)
male_corp<- tm_map(male_corp, removeWords,stopwords("english"))

doc_term_matrix_male <- DocumentTermMatrix(male_corp)
wc2 <- wordcloud(male_corp, min.freq = 30, max.words = 100, random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))

male_tweet_df <- data.frame(text = sapply(male_corp, as.character), stringsAsFactors = FALSE)
male_bigrams <- male_tweet_df %>% unnest_tokens(bigrams, text, token="ngrams", n=2)
male_bigrams <- male_bigrams %>% count(bigrams) %>% 
  mutate(prop = n/sum(n)) %>% arrange(desc(prop)) %>% mutate(id = row_number()) 
top_ten_male <- male_bigrams %>% filter(id %in% c(1:5))
        
m <- ggplot(top_ten_male, aes(x=reorder(bigrams, -n), y = n)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Bigrams') + scale_y_continuous(name = "Count of Bigrams") +
  ggtitle("Top 5 Bigrams Identified in Tweets
          Mentioning Nonmedical Use (Males)") +
  ggeasy::easy_center_title()
m

opioids <- data.frame(word = c("hydrocone", "banana", "bananas", "dro", "fluff", "tabs", "norco", "vics", "vikes", "watsons",
                               "oxycodone", "30s", "40s", "beans", "blues", "buttons", "greens", "oc", "oxy", "whites",
                               "percocet", "512s", "blue", "blueberries", "buttons", "percs", "greenies", "kickers", "rims", "tires", "wheels",
                               "dilaudid", "dillies", "dust", "footballs", "juice")) #smack
in_lexicon <- unlabeled_clean %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% opioids$word
  ) %>%
  dplyr::group_by(id)

in_lexicon <- in_lexicon %>% filter(in_lexicon=="TRUE")
city_a <- in_lexicon %>% filter(city == "A")
gender_f <- in_lexicon %>% filter(gender_id == "F")

opioids <- data.frame(word = c("hydrocone", "banana", "bananas", "dro", "fluff", "tabs", "norco", "vics", "vikes", "watsons",
                               "oxycodone", "30s", "40s", "beans", "blues", "buttons", "greens", "oc", "oxy", "whites",
                               "percocet", "512s", "blue", "blueberries", "buttons", "percs", "greenies", "kickers", "rims", "tires", "wheels",
                               "dilaudid", "dillies", "dust", "footballs", "juice", "smack")) 
in_lexicon <- unlabeled_clean %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% opioids$word
  ) %>%
  dplyr::group_by(id)

in_lexicon <- in_lexicon %>% filter(in_lexicon=="TRUE")
city_a <- in_lexicon %>% filter(city == "A")
gender_f <- in_lexicon %>% filter(gender_id == "F")

benzos <- data.frame(word = c("klonopin", "k-pin", "pin",
                              "xanax", "bars", "footballs", "hulk", "ladders", "planks", "sticks", "xanies", "zannies", "z-bars",
                              "benzos", "downers", "zombie pills"))
in_lexicon <- unlabeled_clean %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% benzos$word
  ) %>%
  dplyr::group_by(id)

in_lexicon <- in_lexicon %>% filter(in_lexicon=="TRUE")
city_a <- in_lexicon %>% filter(city == "A")
gender_f <- in_lexicon %>% filter(gender_id == "F")

stimulants <- data.frame(word = c("adderall", "aderal", "adderal", "aderall", "a-train", "abby", "addy",
                                  "amps", "smarties", "zing", "study buddies", "ritalin", "concerta", "vyvanse"))
in_lexicon <- unlabeled_clean %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% stimulants$word
  ) %>%
  dplyr::group_by(id)

in_lexicon <- in_lexicon %>% filter(in_lexicon=="TRUE")
city_a <- in_lexicon %>% filter(city == "A")
gender_f <- in_lexicon %>% filter(gender_id == "F")


  dplyr::summarise(words_in_lexicon = sum(in_lexicon))