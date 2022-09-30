#package installation 
packages <- c("rtweet", "tm", "stringr", "SnowballC", "lsa", "dplyr", "xlsx", "readxl", "quanteda", "quanteda.corpora", 
              "lubridate", "ggplot2", "table1", "wordcloud", "ngram", "tidytext")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN',dependencies=T)
  }
}

#load data
setwd("/Users/erinrogers/Library/CloudStorage/OneDrive-EmoryUniversity/Desktop/Fall 2022/TADA/")
dat1 <- read.csv(file = "TADA_A1_set1.csv")
dat2 <- read.csv(file = "TADA_A1_set2.csv")


#1
  #what are the date ranges for the two sets? 
dat1$a <- as.Date(dat1$date,format="%m/%d/%Y")
range(dat1$a)

dat2$a <- as.Date(dat2$date,format="%m/%d/%Y")
range(dat2$a)

  #what information are provided in the CSV files? 
ls(dat1)
ls(dat2)

  #what languages in which tweets have been posted?
ggplot(data = dat1) +
  geom_bar(mapping = aes(x = lang))

ggplot(data = dat2) +
  geom_bar(mapping = aes(x = lang))

dat1 %>%
  count(lang) %>%
  mutate(percent = n / sum(n) * 100)

dat2 %>%
  count(lang) %>%
  mutate(percent = n / sum(n) * 100)

#subset to english only tweets
dat1 <- dat1 %>% filter(lang == "en")
dat2 <- dat2 %>% filter(lang == "en")

#testing on first 500 observations [remove when running program on full data sets]
  #dat1 <- dat1[1:500, ]
  #dat2 <- dat2[1:500, ]

full_dat <- rbind(dat1, dat2)
#3. How many tweets are there for methadone, Suboxone and fentanyl? (sometimes alternative expressions are used for substances)
full_tweets <- data.frame(
  id = 1:128724, #change to number of obs
  text = full_dat$date.1,
  date = full_dat$date
)

#full tweets contains all english-language tweets

tweets <- data.frame(
  id = 1:66592, #change to number of obs
  text = dat1$date.1,
  date = dat1$date
)
#tweets contains all english-language tweets in first time series

tweets_2 <- data.frame(
  id = 1:62132, #change to number of obs
  text = dat2$date.1,
  date = dat2$date
)
#tweet_2 contains all english-language tweets in second time series

# Fentanyl
fentanyl <- data.frame(word = c("fent", "fentanyl", "apace", "china girl", "china town", "china white", "dance fever", "goodfellas", "great bear", "he-man", "tango"))

# Full Data
in_lexicon <- full_tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% fentanyl$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

sub <- in_lexicon %>% filter(words_in_lexicon >= 1)
fentanyl <- left_join(sub, full_tweets, by="id") #now we have a data set that contains all tweets that mention fentanyl

# Data set 1
fentanyl <- data.frame(word = c("fent", "fentanyl", "apace", "china girl", "china town", "china white", "dance fever", "goodfellas", "great bear", "he-man", "tango"))
in_lexicon <- tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% fentanyl$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))
sub <- in_lexicon %>% filter(words_in_lexicon >=1 )
fentanyl_pre <- left_join(sub, tweets, by="id")

table(in_lexicon$words_in_lexicon) #count number of words

# Data set 2
in_lexicon <- tweets_2 %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% fentanyl$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

table(in_lexicon$words_in_lexicon) #count number of words

# Methadone
methadone <- data.frame(word = c("methadone", "dolophine", "amidone", "chocolate chip cookies", "fizzes with MDMA", "wafer"))

# Full Data
in_lexicon <- full_tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% methadone$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

sub <- in_lexicon %>% filter(words_in_lexicon >= 1)
methadone <- left_join(sub, full_tweets, by="id") #now we have a data set that contains all tweets that mention methadone

# Data set 1
methadone <- data.frame(word = c("methadone", "dolophine", "amidone", "chocolate chip cookies", "fizzes with MDMA", "wafer"))
in_lexicon <- tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% methadone$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

sub <- in_lexicon %>% filter(words_in_lexicon >= 1)
methadone_pre <- left_join(sub, tweets, by="id") #now we have a data set that contains all tweets that mention methadone

table(in_lexicon$words_in_lexicon) #count number of words

# Data Set 2
in_lexicon <- tweets_2 %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% methadone$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

table(in_lexicon$words_in_lexicon) #count number of words

# Suboxone
suboxone <- data.frame(word = c("suboxone", "subs", "buprenorphine", "bups", "bupes", "sobos", "stop signs", "stops"))

# Full Data
in_lexicon <- full_tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% suboxone$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

sub <- in_lexicon %>% filter(words_in_lexicon >= 1)
suboxone <- left_join(sub, full_tweets, by="id")

suboxone <- data.frame(word = c("suboxone", "subs", "buprenorphine", "bups", "bupes", "sobos", "stop signs", "stops"))
in_lexicon <- tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% suboxone$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

sub <- in_lexicon %>% filter(words_in_lexicon >= 1)
suboxone_pre <- left_join(sub, tweets, by="id")

# Data Set 1
in_lexicon <- tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% suboxone$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

table(in_lexicon$words_in_lexicon) #count number of words

# Data Set 2
in_lexicon <- tweets_2 %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% suboxone$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

table(in_lexicon$words_in_lexicon) #count number of words

#4. Are there fentanyl analogs that are also being discussed (eg., carfentanil)?
analogs <- data.frame(word = c("acetylfentanyl", "furanylfentanyl", "carfentanil"))

in_lexicon <- tweets %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% analogs$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

table(in_lexicon$words_in_lexicon) #count number of words

# Data Set 2
in_lexicon <- tweets_2 %>%
  tidytext::unnest_tokens(output = 'words', input = text) %>%
  dplyr::mutate(
    in_lexicon = words %in% analogs$word
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(words_in_lexicon = sum(in_lexicon))

table(in_lexicon$words_in_lexicon) #count number of words

#5. What are some of the topics that are most closely associated with each of the three substances? Top 5-10 topics (if relevant)
Textprocessing <- function(x)
{gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}

#Fentanyl
fentanyl_tweets <- fentanyl_pre$text
fentanyl_corp <- Corpus(VectorSource(fentanyl_tweets))
fentanyl_corp <- tm_map(fentanyl_corp, content_transformer(tolower)) 
fentanyl_corp <- tm_map(fentanyl_corp, removeWords, stopwords("en"))
fentanyl_corp <- tm_map(fentanyl_corp, removePunctuation)
fentanyl_corp <- tm_map(fentanyl_corp, stemDocument)
fentanyl_corp <- tm_map(fentanyl_corp,Textprocessing)
doc_term_matrix_fentanyl <- DocumentTermMatrix(fentanyl_corp)

#find word associations
findAssocs(doc_term_matrix_fentanyl, "fentanyl", 0.50) 
inspect(doc_term_matrix_fentanyl)

#word cloud
stopwords_regex = paste(rev(stopwords('en')), collapse = '\\b|\\b')
stopwords_regex <- paste(stopwords(), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
fentanyl_doc <- paste(c(fentanyl_corp))
fentanyl_corp_wc <- stringr::str_replace_all(fentanyl_doc, stopwords_regex, '')

# Methadone
methadone_tweets <- methadone_pre$text
methadone_corp <- Corpus(VectorSource(methadone_tweets))
methadone_corp <- tm_map(methadone_corp, content_transformer(tolower)) 
methadone_corp <- tm_map(methadone_corp, removeWords, stopwords("en"))
methadone_corp <- tm_map(methadone_corp, removePunctuation)
methadone_corp <- tm_map(methadone_corp, removeNumbers)
methadone_corp <- tm_map(methadone_corp, stemDocument)
methadone_corp <- tm_map(methadone_corp,Textprocessing)
doc_term_matrix_methadone <- DocumentTermMatrix(methadone_corp)
findAssocs(doc_term_matrix_methadone, "methadone", 0.1) 
inspect(doc_term_matrix_methadone)

#Suboxone 
suboxone_tweets <- suboxone_pre$text
suboxone_corp <- Corpus(VectorSource(suboxone_tweets))
suboxone_corp <- tm_map(suboxone_corp,Textprocessing)
suboxone_corp <- tm_map(suboxone_corp,content_transformer(tolower))
suboxone_corp <- tm_map(suboxone_corp,removeNumbers)
suboxone_corp <- tm_map(suboxone_corp,removePunctuation)
suboxone_corp <- tm_map(suboxone_corp, stemDocument)
doc_term_matrix_suboxone <- DocumentTermMatrix(suboxone_corp)
findAssocs(doc_term_matrix_suboxone, "suboxone", 0.1) 
inspect(doc_term_matrix_suboxone)

#6  Generate word clouds for each set, so they can be shown to the researcher
wc1 <- wordcloud(fentanyl_corp, min.freq = 20, max.words = 100, scale = c(2, .25), random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))
wc2 <- wordcloud(methadone_corp, min.freq = 20, max.words = 100, scale = c(2, .25), random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))
wc3 <- wordcloud(suboxone_corp, min.freq = 20, max.words = 100, scale = c(2, .25), random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))

#7. Generate appropriate time-series figures to compare how the frequencies of mentions of these substances differ
# ggplot with a line for fentanyl, methadone and suboxone mentions over time
time_fentanyl <- fentanyl %>% count(date) %>% mutate(date2 = as.Date(date), format="%m/%d/%Y")
time_fentanyl$date2 <- as.Date(time_fentanyl$date,format="%m/%d/%Y")
p1 <- ggplot(time_fentanyl, aes(x=date2, y=n)) + geom_point(na.rm=TRUE, color = "darkgoldenrod2", size=3, pch=18) + scale_x_date(name = 'Dates', breaks = '1 month') + scale_y_continuous(name = "Number of Tweets")
p1 + geom_smooth(color="darkgoldenrod2") + ggtitle("Frequency of Tweets Mentioning Fentanyl \n Before and After COVID-19 Emergence")

time_suboxone <- suboxone %>% count(date)
time_suboxone$date2 <- as.Date(time_suboxone$date, format="%m/%d/%Y")
p2 <- ggplot(time_suboxone, aes(x=date2, y=n)) + geom_point(na.rm=TRUE, color = "darkseagreen", size=3, pch=18) + scale_x_date(name = 'Dates', breaks = '1 month') + scale_y_continuous(name = "Number of Tweets")
p2 + geom_smooth(color="darkseagreen") + ggtitle("Frequency of Tweets Mentioning Suboxone \n Before and After COVID-19 Emergence")

time_methadone <- methadone %>% count(date)
time_methadone$date2 <- as.Date(time_methadone$date, format="%m/%d/%Y")
p3 <- ggplot(time_methadone, aes(x=date2, y=n)) + geom_point(na.rm=TRUE, color = "coral1", size=3, pch=18) + scale_x_date(name = 'Dates', breaks = '1 month') + scale_y_continuous(name = "Number of Tweets")
p3 + geom_smooth(color="coral1") + ggtitle("Frequency of Tweets Mentioning Methadone \n Before and After COVID-19 Emergence")

#8. Find the top 10 most frequent bigrams in each of the three sets. Plot a bar chart for these.
methadone_tweet_df <- data.frame(text = sapply(methadone_corp, as.character), stringsAsFactors = FALSE)
methadone_bigrams <- methadone_tweet_df %>% unnest_tokens(bigrams, text, token="ngrams", n=2)
methadone_bigrams <- methadone_bigrams %>% count(bigrams) %>% 
  mutate(prop = n/sum(n)) %>% arrange(desc(prop)) %>% mutate(id = row_number()) %>% filter(id %in% c(1, 3, 4, 7, 8, 9, 10, 11, 12, 13))

p4 <- ggplot(methadone_bigrams, aes(x=reorder(bigrams, -prop), y = prop)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Bigrams') + scale_y_continuous(name = "Proportion of Bigrams") +
  ggtitle("Top 10 Bigrams Identified in Tweets Mentioning Methadone")
p4

fentanyl_tweet_df <- data.frame(text = sapply(fentanyl_corp, as.character), stringsAsFactors = FALSE)
fentanyl_bigrams <- fentanyl_tweet_df %>% unnest_tokens(bigrams, text, token="ngrams", n=2)
fentanyl_bigrams <- fentanyl_bigrams %>% count(bigrams) %>% 
  mutate(prop = n/sum(n)) %>% arrange(desc(prop)) %>% mutate(id = row_number()) %>% filter(id %in% c(2, 3, 4, 5, 6, 9, 10, 11, 12, 14))

p5 <- ggplot(fentanyl_bigrams, aes(x=reorder(bigrams, -prop), y = prop)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Bigrams') + scale_y_continuous(name = "Proportion of Bigrams") +
  ggtitle("Top 10 Bigrams Identified in Tweets Mentioning Fentanyl")
p5

suboxone_tweet_df <- data.frame(text = sapply(suboxone_corp, as.character), stringsAsFactors = FALSE)
suboxone_bigrams <- suboxone_tweet_df %>% unnest_tokens(bigrams, text, token="ngrams", n=2)
suboxone_bigrams <- suboxone_bigrams %>% count(bigrams) %>% mutate(prop = n/sum(n)) %>% arrange(desc(prop)) %>% mutate(id = row_number()) %>% filter(id %in% c(4, 5, 6, 19, 21, 23, 24, 27, 35, 45))


p6 <- ggplot(suboxone_bigrams, aes(x=reorder(bigrams, -prop), y = prop)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Bigrams') + scale_y_continuous(name = "Proportion of Bigrams") +
  ggtitle("Top 10 Bigrams Identified in Tweets Mentioning Suboxone")
p6
