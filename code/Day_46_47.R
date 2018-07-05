library(tibble)
library(readr)
library(tidyverse)
download.file('https://royallib.com/get/txt/Townsend_Sue/The_Secret_Diary_of_Adrian_Mole_Aged_13_34.zip',
              destfile = 'data/adrian.zip')
zipF<-file.choose()
outDir<-getwd()
unzip(zipF,exdir=outDir)
adrian <- read_lines(file.choose(), skip = 11)
adrian <- tibble(text = adrian)
x <- adrian %>% 
  mutate(len = unlist(map(str_extract_all(text, boundary('word')), length)),
         digit = str_detect(text, '[:digit:]$'))
pos <- as.numeric(rownames(x)[which(x$len == 3 & x$digit == TRUE)])
value_date <- x %>% 
  filter(len == 3, digit == TRUE)
date_list <- value_date$text %>% 
  str_subset('[:digit:]$') %>% 
  str_extract_all(boundary('word'))
year_rep <- rep(c(1981, 1982), c(369, 93))
library(lubridate)
value_date <- date_list %>% 
  map(function(x) paste(x[2], x[3])) %>%
  unlist() %>% 
  paste(year_rep) %>% 
  parse_date_time('%b %d %Y')
value_weekday <- date_list %>% 
  map(function(x) x[1]) %>%
  unlist()

x$date <- as.Date(NA)
x$date[pos] <- value_date
x$weekday <- as.character(NA)
x$weekday[pos] <- value_weekday
x$weekday[c(2431, 2433, 2439, 2443)] <- NA
x$text[pos] <- ""
x <-  x %>% 
  select(-len, -digit)

wday_sort <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
df <- x %>%
  fill(date, weekday) %>%
  group_by(date, weekday) %>% 
  summarise(senten = paste(text, collapse = ' ')) %>% 
  ungroup() %>% 
  mutate(weekday = factor(weekday, levels = wday_sort))
df$senten <- df$senten %>% 
  iconv("UTF-8", "ASCII", "'")
head(df)
library(tidytext)
df$senten <- df$senten %>% 
  str_replace_all(pattern = "[[:punct:]]|[[:digit:]]", replacement = "") %>%
  tolower()
df <- df %>% 
  filter(!is.na(date))
df_tidy <- df %>% 
  unnest_tokens(word, senten) %>%
  filter(!nchar(word) < 3) %>%
  anti_join(stop_words)

df_tidy %>% 
  group_by(weekday) %>% 
  ggplot(aes(x = weekday, fill = weekday)) + geom_bar() + coord_flip()
word_len <- df_tidy %>% 
  group_by(weekday) %>% 
  count(word, sort = T) %>% 
  arrange(desc(n)) %>%
  slice(seq_len(7)) %>%
  ungroup() %>% 
  arrange(weekday, n) %>%
  mutate(row = row_number())

ggplot(word_len, aes(x = row, y = n, fill = weekday)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~weekday, scales = "free_y") + 
  coord_flip() +
  scale_x_continuous(breaks = word_len$row, labels = word_len$word) +
  xlab(NULL) + 
  ylab(NULL)

#datacamp code
df_bigrams <- df %>% 
  unnest_tokens(bigram, senten, token = "ngrams", n = 2) %>% 
  mutate(month = month(date),
         decade = case_when(
           month == 1 | month == 2 | month == 12 ~ 'winter',
           month >= 3 & month <= 5 ~ 'spring',
           month >= 6 & month <= 8 ~ 'summer',
           month >= 9 & month <= 11 ~ 'fall'
         )
  )

df_separated <- df_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

df_filtered <- df_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

df_decade <- df_filtered %>%
  filter(word1 != word2) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, decade, sort = TRUE) %>%
  group_by(decade) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())

df_decade %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~decade, scales = "free_y") +
  coord_flip() +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(  # This handles replacement of row
    breaks = df_decade$row, # Notice need to reuse data frame
    labels = df_decade$bigram)

#sentiments
afinn_pos_words <- get_sentiments('afinn') %>% 
  filter(score > 0) %>%
  select(word) %>%
  unlist %>%
  as.character()

afinn_neg_words <- get_sentiments('afinn') %>% 
  filter(score < 0) %>%
  select(word) %>%
  unlist %>%
  as.character()

bing_pos_words <- get_sentiments('bing') %>%
  filter(sentiment == 'positive') %>%
  select(word) %>%
  unlist %>%
  as.character()

bing_neg_words <- get_sentiments('bing') %>%
  filter(sentiment == 'negative') %>%
  select(word) %>%
  unlist %>%
  as.character()

pos_words <- union(afinn_pos_words, bing_pos_words)
neg_words <- union(afinn_neg_words, bing_neg_words)

df_tidy %>% 
  group_by(date) %>% 
  summarise(value = sum(word %in% pos_words) - sum(word %in% neg_words)) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line() + 
  geom_hline(yintercept = 0, col = 'red') + 
  geom_smooth(se = F) +
  scale_x_date(breaks = seq(min(df_tidy$date), max(df_tidy$date), 120))

pos_col <- RColorBrewer::brewer.pal(name = 'Dark2', 3)[1]
neg_col <- RColorBrewer::brewer.pal(name = 'Set1', 3)[1]
df_tidy_sentiment <- df_tidy %>% 
  group_by(date, weekday) %>% 
  summarise(value_pos = sum(word %in% pos_words),
            value_neg = sum(word %in% neg_words)) 

p <-  ggplot(df_tidy_sentiment, aes(x = date)) + 
  geom_line(aes(y = value_pos, colour = pos_col)) + 
  geom_line(aes(y = value_neg, colour = neg_col)) + 
  geom_smooth(aes(y = value_pos, colour = pos_col), se = F) + 
  geom_smooth(aes(y = value_neg, colour = neg_col), se = F) + 
  scale_colour_manual("Value", 
                      labels = c('Positive', 'Negative'),
                      values = c(pos_col, neg_col)) +
  ylab('n words')
p

df_tidy %>% 
  group_by(word) %>% 
  inner_join(tibble(word = pos_words)) %>% 
  count(word, sort = T)

df_tidy %>% 
  group_by(word) %>% 
  inner_join(tibble(word = neg_words)) %>% 
  count(word, sort = T)