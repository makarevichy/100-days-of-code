library(tibble)
library(readr)
library(tidyverse)
download.file('https://royallib.com/get/txt/Townsend_Sue/The_Secret_Diary_of_Adrian_Mole_Aged_13_34.zip',
              destfile = 'adrian.zip')
zipF<-file.choose()
outDir<-"C:\\Users\\user\\Documents"
unzip(zipF,exdir=outDir)
adrian <- read_lines(file.choose(), skip = 11)
adrian <- tibble(text = adrian)
x <- adrian %>% 
  mutate(len = unlist(map(str_extract_all(text, boundary('word')), length)),
         digit = str_detect(text, '[:digit:]$'))
pos <- as.numeric(rownames(x)[which(x$len == 3 & x$digit == TRUE)])
x <- x %>% 
  filter(len == 3)
y <- x$text %>% 
  str_subset('[:digit:]$') %>% 
  str_extract_all(boundary('word'))
z <- rep(c(1981, 1982), c(369, 93))
y <- y %>% 
  map(function(x) paste(x[2], x[3])) %>%
  unlist() %>% 
  paste(z)
y %>% 
  duplicated() %>% 
  sum()
adrian %>% 
  mutate(len = str_detect(text, '[:digit:]$')) %>% 
  filter(len == TRUE, ) %>% 
  View()
