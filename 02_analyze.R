library(tidyverse)
library(tidytext)
library(lubridate)

rm(list = ls())
links <- read_csv('links.csv')

# stories <- read_csv("http://varianceexplained.org/files/stories_1000000.csv.gz") %>%
#   mutate(time = as.POSIXct(time, origin = "1970-01-01"),
#          month = round_date(time, "month"))

links <- links %>% 
  mutate(time = as.POSIXct(issue, origin = "1970-01-01"),
                 month = round_date(time, "month"))

stop_words_added <- tibble(word = c('e2'), lexicon = rep('temp',1))
stop_words <- bind_rows(stop_words, stop_words_added)
  
links_words <- links %>%
  distinct(title, .keep_all = TRUE) %>%
  unnest_tokens(word, title, drop = FALSE) %>%
  distinct(title, word, .keep_all = TRUE)  %>%
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word, "[^\\d]")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()
  

word_counts <- links_words %>%
  count(word, sort = TRUE)

word_counts %>%
  head(93) %>%
  slice(-c(1:3)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "lightblue") +
  scale_y_continuous() +
  coord_flip() +
  labs(title = "Most common words in R Weekly titles",
       subtitle = "From May 2016 to August 2017",
       y = "# of uses")

stories_per_month <- stories %>%
  group_by(month) %>%
  summarize(month_total = n())

links_per_month <- links %>%
  group_by(month) %>%
  summarize(month_total = n())
  
  
word_month_counts <- links_words %>%
  filter(word_total >= 15, word_total < 70) %>%
  count(word, month) %>%
  complete(word, month, fill = list(n = 0)) %>%
  inner_join(links_per_month, by = "month") %>%
  mutate(percent = n / month_total) %>%
  mutate(year = year(month) + yday(month) / 365)

library(broom)

mod <- ~ glm(cbind(n, month_total - n) ~ year, ., family = "binomial")

slopes <- word_month_counts %>%
  nest(-word) %>%
  mutate(model = map(data, mod)) %>%
  unnest(map(model, tidy)) %>%
  filter(term == "year") %>%
  arrange(desc(estimate))

slopes <- left_join(slopes,word_counts)
write_csv(slopes, 'slopes.csv')

ggplot(slopes, aes(statistic,estimate)) + 
  geom_point(aes(colour = estimate, size= n), alpha = 0.5) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  coord_flip() +
  ggrepel::geom_text_repel(aes(label = word, size = n), data = slopes)
