library(tidyverse)
library(assertthat)

all_lines <- list.files(path = 'md-to-csv-files',
                        full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

links <- all_lines %>% 
  filter(type == 'link') %>%
  select(-section_lookup, -type) %>%
  na.omit() 

link_title = str_locate_all(pattern ='\\[.+\\]', links$value)
link_url = str_locate(pattern ='\\(http.*?\\)', links$value) 

assert_that(length(link_title) == dim(links)[1], 
            msg = "You have invalid links in rmarkdown files!")

assert_that(length(unique(do.call(rbind, link_title)[,1])) == 1,
            msg = "You have link titles starting in different positions")

assert_that(dim(link_url)[1] == dim(links)[1],
            msg = "You have invalid links in rmarkdown files!")

title1 <- sapply(link_title,'[[',1)
title2 <- sapply(link_title,'[[',2)

url1 <- link_url[,1]
url2 <- link_url[,2]

link_positions <- tibble(title1, title2, url1, url2)
links <- bind_cols(links, link_positions)

links <- links %>%
  mutate(section_title = gsub('#', '', section_title),
    title = substr(value, 4, title2 - 1),
    url = substr(value, url1+1, url2 -1),
    description = substr(value, url2+4, nchar),
    nchar = nchar(title),
    title_c = gsub('\\(http.*','',title),
    title = gsub('\\]','',title_c),
    nchar1 = nchar(title_c),
    issue = lubridate::ymd(issue)) %>%
  select(issue,section_title,title,url,description)

links <- links %>%
  distinct(title, url, .keep_all = TRUE)

#### section_titleS
links$section_title
links$section_title <- trimws(links$section_title)
links$description <- trimws(links$description)

section_titles <- count(links, section_title)

# ---- fixing different names for the same section_titles

links <- links %>%
  mutate(section_title = recode(section_title,
                            'R in Real World' = 'R in the Real World',
                            'R in the Real World' = 'R in the Real World',
                            'R in Organization' = 'R in Organizations',
                            'R in Organizations' = 'R in Organizations',
                            'International R' = 'R Internationally',
                            'New Releases' = 'Package Releases',
                            'Package Releases' = 'Package Releases',
                            'New Packages & Tools' = 'New Packages',
                            'New Packages and Tools' = 'New Packages',
                            'Video and Podcast' = 'Videos and Podcasts',
                            'Videos & Podcasts' = 'Videos and Podcasts',
                            'Job' = 'Jobs',
                            'R OpenSci Unconference 2017' = 'Events',
                            'Live in rstudio::conf 2017' = 'Events',
                            'UseR! 2017' = 'Events',
                            'Call for Participation' = 'Events',
                            'Upcoming Events' = 'Events',
                            'R in Bioinformatics' = 'R in the Real World',
                            'Teaching' = 'Tutorials',
                            'R Project Updates' = '(misc)',
                            'Packages Development' = '(misc)',
                            'Blog Posts' = '(misc)',
                            'Web' = '(misc)',
                            'Visualization' = '(misc)',
                            'R & Other Languages' = '(misc)',
                            'Tools' = '(misc)',
                            'Gist & Cookbook' = '(misc)',
                            'R for Fun' = '(misc)',
                            'Data' = '(misc)',
                            'Machine Learning and Statistics' = '(misc)',
                            .missing = section_title
  ))


links$section_title <- str_replace(links$section_title, 
                             pattern = '.R in Academia', 
                             replacement = 'R in Academia')

links$section_title <- str_replace(links$section_title, 
                             pattern = '.*Election.*', 
                             replacement = '(misc)')

section_titles <- count(links, section_title)

## to extract url_base, I borrowed the idea from Josh O'Brien
## https://stackoverflow.com/questions/15909626/regex-grab-from-beginning-to-n-occurrence-of-character

matchToNth <- function(char, n) { 
  others <- paste0("[^", char, "]*") 
  mainPat <- paste0(c(rep(c(others, char), n - 1), others), collapse = "")
  paste0("(^", mainPat, ")", "(.*$)")
}

glimpse(links)

links <- links %>%
  mutate(url_base = str_replace(url, matchToNth("/", 3), '\\1'),
         url_base = str_replace(url_base,  'https?://', ''),
         url_base = str_to_lower(url_base))

url_bases <- count(links, url_base) %>%
  arrange(desc(n)) %>%
  rename(N = n)

section_title_url <- links %>%
  count(section_title, url_base) %>%
  group_by(url_base) %>%
  mutate(prop = prop.table(n)) %>%
  left_join(url_bases, by = 'url_base') %>%
  arrange(desc(N),desc(n))

tutorials        <- filter(links, section_title == 'Tutorials')
real_world       <- filter(links, section_title == 'R in the Real World')
new_packages     <- filter(links, section_title == 'New Packages')
package_releases <- filter(links, section_title == 'Package Releases')
organizations    <- filter(links, section_title == 'R in Organizations')
highlight        <- filter(links, section_title == 'Highlight')
insights         <- filter(links, section_title == 'Insights')
resources        <- filter(links, section_title == 'Resources')
videos_podcasts  <- filter(links, section_title == 'Videos % Podcasts')

highquality <- filter(links, section_title %in% c('Highlight','Insights')) %>%
  group_by(url_base, N) %>%
  summarise(n = n()) %>%
  mutate(ratio = n/N) %>%
  mutate(label = ifelse(n > 1 , as.character(url_base),'')) %>%
  arrange(desc(ratio)) %>%
  ungroup()

write_csv(links,'links.csv')
write_excel_csv(links, 'links.csv')


