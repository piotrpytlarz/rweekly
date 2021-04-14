library(tidyverse)
library(stringr)
df <-  list.files(path = 'md-to-csv-files', full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

links <- df %>% 
  filter(type == 'link') %>%
  select(value,nchar,section,issue) %>%
  na.omit() 


link_title = str_locate_all(pattern ='\\[.+\\]',links$value)
link_url = str_locate(pattern ='\\(http.*?\\)',links$value) 

if(
  length(link_title) != dim(links)[1]) 
  stop("Error: You have invalid links in rmarkdown files!")

if(
  length(unique(do.call(rbind,link_title)[,1])) != 1)
  stop("Error: You have link titles starting in different positions")

if(
  length(link_url) != dim(links)[1]) 
  stop("Error: You have invalid links in rmarkdown files!")

title1 <- sapply(link_title,'[[',1)
title2 <- sapply(link_title,'[[',2)

# url1 <- sapply(link_url,'[[',1)
# url2 <- sapply(link_url,'[[',2)

url1 <- link_url[,1]
url2 <- link_url[,2]

# link_url <- do.call(rbind,link_url)

link_positions <- tibble(title1,title2,url1,url2)
links <- bind_cols(links, link_positions)

links <- links %>%
  mutate(
    section = gsub('#', '', section),
    title = substr(links$value, 4, links$title2 - 1),
    url = substr(links$value, links$url1+1, links$url2 -1),
    description = substr(links$value, links$url2+4, nchar)
  ) %>%
  select(issue,section,title,url,description)

links <- links %>%
  mutate(nchar = nchar(title),
         title_c = gsub('\\(http.*','',title),
         title = gsub('\\]','',title_c),
         nchar1 = nchar(title_c)) %>%
  mutate(issue = lubridate::ymd(issue)) %>%
  select(issue,section,title,url,description)

links <- links %>%
  distinct(title, url, .keep_all = TRUE)

#### SECTIONS
links$section
links$section <- trimws(links$section)
links$description <- trimws(links$description)

sections <- count(links, section)

# ---- fixing different names for the same sections

links <- links %>%
  mutate(section = recode(section,
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
                            .missing = section
  ))


links$section <- str_replace(links$section, 
                             pattern = '.R in Academia', 
                             replacement = 'R in Academia')

links$section <- str_replace(links$section, 
                             pattern = '.*Election.*', 
                             replacement = '(misc)')



sections <- count(links, section)

# str_extract(links$url, '.*\\/\\/.*[^\\/]*')


## https://stackoverflow.com/questions/15909626/regex-grab-from-beginning-to-n-occurrence-of-character
## Josh O'Brien answer: function
matchToNth <- function(char, n) { 
  others <- paste0("[^", char, "]*") 
  mainPat <- paste0(c(rep(c(others, char), n - 1), others), collapse = "")
  paste0("(^", mainPat, ")", "(.*$)")
}

links$url_base <- str_replace(
  str_replace(links$url, matchToNth("/", 3), '\\1'),
  str_extract(links$url, 'https?://'),
  ''
  )


url_bases <- count(links, url_base) %>%
  arrange(desc(n)) %>%
  rename(N = n)

links <- left_join(links, url_bases)

links$url_base <- str_replace(links$url_base, 
                              pattern = 'shirinG.github.io', 
                              replacement = 'shiring.github.io')

section_url <- links %>%
  count(section, url_base) %>%
  group_by(url_base) %>%
  mutate(prop = prop.table(n)) %>%
  left_join(url_bases, by = 'url_base') %>%
  arrange(desc(N),desc(n))

url_factors <- links %>%
  arrange(N) %>%
  distinct(url_base) %>%
  pull(url_base)

links$url_base <- factor(links$url_base, levels = url_factors)


links %>%
  arrange(desc(N)) %>%
  filter(N > 10) %>%
  ggplot() +
  geom_bar(aes(url_base, fill = section), position = 'fill') + 
  coord_flip()



tutorials        <- filter(links, section == 'Tutorials')
real_world       <- filter(links, section == 'R in the Real World')
new_packages     <- filter(links, section == 'New Packages')
package_releases <- filter(links, section == 'Package Releases')
organizations    <- filter(links, section == 'R in Organizations')
highlight        <- filter(links, section == 'Highlight')
insights         <- filter(links, section == 'Insights')
resources        <- filter(links, section == 'Resources')
videos_podcasts  <- filter(links, section == 'Videos % Podcasts')

highquality <- filter(links, section %in% c('Highlight','Insights')) %>%
  group_by(url_base, N) %>%
  summarise(n = n()) %>%
  mutate(ratio = n/N) %>%
  mutate(label = ifelse(n > 1 , as.character(url_base),'')) %>%
  arrange(desc(ratio)) %>%
  ungroup()


ggplot(highquality, aes(n,N)) +
  geom_point(aes(color = ratio, size = n), alpha = 0.6 ) +
  geom_smooth() +
  ggrepel::geom_label_repel(aes(label = label), size = 2 ) +
  xlim(0,15) + ylim(0,100) 
  
  # geom_text(aes(label = label), size = 2 )

select(highquality, n,N) %>%
  ungroup() %>%
ggplot(aes(N,n)) +
  geom_point() +
  geom_smooth()

highlight <- bind_rows(highlight,highlight)


write_csv(links,'links.csv')
write_excel_csv(links, 'links.csv')


