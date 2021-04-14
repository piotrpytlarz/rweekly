library(tidyverse)
library(zoo)

download.file('https://github.com/rweekly/rweekly.org/archive/gh-pages.zip', 'gh-pages.zip')

md_files <- unzip('gh-pages.zip', list = TRUE) %>%
  mutate(posts = grepl('rweekly.org-gh-pages/_posts/', Name)) %>%
  filter(posts == TRUE, Length > 0) %>%
  pull(Name)

unzip('gh-pages.zip', files = md_files)

md_files = list.files(path = 'rweekly.org-gh-pages/_posts', pattern="*.md")
md_files_full = list.files(path = 'rweekly.org-gh-pages/_posts', pattern="*.md", full.names = TRUE)

ifelse(!dir.exists(file.path('md-to-csv-files')), dir.create(file.path('md-to-csv-files')), FALSE)

for (f in 1:length(md_files_full))  {

mdown <- readLines(md_files_full[f])

mdown <- as_tibble(mdown) %>% 
  mutate(
    nchar = nchar(value)) %>% 
  filter(
    nchar > 0) %>%
  mutate(
    section_lookup = grepl('##',value),
    section_title = ifelse(section_lookup == TRUE, value, NA ))

mdown <- mdown %>%
  mutate(
    section = zoo::na.locf(mdown$section_title, na.rm = FALSE)) %>%
  filter(
    section_lookup == FALSE)

mdown <- mdown %>% 
  mutate(
         type = ifelse(substring(mdown$value,1,3) == '+ [' & 
                         grepl('\\]', mdown$value) &
                         grepl('\\(', mdown$value) &
                         grepl('\\)', mdown$value),'link',
                       ifelse(substring(mdown$value,1,2) == '![','image','unid')))

issue_long = md_files[f]
issue_regexp = regexpr("[0-9]+-[0-9]+-[0-9]+-",issue_long)

mdown <- mdown %>%
  mutate(
    issue = substring(issue_long,1,attributes(issue_regexp)$match.length-1))

write_csv(mdown,paste0('md-to-csv-files/',md_files[f],'.csv'))

str(mdown)
}

rm(list = ls())



