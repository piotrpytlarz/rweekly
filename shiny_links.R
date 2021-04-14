nms <- names(links)

shiny_title <- links[str_detect(links$title, '[Ss]hiny'),]
shiny_url <- links[str_detect(links$url, '[Ss]hiny'),]
shiny_description <- links[str_detect(links$description, '[Ss]hiny'),]

shiny_links <- rbind(shiny_url, shiny_title) %>% rbind(shiny_description) %>% distinct()
rm(shiny_title, shiny_url, shiny_description)

shiny_url_bases <- shiny_links %>%
  count(url_base) %>%
  arrange(desc(n))


