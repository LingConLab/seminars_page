library(tidyverse)
library(lubridate)
df <- readxl::read_xlsx("data.xlsx")

df %>% 
  mutate(date = lubridate::make_date(year = year, month = month, day = day)) %>% 
  filter(year > 2018, year < 2023) %>% 
  ggplot(aes(date))+
  geom_dotplot()+
  facet_wrap(~year, scales = "free")+
  theme_minimal()
  
df %>% 
  mutate(author = str_remove_all(author, "\\(.*?\\)"),
         author = str_split(author, ", ")) %>% 
  unnest_longer(author) %>% 
  mutate(author = str_squish(author)) %>% 
  count(author, sort = TRUE) %>% 
  filter(n > 1) %>%
  mutate(author = fct_reorder(author, n)) %>% 
  ggplot(aes(n, author))+
  geom_col()+
  labs(x = "", y = "")+
  theme_minimal()
  

