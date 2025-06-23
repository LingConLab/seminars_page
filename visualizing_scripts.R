setwd("/home/agricolamz/work/bureaucracy/linguistic_convergency/seminars_page")
library(tidyverse)
library(lubridate)
library(tidytext)
library(ggwordcloud)

df <- read_csv("data.csv")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
df |> 
  mutate(date = lubridate::make_date(year = 2020, month = month, day = day)) |> 
  ggplot(aes(date))+
  geom_dotplot(dotsize = 0.3)+
  facet_wrap(~year, ncol = 1)+
  labs(x = "", y = "")+
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        text = element_text(size = 18))+
  scale_x_date(date_labels = "%B")
Sys.setlocale("LC_TIME", "ru_RU.UTF-8")

df |> 
  mutate(author = str_remove_all(author, "\\(.*?\\)"),
         author = str_split(author, ", ")) |> 
  unnest_longer(author) |> 
  mutate(author = str_squish(author)) |> 
  count(author, sort = TRUE) |> 
  filter(n > 1) |>
  mutate(author = fct_reorder(author, n)) |> 
  ggplot(aes(n, author))+
  geom_col()+
  labs(x = "", y = "")+
  theme_minimal()

map(stopwords::stopwords_getsources()[-c(3, 6, 8)], function(i){
  print(i)
  stopwords::stopwords(language = "en", source = i)}) |> 
  unlist() |>  
  unique() |> 
  sort() ->
  stopwords_en

df |> 
  mutate(abstract = str_replace_all(abstract, "Nakh-Daghestanian", "Nakh_Daghestanian"),
         abstract = str_replace_all(abstract, "cross-linguistic", "cross_linguistic"),
         abstract = str_replace_all(abstract, "East Caucasian", "East_Caucasian"),
         abstract = str_replace_all(abstract, "Indo-European", "Indo_European"),
         abstract = str_replace_all(abstract, "Afro-Asiatic", "Afro_Asiatic"),
         abstract = str_replace_all(abstract, "West Caucasian", "West_Caucasian"),
         abstract = str_replace_all(abstract, "East-Caucasian", "East_Caucasian"),
         abstract = str_replace_all(abstract, "West-Caucasian", "West_Caucasian"),
         abstract = str_replace_all(abstract, "Dagestan", "Daghestan"),
         abstract = str_replace_all(abstract, "language", "languages"),
         abstract = str_replace_all(abstract, "languagess", "languages")) |> 
  unnest_tokens(input = "abstract", output = "word") |> 
  count(word, sort = TRUE) |> 
  anti_join(tibble(word = stopwords_en)) |>
  filter(str_detect(word, "[A-z]"),
         !(word %in% c("e.g", "eds", "https", "university", "i.e")),
         n > 20) |> 
  mutate(word = case_when(word == "east_caucasian" ~ "East Caucasian",
                          word == "caucasus" ~ "Caucasus",
                          word == "daghestan" ~ "Daghestan",
                          word == "daghestanian" ~ "Daghestanian",
                          word == "nakh_daghestanian" ~ "Nakh-Daghestanian",
                          word == "indo_european" ~ "Indo-European",
                          word == "afro_asiatic" ~ "Afro-Asiatic",
                          word == "cross_linguistic" ~ "cross-linguistic",
                          word == "russian" ~ "Russian",
                          word == "rutul" ~ "Rutul",
                          word == "andic" ~ "Andic",
                          word == "avar" ~ "Avar",
                          word == "oxford" ~ "Oxford",
                          word == "abaza" ~ "Abaza",
                          word == "lezgic" ~ "Lezgic",
                          word == "andi" ~ "Andi",
                          TRUE ~ word),
         n = log(n)) |> 
  ggplot(aes(label = word, size = n))+
  geom_text_wordcloud(rm_outside = FALSE,
                      grid_margin = 2, 
                      seed = 42,
                      shape = "square",
                      max_grid_size = 138) +
  theme_minimal()

# number of seminars
df |> 
  mutate(date = lubridate::make_date(year = year, month = month, day = day)) |> 
  distinct(date) |> 
  nrow()

