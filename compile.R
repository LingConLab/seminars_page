## RScript doesn't load the same environment variables as your RStudio session, so find your path executing `quarto:::find_quarto()`:

Sys.setenv(QUARTO_PATH="/usr/lib/rstudio/resources/app/bin/quarto/bin/quarto")

# package instalation -----------------------------------------------------

# install.packages(c("tidyverse", "readxl", "quarto"))

# data transform ----------------------------------------------------------
library(tidyverse)
df <- readxl::read_xlsx("data.xlsx")
df |> 
  slice(1) |> 
  mutate(day = day + 1) |> 
  bind_rows(df) |> 
  arrange(desc(year), desc(month), desc(day)) |> 
  mutate(day2 = ifelse(lag(day) == day & lag(month) == month, NA, day),
         month2 = ifelse(lag(day) == day & lag(month) == month, NA, month),
         day = day2,
         abstract = ifelse(is.na(abstract), "", abstract),
         month = month2) |> 
  slice(-1) |> 
  mutate(month_ru = case_when(month == 1 ~ "января",
                              month == 2 ~ "февраля",
                              month == 3 ~ "марта",
                              month == 4 ~ "апреля",
                              month == 5 ~ "мая",
                              month == 6 ~ "июня",
                              month == 7 ~ "июля",
                              month == 8 ~ "августа",
                              month == 9 ~ "сентября",
                              month == 10 ~ "октября",
                              month == 11 ~ "ноября",
                              month == 12 ~ "декабря"),
         month_en = case_when(month == 1 ~ "January",
                              month == 2 ~ "February",
                              month == 3 ~ "March",
                              month == 4 ~ "April",
                              month == 5 ~ "May",
                              month == 6 ~ "June",
                              month == 7 ~ "July",
                              month == 8 ~ "August",
                              month == 9 ~ "September",
                              month == 10 ~ "October",
                              month == 11 ~ "November",
                              month == 12 ~ "December")) ->
  df


# create russian part -----------------------------------------------------

file.remove("result_ru.qmd")

str_c("\n\n## Семинары [международная лаборатории языковой конвергенции](https://ilcl.hse.ru/)\n\n
  Если вы хотите участвовать в семинарах лаборатории, вы можете зарегистрироваться [здесь](https://ilcl.hse.ru/en/polls/420288221.html).
        ") |> 
  write_lines("result_ru.qmd", append = TRUE)

map(unique(df$year), function(i){
  df |> 
    filter(year == i) ->
    data_generate_text
  
  str_c("\n\n### Расписание семинаров в ", i, " году\n\n
:::: {.columns}
\n\n") |> 
    write_lines("result_ru.qmd", append = TRUE)
  
  map(seq_along(data_generate_text$title), function(j){
    data_generate_text |> 
      slice(j)  |> 
      mutate(one_talk = str_c("::: {.column width='15%'}\n\n",
                              ifelse(is.na(day), "", str_c("**", day)), 
                              " ", 
                              ifelse(is.na(month_ru), "", str_c(month_ru, "**")), 
                              "\n\n", 
                              ":::\n\n",
                              "::: {.column width='85%'}\n\n",
                              "*", author, "*\n\n",
                              "**", title, "**\n\n",
                              "<details><summary>Аннотация</summary>\n\n",
                              abstract,
                              "</details>\n\n",
                              ":::\n\n")) |> 
      pull(one_talk) |> 
      write_lines("result_ru.qmd", append = TRUE)
  })
  
  str_c("::::\n\n") |> 
    write_lines("result_ru.qmd", append = TRUE)
})

quarto::quarto_render("result_ru.qmd")

# create english part -----------------------------------------------------

file.remove("result_en.qmd")

str_c("\n\n## Seminars of [Linguistic Convergence Laboratory](https://ilcl.hse.ru/en/)\n\n
  If you are interested in participating in the laboratory seminars, please register [here](https://ilcl.hse.ru/en/polls/420288221.html).
        ") |> 
  write_lines("result_en.qmd", append = TRUE)

map(unique(df$year), function(i){
  df |> 
    filter(year == i) ->
    data_generate_text
  
  str_c("\n\n### Seminar schedule ", i, "\n\n
:::: {.columns}
\n\n") |> 
    write_lines("result_en.qmd", append = TRUE)
  
  map(seq_along(data_generate_text$title), function(j){
    data_generate_text |> 
      slice(j)  |> 
      mutate(one_talk = str_c("::: {.column width='15%'}\n\n",
                              ifelse(is.na(day), "", str_c("**", day)), 
                              " ", 
                              ifelse(is.na(month_en), "", str_c(month_en, "**")), 
                              "\n\n", 
                              ":::\n\n",
                              "::: {.column width='85%'}\n\n",
                              "*", author, "*\n\n",
                              "**", title, "**\n\n",
                              "<details><summary>Abstract</summary>\n\n",
                              abstract,
                              "</details>\n\n",
                              ":::\n\n")) |> 
      pull(one_talk) |> 
      write_lines("result_en.qmd", append = TRUE)
  })
  
  str_c("::::\n\n") |> 
    write_lines("result_en.qmd", append = TRUE)
})

quarto::quarto_render("result_en.qmd")
