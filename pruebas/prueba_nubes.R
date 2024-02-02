# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

library(dplyr)
library(ggplot2)
library(ggwordcloud)

data("love_words_small")


liras <- arrow::read_feather("~/Documents/Apps Shiny/lira_popular/datos/lira_datos_palabra.feather")

datos <- liras |> 
  filter(n_id == 144) |>
  select(titulo, n_palabra, txt_palabra) |> 
  count(txt_palabra) |> 
  filter(n > 1)

datos |> 
  ggplot(aes(label = txt_palabra, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10, trans = power_trans(1/.5)) +
  theme_minimal()
