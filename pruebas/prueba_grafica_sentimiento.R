sdal_3 <- readr::read_rds("datos/sdal_diccionario_afectos_español_expandido.rds")
sdal_2 <- readr::read_rds("datos/sdal_diccionario_afectos_español.rds")

liras <- readr::read_rds("datos/corpus_lira_popular.rds")
liras_palabra <- arrow::read_feather("datos/corpus_lira_popular_palabra.feather")

library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)

id_lira = sample(unique(liras_palabra$id), 1)

lira_2 <- liras_palabra |> 
  filter(id == id_lira) |> 
  select(starts_with("n_"), starts_with("txt_")) |> 
  left_join(sdal_3, join_by(txt_palabra == word), relationship = "many-to-many")

lira_3 <- lira_2 |> 
  group_by(n_linea, txt_palabra) |> 
  summarize(across(starts_with("s_"), ~mean(.x, na.rm=T))) |> 
  mutate(across(starts_with("s_"), ~replace_na(.x, 0)))

lira_4 <- lira_3 |> 
  pivot_longer(cols = starts_with("s_"))

lira_4 |> print(n=Inf)

lira_4 |> 
  ggplot(aes(x = n_linea, y = value, fill = value)) +
  geom_col() +
  stat_smooth(method = lm, formula = y ~ poly(x, 10), se = FALSE, color = "orange") +
  scale_fill_gradient2(low = "green3", mid = "white", high = "purple2") +
  facet_wrap(~name, ncol = 1) +
  theme_minimal(base_line_size = 0) +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1))


