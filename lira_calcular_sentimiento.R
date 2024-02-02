# aplica el diccionario de sentimientos SDAL a cada palabra del corpus, para definir su sentimiento en tres dimensiones:
# 
#   agrado (agradable / neutra / desagradable)
#   activación (activa / neutra / pasiva)
#   imaginabilidad (fácil de imaginar / neutra / difícil de imaginar)
# 
# luego, estos valores se usan para calcular promedios de sentimiento de líneas, párrafos y finalmente documentos
# https://github.com/abcsds/sdal

# get_sentiments("bing")
# get_sentiments("afinn")
# get_sentiments("nrc")

sdal_3 <- readr::read_rds("datos/sdal_diccionario_afectos_español_expandido.rds")
# sdal_2 <- readr::read_rds("datos/sdal_diccionario_afectos_español.rds")

liras <- readr::read_rds("datos/corpus_lira_popular.rds")
liras_palabra <- arrow::read_feather("datos/lira_datos_palabra.feather")

library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(ggplot2)


lira_sdal <- liras_palabra |> 
  select(starts_with("n_"), starts_with("txt_")) |> 
  left_join(sdal_3, join_by(txt_palabra == word), relationship = "many-to-many")


# lira_sdal_calculado <- lira_sdal |> 
#   group_by(n_id, n_parrafo, n_linea) |> 
#   select(-ends_with("_sdev"), -obj) |> 
#   #sentimiento por línea
#   mutate(across(c(s_activation, s_imagination, s_pleasantness),
#                    ~mean(.x, na.rm=T) |> replace_na(0), 
#                    .names = "s_linea_{str_remove(.col, 's_')}")) |> 
#   #sentimiento por párrafo
#   group_by(n_id, n_parrafo) |> 
#   mutate(across(c(s_activation, s_imagination, s_pleasantness), 
#                 ~mean(.x, na.rm=T) |> replace_na(0), 
#                 .names = "s_parrafo_{str_remove(.col, 's_')}")) |> 
#   #sentimiento por documento
#   group_by(n_id) |> 
#   mutate(across(c(s_activation, s_imagination, s_pleasantness), 
#                 ~mean(.x, na.rm=T) |> replace_na(0), 
#                 .names = "s_lira_{str_remove(.col, 's_')}")) |> 
#   ungroup()

lira_sdal_linea <- lira_sdal |> 
  group_by(n_id, n_parrafo, n_linea) |> 
  select(-ends_with("_sdev"), -obj) |> 
  #sentimiento por línea
  summarize(across(c(s_activation, s_imagination, s_pleasantness),
                   ~mean(.x, na.rm=T) |> replace_na(0))) |> 
  mutate(unidad = n_linea)

#   #sentimiento por párrafo
lira_sdal_parrafo <- lira_sdal |> 
  group_by(n_id, n_parrafo) |>
  summarize(across(c(s_activation, s_imagination, s_pleasantness),
                   ~mean(.x, na.rm=T) |> replace_na(0))) |> 
  mutate(unidad = n_parrafo)

#sentimiento por documento
lira_sdal_documento <- lira_sdal |> 
  group_by(n_id) |>
  summarize(across(c(s_activation, s_imagination, s_pleasantness),
                   ~mean(.x, na.rm=T) |> replace_na(0))) |> 
  mutate(unidad = n_id)

#guardar cada uno en una misma lista
lira_sentimiento <- list("linea" = lira_sdal_linea,
                         "parrafo" = lira_sdal_parrafo,
                         "documento" = lira_sdal_documento)


#guardar ----
readr::write_rds(lira_sentimiento, "datos/lira_sentimiento.rds")


#graficar ----
lira_sentimiento$linea |> 
  filter(n_id == 200) |>
  pivot_longer(cols = starts_with("s_"), names_to = "sentimiento", values_to = "sentimiento_valor") |> 
  #graficar
  ggplot(aes(x = unidad, y = sentimiento_valor, fill = sentimiento_valor)) +
  geom_col() +
  stat_smooth(method = lm, formula = y ~ poly(x, 10), se = FALSE, color = "orange") +
  scale_fill_gradient2(low = "green3", mid = "white", high = "purple2") +
  facet_wrap(~sentimiento, ncol = 1) +
  theme_minimal(base_line_size = 0) +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1))


