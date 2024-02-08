# The idea of tf-idf is to find the important words for the content of each document by 
# decreasing the weight for commonly used words and increasing the weight for words that are 
# not used very much in a collection or corpus of documents

library(dplyr)
library(stringr)
library(tidytext)
library(forcats)
library(ggplot2)


#cargar
liras_palabra <- arrow::read_feather("datos/lira_datos_palabra.feather")

liras_topicos <- readr::read_rds("~/Documents/Apps Shiny/lira_popular/datos/lira_topicos.rds") |> 
  select(n_id, topico)


#por topico ---- 
liras_conteo_topico <- liras_palabra |> 
  left_join(liras_topicos, by = "n_id") |> 
  group_by(topico) |> 
  count(n_id, txt_palabra, sort = T)
  

liras_tfidf_topico <- liras_conteo_topico |> 
  bind_tf_idf(txt_palabra, n_id, n)

liras_tfidf_topico |>   
  arrange(desc(tf_idf))

liras_tfidf_topico |> 
  group_by(topico) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(txt_palabra, tf_idf), fill = topico)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topico, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)





#por categoria ---- 
liras_conteo_categoria <- liras_palabra |> 
  #reducir categorías
  mutate(categoria = tolower(categoria),
         categoria = case_when(str_detect(categoria, "décima") ~ "Décima",
                               str_detect(categoria, "verso") ~ "Verso",
                               .default = str_to_sentence(categoria))) |> 
  #separar categorias en filas (duplica documentos)
  separate(categoria, into = paste("categoria", 1:5, sep = "_"),  sep = ", ") |> 
  pivot_longer(cols = starts_with("categoria"), names_to = "categoria_n", values_to = "categoria") |> 
  filter(!is.na(categoria)) |> 
  #agregar topicos   
  left_join(liras_topicos, by = "n_id") |> 
  #conteo
  group_by(categoria) |> 
  count(n_id, txt_palabra, sort = T)


liras_tfidf_categoria <- liras_conteo_categoria |> 
  bind_tf_idf(txt_palabra, n_id, n)

liras_tfidf_categoria |>   
  arrange(desc(tf_idf))

liras_tfidf_categoria |> 
  group_by(categoria) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(txt_palabra, tf_idf), fill = categoria)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~categoria, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)



