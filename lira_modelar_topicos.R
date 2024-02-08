# https://www.tidytextmining.com/topicmodeling.html#latent-dirichlet-allocation

# modelar topicos entre los textos usando asignación Latente de Dirichlet (ALD)
# algoritmo de aprendizaje automático no supervisado

library(tidytext)
library(topicmodels)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

liras_palabra <- arrow::read_feather("datos/lira_datos_palabra.feather")

# conteo de palabras por documento ----
liras_palabra_conteo <- liras_palabra |> 
  filter(!str_detect(txt_palabra, "\\d+")) |> 
  count(n_id, txt_palabra, sort = T) |> 
  filter(txt_palabra != "")

# crear document text matrix ----
lira_dtm <- tidytext::cast_dtm(data = liras_palabra_conteo, document = n_id, term = txt_palabra, value = n)

# modelar temas ----
lira_lda <- topicmodels::LDA(lira_dtm, k = 4, #cantidad de temas
                             control = list(seed = 1234)
)

# calcular word-topic probabilities ----
lira_topics <- tidytext::tidy(lira_lda, matrix = "beta")

## top terminos por tópico ----
lira_top_terminos <- lira_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

## graficar topicos ----
lira_top_terminos %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## probabilidad de topico por documento ----
# per-document-per-topic probabilities
lira_documents <- tidy(lira_lda, matrix = "gamma")

lira_documents |> 
  arrange(as.numeric(document), desc(gamma)) |> 
  filter(gamma > 0.4) |>
  group_by(document) |> 
  mutate(n_topics = n()) |> 
  filter(n_topics > 1)
#algunos documentos no clasifican 100% en un solo topico


#pivotar a columnas para indicar el tópico al que pertenece cada documento
liras_topicos <- lira_documents |> 
  arrange(as.numeric(document), desc(gamma)) |>
  group_by(document) |> 
  mutate(topico = max(gamma),
         topico = ifelse(topico == gamma, topic, NA)) |> 
  fill(topico) |> 
  pivot_wider(id_cols = c(document, topico), names_from = topic, names_prefix = "topico_", values_from = gamma) |> 
  mutate(document = as.integer(document)) |> 
  rename(n_id = document)


# guardar ----

#documentos por id, topico al cual pertenecen, y gammas de cada topico para el documento
readr::write_rds(liras_topicos, "datos/lira_topicos.rds")

#lista de palabras y sus respectivos gammas por topico
readr::write_rds(lira_topics, "datos/lira_diccionario_topicos.rds")

#lista de palabras unicas y el topico que tiene mayor gamma
lira_topics |> 
  group_by(term) |> 
  slice_max(beta) |> 
  arrow::write_feather("datos/lira_diccionario_topicos_unicos.feather")
