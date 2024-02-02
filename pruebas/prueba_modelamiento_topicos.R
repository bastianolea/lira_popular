# https://www.tidytextmining.com/topicmodeling.html#latent-dirichlet-allocation

# modelar topicos entre los textos usando asignación Latente de Dirichlet (ALD)
# algoritmo de aprendizaje automático no supervisado

library(tidytext)
library(topicmodels)
library(dplyr)
library(tidyr)
library(ggplot2)

# cast() turns a tidy one-term-per-row data frame into a matrix. 
# tidytext provides three variations of this verb, each converting to a different type of matrix: 
# cast_sparse() (converting to a sparse matrix from the Matrix package), 
# cast_dtm() (converting to a DocumentTermMatrix object from tm), and cast_dfm() (converting to a dfm object from quanteda).

liras_palabra <- arrow::read_feather("datos/corpus_lira_popular_palabra.feather")

cast(liras_palabra)

# conteo de palabras ----
liras_palabra_conteo <- liras_palabra |> 
  count(id, txt_palabra, sort = T) |> 
  filter(txt_palabra != "")

# crear document text matrix ----
lira_dtm <- tidytext::cast_dtm(data = liras_palabra_conteo, document = id, term = txt_palabra, value = n)

# modelar temas ----
lira_lda <- topicmodels::LDA(lira_dtm, k = 6, control = list(seed = 1234))

# ## top palabras por texto 
# tidy(lira_dtm) %>%
#   filter(document == 60) %>%
#   arrange(desc(count))


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

## palabras por topico ----
beta_wide <- lira_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

## probabilidad de topico por documento ----
# per-document-per-topic probabilities
lira_documents <- tidy(lira_lda, matrix = "gamma")

lira_documents |> 
  arrange(as.numeric(document), desc(gamma))

lira_documents |> 
  arrange(as.numeric(document), desc(gamma)) |> 
  filter(gamma > 0.4) |>
  group_by(document) |> 
  mutate(n_topics = n()) |> 
  filter(n_topics > 1)
#algunos documentos no clasifican 100% en un solo topico


liras_topicos <- lira_documents |> 
  arrange(as.numeric(document), desc(gamma)) |>
  group_by(document) |> 
  mutate(topico = max(gamma),
         topico = ifelse(topico == gamma, topic, NA)) |> 
  fill(topico) |> 
  pivot_wider(id_cols = c(document, topico), names_from = topic, names_prefix = "topico_", values_from = gamma)


# guardar ----
readr::write_rds(liras_topicos, "datos/lira_topicos.rds")
