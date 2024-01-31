library(dplyr)
library(stringr)
library(tidytext)

#cargar ----
liras_0 <- bind_rows(readr::read_rds("liras_scraping.rds"),
                     readr::read_rds("liras_scraping_2.rds"),
                     readr::read_rds("liras_scraping_3.rds"))

liras_0 |> 
  mutate(titulo = str_to_sentence(titulo))

load("/home/bastian/Otros/Constitucion/constitucion_conceptos/stopwords.Rdata")
stopwords

#liras por párrafo ----
liras_parrafo <- liras_0 |> 
  arrange(url) |> 
  #dar id a cada lira
  group_by(autor, titulo, url) |> 
  mutate(id_lira = dplyr::cur_group_id()) |> 
  arrange() |> 
  #numero de párrafo
  group_by(id_lira) |> 
  mutate(n_parrafo = 1:n()) |> 
  ungroup() |> 
  rename(txt_parrafo = texto)

#liras por línea ----
liras_linea <- liras_parrafo |> 
  #separar párrafos en líneas
  unnest_tokens(output = txt_linea, input = txt_parrafo, 
                token = "regex", pattern = "\n", #separar palabras en los saltos
                to_lower = FALSE, drop = F) |>  #sin tirar a minúsculas
  #numero de línea
  group_by(id_lira) |> 
  mutate(n_linea = 1:n()) |> 
  ungroup() |> 
  #limpiar
  mutate(txt_linea = str_trim(txt_linea),
         txt_linea = str_squish(txt_linea)) |> 
  print(n=20)

#liras por palabra ----
liras_palabra <- liras_linea |> 
  #separar líneas en palabras
  unnest_tokens(output = txt_palabra, input = txt_linea, 
                token = "regex", pattern = " ", #separar palabras en los espacios
                to_lower = FALSE, drop = F) |>  #sin tirar a minúsculas
  #numero de palabra
  group_by(id_lira) |> 
  mutate(n_palabra = 1:n()) |>  #crear número de cada línea
  ungroup() |> 
  #limpiar
  mutate(txt_palabra = str_to_lower(txt_palabra),
         txt_palabra = str_trim(txt_palabra),
         txt_palabra = str_squish(txt_palabra)) |> 
  #eliminar puntuacion
  mutate(txt_palabra = str_remove_all(txt_palabra, "\\.|\\,|\\¿|\\?|\\!|\\¡|\\*|\\+|\\-")) |> 
  print(n=20)

#filtrar stopwords
stopwords_extra <- c("mui", "porque", "cuando", "fué", "hoi", "hai", 
                     "despues", "voi", "dió", "aunque", "soi", "digo", "tambien",
                     "siempre", "estaba")

stopwords_cortas <- stopwords[nchar(stopwords) < 6]
stopwords_2 <- c(stopwords_cortas, stopwords_extra)

#filtrar
tictoc::tic()
liras_palabra_2 <- liras_palabra |> 
  filter(!(txt_palabra %in% stopwords_2))
tictoc::toc()

#guardar ----
arrow::write_feather(liras_palabra, "liras_palabra.feather")
arrow::write_feather(liras_palabra_2, "liras_palabra_2.feather")
