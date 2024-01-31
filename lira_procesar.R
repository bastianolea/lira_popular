library(dplyr)
library(stringr)
library(tidytext)
library(textclean)

# cargar ----
liras <- readr::read_rds("datos/corpus_lira_popular.rds")
source("funciones.R")

#liras por lira ----
liras_cuerpo <- liras |> 
  arrange(enlace) |> 
  #dar id a cada lira
  group_by(autor, titulo, enlace) |> 
  mutate(id = dplyr::cur_group_id()) |> 
  arrange(id) |> 
  #numero de párrafo
  group_by(id) |> 
  mutate(n_parrafo = 1:n()) |> 
  ungroup() |> 
  rename(txt_cuerpo = cuerpo)


#liras por párrafo ----
liras_parrafo <- liras_cuerpo |> 
  #separar párrafos en líneas
  unnest_tokens(output = txt_parrafo, input = txt_cuerpo, 
                token = "regex", pattern = "\n\n|\n\n\n", #separar palabras en los saltos
                to_lower = FALSE, drop = F)

liras_cuerpo |> filter(id == 2) |> pull(txt_cuerpo)
liras_parrafo |> filter(id == 2) |> pull(txt_parrafo)


#liras por línea ----
liras_linea <- liras_parrafo |> 
  #separar párrafos en líneas
  unnest_tokens(output = txt_linea, input = txt_parrafo, 
                token = "regex", pattern = "\n", #separar palabras en los saltos
                to_lower = FALSE, drop = F) |>  #sin tirar a minúsculas
  #numero de línea
  group_by(id) |> 
  mutate(n_linea = 1:n()) |> 
  ungroup() |> 
  #limpiar
  mutate(txt_linea = str_trim(txt_linea),
         txt_linea = str_squish(txt_linea)) |> 
  print(n=20)

liras_parrafo |> filter(id == 2) |> pull(txt_parrafo)
liras_linea |> filter(id == 2) |> pull(txt_linea)


#liras por palabra ----
liras_palabra <- liras_linea |> 
  #separar líneas en palabras
  unnest_tokens(output = txt_palabra, input = txt_linea, 
                token = "regex", pattern = " ", #separar palabras en los espacios
                to_lower = FALSE, drop = F) |>  #sin tirar a minúsculas
  #numero de palabra
  group_by(id) |> 
  mutate(n_palabra = 1:n()) |>  #crear número de cada línea
  ungroup() |> 
  #limpiar
  mutate(txt_palabra = str_to_lower(txt_palabra),
         txt_palabra = str_trim(txt_palabra),
         txt_palabra = str_squish(txt_palabra)) |> 
  #eliminar puntuacion
  mutate(txt_palabra = str_remove_all(txt_palabra, simbolos)) |> 
  print(n=20)

liras_parrafo |> filter(id == 2) |> pull(txt_parrafo)
liras_linea |> filter(id == 2) |> pull(txt_linea)
liras_palabra |> filter(id == 2) |> pull(txt_palabra)



palabras <- liras_palabra |> 
  filter(id == 2) |> 
  #filtrar stopwords
  filter(!(txt_palabra %in% stopwords_2)) |> 
  pull(txt_palabra) |> 
  #remover símbolos
  str_remove(simbolos_regex) |> 
  str_remove(numeros)
 
tibble("word" = palabras) |> 
  left_join(sdal, join_by(word)) |> 
  print(n=Inf)
  



#filtrar
tictoc::tic()
liras_palabra_2 <- liras_palabra |> 
  filter(!(txt_palabra %in% stopwords_2))
tictoc::toc()



#guardar ----
arrow::write_feather(liras_palabra, "liras_palabra.feather")
arrow::write_feather(liras_palabra_2, "liras_palabra_2.feather")
