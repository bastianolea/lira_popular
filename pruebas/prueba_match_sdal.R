library(dplyr)
library(stringr)
library(tidytext)
library(textclean)

liras <- readr::read_rds("datos/corpus_lira_popular.rds")
source("funciones.R")

corpus_palabras <- liras_palabra$txt_palabra |> unique() |> sort()

corpus_palabras_2 <- corpus_palabras |> 
  str_remove_all(simbolos_regex) |> 
  str_remove_all(numeros) |> 
  str_subset(".") |> 
  unique() |> 
  sort()


sdal

pruebas <- tibble("palabras_presentes" = corpus_palabras_2) |> 
  filter(nchar(palabras_presentes) > 1) |> 
  mutate(match_total = ifelse(palabras_presentes %in% sdal$word, TRUE, FALSE),
         match_parcial = ifelse(str_detect(palabras_presentes, paste0(sdal$word, collapse = "|")), TRUE, FALSE))

pruebas |> 
  filter(match_total)

pruebas |> 
  filter(match_parcial)


#hay que usar string dist y evaluar si el match es de n-2 o n-3 caracteres de la palabra del diccionario