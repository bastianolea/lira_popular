library(shiny)
library(shinyWidgets)
library(dplyr)

library(here)
here::i_am("analizar.r")

#datos ----
liras_palabra <- arrow::read_feather(here("lira_popular_comparaciones", "liras_palabra.feather"))
liras_linea <- arrow::read_feather(here("lira_popular_comparaciones", "liras_linea.feather"))
lista_liras <- arrow::read_feather(here("lira_popular_comparaciones", "lista_liras.feather"))
comparadas_0 <- arrow::read_feather(here("lira_popular_comparaciones", "liras_comparadas_1.feather"))

#stopwords ----
load("/home/bastian/Otros/Constitucion/constitucion_conceptos/stopwords.Rdata")
stopwords_extra <- c("mui", "porque", "cuando", "fué", "hoi", "hai", 
                     "despues", "voi", "dió", "aunque", "soi", "digo", "tambien",
                     "siempre", "estaba", "á", 0:9)
stopwords_cortas <- stopwords[nchar(stopwords) < 6]
stopwords_2 <- c(stopwords_cortas, stopwords_extra)

#vectores ----
v_autores <- liras_palabra$autor |> unique() |> sort()

#sudo ln -s ~/Otros/lira_popular/lira_popular /srv/shiny-server/
  