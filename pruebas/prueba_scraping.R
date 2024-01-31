library(dplyr)
library(rvest)
library(polite)
library(purrr)
library(stringr)

url = "https://www.lirapopular.cl/lirapopularxix/"
sitio <- session(url) |> 
  read_html()

sitio |> 
  html_element(".menu-liras-container") |> 
  #html_element(".menu-item") |> 
  html_elements(".sub-menu") |> 
  html_elements("a") |>
  html_attr("href")

enlaces_autores <- sitio |> 
  html_elements("ul") |> 
  html_elements("a") |>
  html_attr("href") |>
  str_subset("category") |> 
  str_subset("menu", negate = TRUE)

#cargar sitio de un autor
sitio_autor <- session(enlaces_autores[20]) |> 
  read_html()
  
#lista de liras del autor
liras_autor <- sitio_autor |> 
  html_elements(".hentry-wrapper")

#elegir una lira de entre las del autor
lira_sitio <- liras_autor[[2]]

lira_titulo <- lira_sitio |> 
  html_elements("h1") |> 
  html_text()

lira_cuerpo <- lira_sitio |> 
  html_elements(".entry-content") |> 
  html_text2()

lira_autor <- lira_sitio |> 
  html_elements(".cat-links") |> 
  html_text2()

lira_categoria <- lira_sitio |> 
  html_elements(".tags-links") |> 
  html_text2()

lira_fecha_post <- lira_sitio |> 
  html_elements(".posted-on") |> 
  html_text2()

lira_completa_link <- lira_sitio |> 
  html_elements("a") |> 
  html_attr("href") |> 
  last()
  


