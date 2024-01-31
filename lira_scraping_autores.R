usethis::use_git()
usethis::use_gpl3_license()

library(dplyr)
library(rvest)
library(polite)
library(purrr)
library(stringr)

url = "https://www.lirapopular.cl/lirapopularxix/"
sitio <- bow(url) |> 
  scrape()

#obtener enlaces a todos los autores desde el footer del sitio
enlaces_autores <- sitio |> 
  html_elements("ul") |> 
  html_elements("a") |>
  html_attr("href") |>
  str_subset("category") |> 
  str_subset("menu", negate = TRUE)

#loop que entra al sitio de cada autor y guarda los enlaces de sus liras
enlaces_autores_liras <- map(enlaces_autores |> set_names(), \(enlace) {
  message(paste("scraping", enlace))
  
  #cargar sitio de un autor
  sitio_autor <- bow(enlace) |> 
    scrape()
  
  nombre_autor <- sitio_autor |> 
    html_elements(".page-title") |> 
    html_text2()
  
  if (length(nombre_autor) == 0) return(NULL)
  
  #lista de liras del autor
  liras_autor <- sitio_autor |> 
    html_elements(".hentry-wrapper")
  
  #lista de links de liras del autor
  listas_autor_enlaces <- liras_autor |>
    html_elements("h1") |>
    html_elements("a") |>
    html_attr("href")
  
  message(paste(nombre_autor, "tiene", length(listas_autor_enlaces), "liras"))
  message()
  
  return(listas_autor_enlaces)
})

# guardar ----
 readr::write_rds(enlaces_autores_liras, "datos/enlaces_liras.rds")
