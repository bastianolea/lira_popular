#usa web scraping para descargar los textos y metadatos de los documentos 
#desde lirapopular.cl y lirapopular.cl/lirapopularxix/

library(dplyr)
library(rvest)
library(polite)
library(purrr)
library(stringr)

#lira popular xix ----

lira_xix_url = "https://www.lirapopular.cl/lirapopularxix/"

sitio_lira_xix <- bow(lira_xix_url) |> 
  scrape()

## enlaces por autores ----
#obtener enlaces a todos los autores desde el footer del sitio
enlaces_autores_lira_xix <- sitio_lira_xix |> 
  html_elements("ul") |> 
  html_elements("a") |>
  html_attr("href") |>
  str_subset("category") |> 
  str_subset("menu", negate = TRUE)

### obtener enlaces ----
#loop que entra al sitio de cada autor y guarda los enlaces de sus liras
enlaces_autores_liras_xix <- map(enlaces_autores_lira_xix |> set_names(), \(enlace) {
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

### guardar enlaces ----
readr::write_rds(enlaces_autores_liras_xix, "datos/enlaces_liras_xix.rds")


## scraping liras ----
enlaces_liras_xix <- enlaces_autores_liras_xix |> unlist() |> unname() |> unique()

#loop por cada lira
corpus_lira_popular_xix <- map(enlaces_liras_xix, \(enlace_lira) {
  # enlace_lira <- enlaces_liras[2]
  message(paste("scraping lira", enlace_lira))
  
  lira_sitio <- bow(enlace_lira) |> 
    scrape() |> 
    html_elements(".site-content") |> 
    html_elements(".hentry-wrapper")
  
  lira_titulo <- lira_sitio |> 
    html_elements("h1") |> 
    html_text()
  
  if (length(lira_sitio) == 0) {
    warning("lira sin título")
    return(NULL)
  }
  
  lira_enlace <- enlace_lira
  
  lira_cuerpo <- lira_sitio |> 
    html_elements(".entry-content") |> 
    html_text2()
  
  if (length(lira_cuerpo) == 0) {
    warning("lira sin texto")
    return(NULL)
  }
  
  lira_autor <- lira_sitio |> 
    html_elements(".cat-links") |> 
    html_text2()
  
  if (length(lira_autor) == 0) {
    warning("lira sin autor")
    return(NULL)
  }
  
  lira_categoria <- lira_sitio |> 
    html_elements(".tags-links") |> 
    html_text2()
  
  lira_fecha_post <- lira_sitio |> 
    html_elements(".posted-on") |> 
    html_text2()
  
  lira_enlace_ver_mas <- lira_sitio |> 
    html_elements("a") |> 
    html_attr("href") |> 
    last()
  
  tabla_lira <- tibble(lira_titulo,
                       lira_enlace,
                       lira_cuerpo,
                       lira_autor,
                       lira_categoria,
                       lira_fecha_post,
                       lira_enlace_ver_mas) |> 
    rename_with(~str_remove(.x, "lira_"))
  
  if (nrow(tabla_lira) == 0) warning("sin contenido")
  
  return(tabla_lira)
})

### aplanar ----
corpus_lira_popular_xix_2 <- corpus_lira_popular_xix |> 
  # list_rbind() |> 
  mutate(titulo = str_to_sentence(titulo),
         titulo = str_remove(titulo, "\\.$")) |> 
  mutate(cuerpo = str_remove(cuerpo,
                             "\\\n\\\nVer lira completa\\\n"),
         cuerpo = str_remove(cuerpo,
                             "\\\nConvertir a PDFImprimir"))

# corpus_lira_popular_2[1, ]$cuerpo

### guardar lira xix ----
readr::write_rds(corpus_lira_popular_xix_2, "datos/corpus_lira_popular_xix.rds")


# lira popular 1950 ----


lira_1950_url = "https://www.lirapopular.cl/index.php"

sitio_lira_1950 <- bow(lira_1950_url) |> 
  scrape()

## enlaces por categorias ----

enlaces_categorias_lira_1950 <- sitio_lira_1950 |> 
  html_elements(".tagcloud") |> 
  html_elements("a") |>
  html_attr("href") |>
  str_subset("tag")

### obtener enlaces ----
enlaces_categorias_liras_1950 <- map(enlaces_categorias_lira_1950 |> set_names(), \(enlace) {
  message(paste("scraping", enlace))
  #enlace <- enlaces_categorias_lira_1950[3]
  
  #cargar sitio de un autor
  sitio_categoria <- bow(enlace) |> 
    scrape()
  
  nombre_categoria <- sitio_categoria |> 
    html_elements(".page-title") |> 
    html_text2()
  
  if (length(nombre_categoria) == 0) return(NULL)
  
  #lista de liras del autor
  liras_categoria <- sitio_categoria |> 
    html_elements(".hentry-wrapper")
  
  #lista de links de liras del autor
  listas_categoria_enlaces <- liras_categoria |>
    html_elements("h1") |>
    html_elements("a") |>
    html_attr("href")
  
  message(paste(nombre_categoria, "tiene", length(listas_categoria_enlaces), "liras"))
  message()
  
  return(listas_categoria_enlaces)
})

enlaces_categorias_liras_1950

### guardar enlaces ----
readr::write_rds(enlaces_categorias_liras_1950, "datos/enlaces_liras_1950.rds")


## scraping liras ----
enlaces_liras_1950 <- enlaces_categorias_liras_1950 |> unlist() |> unname() |> unique()

#loop por cada lira
corpus_lira_popular_1950 <- map(enlaces_liras_1950, \(enlace_lira) {
  # enlace_lira <- enlaces_liras_1950[2]
  message(paste("scraping lira", enlace_lira))
  
  lira_sitio <- bow(enlace_lira) |> 
    scrape() |> 
    html_elements(".site-content") |> 
    html_elements(".hentry-wrapper")
  
  lira_titulo <- lira_sitio |> 
    html_elements("h1") |> 
    html_text()
  
  if (length(lira_sitio) == 0) {
    warning("lira sin título")
    return(NULL)
  }
  
  lira_enlace <- enlace_lira
  
  lira_cuerpo <- lira_sitio |> 
    html_elements(".entry-content") |> 
    html_text2()
  
  if (length(lira_cuerpo) == 0) {
    warning("lira sin texto")
    return(NULL)
  }
  
  lira_autor <- lira_sitio |> 
    html_elements(".cat-links") |> 
    html_text2()
  
  if (length(lira_autor) == 0) {
    warning("lira sin autor")
    return(NULL)
  }
  
  lira_categoria <- lira_sitio |> 
    html_elements(".tags-links") |> 
    html_text2()
  
  lira_fecha_post <- lira_sitio |> 
    html_elements(".posted-on") |> 
    html_text2()
  
  lira_enlace_ver_mas <- lira_sitio |> 
    html_elements("a") |> 
    html_attr("href") |> 
    last()
  
  tabla_lira <- tibble(lira_titulo,
                       lira_enlace,
                       lira_cuerpo,
                       lira_autor,
                       lira_categoria,
                       lira_fecha_post,
                       lira_enlace_ver_mas) |> 
    rename_with(~str_remove(.x, "lira_"))
  
  if (nrow(tabla_lira) == 0) warning("sin contenido")
  
  return(tabla_lira)
})

### aplanar ----
corpus_lira_popular_1950_2 <- corpus_lira_popular_1950 |> 
  # list_rbind() |> 
  mutate(titulo = str_to_sentence(titulo),
         titulo = str_remove(titulo, "\\.$")) |> 
  mutate(cuerpo = str_remove(cuerpo,
                             "\\\n\\\nVer lira completa\\\n"),
         cuerpo = str_remove(cuerpo,
                             "\\\nConvertir a PDFImprimir"))

# corpus_lira_popular_2[1, ]$cuerpo

### guardar lira 1950 ----
readr::write_rds(corpus_lira_popular_1950_2, "datos/corpus_lira_popular_1950.rds")

