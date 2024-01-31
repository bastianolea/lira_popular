library(dplyr)
library(rvest)
library(polite)
library(purrr)
library(stringr)


#guardar lista de enlaces a liras, por autor
enlaces_autores_liras <- readr::read_rds("datos/enlaces_liras.rds")

enlaces_liras <- enlaces_autores_liras |> unlist() |> unname() |> unique()

#loop por cada lira
corpus_lira_popular <- map(enlaces_liras, \(enlace_lira) {
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

# aplanar ----
corpus_lira_popular_2 <- corpus_lira_popular |> 
  # list_rbind() |> 
  mutate(titulo = str_to_sentence(titulo),
         titulo = str_remove(titulo, "\\.$")) |> 
  mutate(cuerpo = str_remove(cuerpo,
                             "\\\n\\\nVer lira completa\\\n"),
         cuerpo = str_remove(cuerpo,
                             "\\\nConvertir a PDFImprimir"))
                             
# corpus_lira_popular_2[1, ]$cuerpo

readr::write_rds(corpus_lira_popular_2, "datos/corpus_lira_popular.rds")
