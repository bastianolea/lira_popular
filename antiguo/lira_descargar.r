library(dplyr)
library(rvest)
library(polite)
library(purrr)
library(stringr)

#índice ----
url_lira <- "https://www.lirapopular.cl/lirapopularxix/2021/10/18/indice/"
sesion <- session(url_lira)

#obtiene todos los enlaces presentes en el índice
links_indice <- sesion |> 
  read_html() |> 
  html_element(".menu-liras-container") |> 
  #html_element(".menu-item") |> 
  html_elements(".sub-menu") |> 
  html_elements("a") |>
  html_attr("href")

length(links_indice) #cantidad de entradas en el índice

#sesiones ----
#entra a uno de los enlaces del índice
#sesion_lira <- session(links_indice[15]) |> read_html()

sesiones_lira <- map(links_indice, ~{
  sesion_lira <- bow(.x) 
})

#enlaces internos ----
#dentro del link, obtiene los enlaces internos a cada entrada del índice, dado que hay varios enlaces por entrada
# links_internos_lira <- sesion_lira |> 
#   html_element(".entry-content") |> 
#   html_elements("a") |>
#   html_attr("href") %>%
#   tibble("link" = .) |> 
#   filter(stringr::str_detect(link, "/$")) |> 
#   pull()

links_internos_liras <- map(sesiones_lira, ~{
  scrape(.x) |> 
    html_element(".entry-content") |> 
    html_elements("a") |>
    html_attr("href") %>%
    tibble("link" = .) |> 
    filter(stringr::str_detect(link, "/$")) |> 
    pull()
})

#cargar enlaces ----
#readr::write_rds(links_internos_liras |> unlist(), "links_internos_liras.rds")
links_internos_liras <- readr::read_rds("links_internos_liras.rds")

#links_internos_liras_2 <- links_internos_liras |> unlist() |> unique()

#prueba ----
#luego entra a los enlaces internos y obtiene las liras en sí mismas

sesion_lira <- bow(links_internos_liras[1546])

lira_interna <- scrape(sesion_lira)

lira_titulo <- lira_interna |>
  html_element("#content") |>
  #html_element("h1") #|>
  html_element(".entry-title") |>
  html_text()

lira_metadata <- lira_interna |>
  html_element("#content") |>
  #html_element("h1") #|>
  html_element(".entry-meta") |>
  html_elements("a") |>
  html_text()

lira_url <- sesion_lira$url
tibble("titulo" = lira_titulo,
       #"metadata" = lira_metadata,
       "fecha" = lira_metadata[1],
       "autor" = lira_metadata[3],
       "tipo" = lira_metadata[4],
       "url" = lira_url,
       "texto" = lira_texto)


lira_texto <- lira_interna |>
  html_element(".entry-content") |>
  html_elements("p") |> 
  html_text2() |> 
  stringr::str_trim() %>%
  tibble("texto" = .) |> 
  filter(row_number() != n()) |> 
  pull()

scrape(sesion_lira) |>
  html_element(".entry-content") |>
  html_elements("p") |> 
  html_text2() |> 
  stringr::str_trim() %>%
  tibble("texto" = .) |> 
  filter(row_number() != n()) |> 
  mutate(url = sesion_lira$url)


#scraping ----
#abrir sesiones
#sesiones_liras_internas <- map((links_internos_liras |> unlist())[1:4], ~{
# sesiones_liras_internas <- map(links_internos_liras, ~{
#   sesion_lira <- bow(.x) 
# })
# 
# #obtener metadata de liras
# liras_metadata <- map(sesiones_liras_internas, ~{



#obtener metadata de liras

#liras_scraping <- map(links_internos_liras[1:2000], ~{
#liras_scraping <- map(links_internos_liras[4001:6211], ~{
liras_scraping <- map(links_internos_liras[c(4001:6207, 6209)], ~{ 
  message("scraping ", .x)
  
  #sesión
  sesion_lira <- bow(.x) |> 
    tryCatch(error = function(e) {
      message(e)
      return(NULL)
    })
  
  #htlm
  lira_interna <- scrape(sesion_lira) 
  
  #titulo
  lira_titulo <- lira_interna |> 
    html_element("#content") |> 
    #html_element("h1") #|> 
    html_element(".entry-title") |> 
    html_text()
  
  if(length(lira_titulo) == 0) { return(NULL) } 
  
  #metadatos
  lira_metadata <- lira_interna |> 
    html_element("#content") |> 
    #html_element("h1") #|> 
    html_element(".entry-meta") |> 
    html_elements("a") |> 
    html_text()
  
  #dirección url
  lira_url <- sesion_lira$url
  
  #texto de la lira
  lira_texto <- lira_interna |>
    html_element(".entry-content") |>
    html_elements("p") |> 
    html_text2() |> 
    stringr::str_trim() %>%
    tibble("texto" = .) |> 
    filter(row_number() != n()) |> 
    pull()
  
  if(length(lira_texto) == 0) { return(NULL) } 
  
  #output
  return(
    tibble("titulo" = lira_titulo,
           #"metadata" = lira_metadata,
           "fecha" = lira_metadata[1],
           "autor" = lira_metadata[3],
           "tipo" = lira_metadata[4],
           "url" = lira_url,
           "texto" = lira_texto)
  )
})

#guardar ----
readr::write_rds(liras_scraping, "liras_scraping_3.rds")

# #links_internos_liras[c(6200:6207, 6209)]
# 
# scraping https://www.lirapopular.cl/lirapopularxix/2021/07/14/contestacional-que-le-hace-los-vereosa-reyes/
#   scraping https://www.lirapopular.cl/lirapopularxix/2021/11/02/duelo-al-poeta-reyes/
#   scraping https://www.lirapopular.cl/lirapopularxix/contestacion-a-las-satiras-de-meneses/
#   scraping https://www.lirapopular.cl/lirapopularxix/gran-catastrofe-en-mulchen/
#   scraping https://pisapapeles.net/un-paseo-por-la-historia-de-la-telefonia-en-chile-review/
#   scraping https://www.lirapopular.cl/lirapopularxix/las-grandes-inundaciones-de-valparaiso/
#   scraping https://www.lirapopular.cl/nosotros/
#   scraping https://transkribus.eu/Transkribus/

# #texto 
# # lira_texto <- lira_interna |>
# #   html_element(".entry-content") |>
# #   html_elements("p")
# 
# liras_texto <- map(links_internos_liras[1:2000], ~{
#   message("scraping ", .x)
#   
#   sesion_lira <- bow(.x) |> 
#     try()
#   
#   scrape(sesion_lira) |>
#     html_element(".entry-content") |>
#     html_elements("p") |> 
#     html_text2() |> 
#     stringr::str_trim() %>%
#     tibble("texto" = .) |> 
#     filter(row_number() != n()) |> 
#     mutate(url = sesion_lira$url) |> 
# })

#revisar ----
liras_2 <- liras_texto |> 
  bind_rows() |> 
  #agregar metadata
  left_join(liras_metadata |> 
              bind_rows())

liras_2
