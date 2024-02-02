# se toma cada línea del corpus, y se compara con todas las demás líneas del corpus, 
# buscando líneas que sean similares entre sí

library(dplyr)
library(stringr)
library(stringdist)
library(purrr)
library(furrr)

here("~/Otros/lira_popular/analizar.r")

#cargar ----
liras_palabra <- arrow::read_feather("datos/corpus_lira_popular_palabra.feather")

#por línea
liras_linea <- liras_palabra |> 
  select(-txt_palabra, -n_palabra) |> 
  distinct()

#limpiar texto para comparar
liras_linea_2 <- liras_linea |> 
  select(id, autor, n_linea, txt_linea) |> 
  #limpiar lineas para comparación
  mutate(txt_linea = txt_linea |> 
           tolower() |> 
           str_remove_all(simbolos_regex) |> 
           str_squish()
  )

#lista de liras ----
max(liras_linea_2$id)

lista_liras <- unique(liras_linea_2$id)
# lista_liras <- liras_linea |>
#   select(autor, titulo, id) |>
#   distinct() |>
#   arrange(id) |> 
#   slice(1:100)


#loop ----
# por cada lira, comparar sus líneas con todas las demás líneas del corpus

plan(multisession, workers = 8) # multicore

comparacion_liras <- future_map(lista_liras, \(.lira) {
  #.lira = 345
  
  message(paste("lira", .lira)) 
  
  #lira filtrada
  lira <- liras_linea_2 |> 
    filter(id == .lira)
  
  #resto de las liras
  liras_resto <- liras_linea_2 |> 
    #excluir la misma lira y las del mismo autor
    filter(id != .lira) |> 
    select(-autor) |> 
    filter(nchar(txt_linea) > 15) |> 
    mutate(join = 1)
  
  #unir liras ----
  message("uniendo...")
  
  liras_pre_comparacion <- lira |>
    select(-autor) |> 
    mutate(join = 1) |> 
    #unión
    left_join(liras_resto, 
              by = "join", relationship = "many-to-many") |> 
    select(-join)
  
  #vetar comparaciones
  liras_pre_comparacion_2 <- liras_pre_comparacion |> 
    mutate(nchar_x = nchar(txt_linea.x),
           nchar_y = nchar(txt_linea.y)) |> 
    mutate(char_dist = nchar_y/nchar_x) |> 
    filter(char_dist <= 1.3,
           char_dist >= 0.7) |> 
    select(-char_dist, -nchar_x, -nchar_y)
  
  remove(liras_pre_comparacion)
  
  #medir distancia ----
  message("midiendo distancia...")
  
    liras_comparadas <- liras_pre_comparacion_2 |> 
      #comparar
      mutate(distancia = stringdist::stringsim(txt_linea.x, txt_linea.y)) |> 
      select(-starts_with("txt")) |> 
      #definir distancia
      filter(distancia >= 0.6)
  
  remove(liras_pre_comparacion_2) #limpiar
  
  return(liras_comparadas)
})

beepr::beep(1)



# terminar proceso ----
comparacion_liras_2 <- comparacion_liras |>
  bind_rows() |> 
  arrange(desc(distancia)) |>
  #poner línea de origen
  left_join(liras_linea |> 
              select(id, n_linea,
                     txt_linea.x = txt_linea,
                     autor.x = autor),
            by = c("id.x" = "id", "n_linea.x" = "n_linea")) |>
  left_join(liras_linea |> 
               select(id, n_linea,
                      txt_linea.y = txt_linea,
                      autor.y = autor),
            by = c("id.y" = "id", "n_linea.y" = "n_linea")) |> 
  filter(!(distancia == 1 & autor.x == autor.y))


# liras_linea |> filter(id == 100) |> select(autor, titulo, txt_cuerpo) |> pull(txt_cuerpo) |> unique()
# liras_linea |> filter(id == 317) |> select(autor, titulo, txt_cuerpo) |> pull(txt_cuerpo) |> unique()

#guardar----
arrow::write_parquet(comparacion_liras_2, "datos/lira_comparacion.parquet")
arrow::read_parquet("datos/lira_comparacion.parquet")
