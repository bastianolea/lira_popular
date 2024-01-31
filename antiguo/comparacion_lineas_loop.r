library(dplyr)
library(stringr)
library(stringdist)
library(purrr)
library(here)

here("~/Otros/lira_popular/analizar.r")

#cargar ----
liras_palabra <- arrow::read_feather(here("liras_palabra_2.feather"))

#por línea
liras_linea <- liras_palabra |> 
  select(-txt_palabra, -n_palabra) |> 
  distinct()

#limpiar texto para comparar
liras_linea_2 <- liras_linea |> 
  select(id_lira, autor, n_linea, txt_linea) |> 
  #limpiar lineas para comparación
  mutate(txt_linea = txt_linea |> 
           tolower() |> 
           textclean::replace_non_ascii() |> 
           str_remove_all("\\.|\\,|\\¿|\\?|\\!|\\¡|\\*|\\+|\\-|\\;|\\[|\\]|\\(|\\)")
  )

#lista de liras ----
max(liras_linea$id_lira)

lista_liras <- liras_linea |> 
  select(autor, titulo, id_lira) |> 
  distinct() |> 
  arrange(id_lira) |> 
  #slice(1:1999)
  #slice(c(1501:2000, 4001:6152))
  #slice(1501:2000)
  #slice(c(5001:5903, 5905:6000))
  slice(6001:6152)
  #faltan de 1501:2000 y de 4001 en adelante

#error en 5001 a 6000: id_lira 5904: “Romance ilustrado sobre el combate naval de iquique el 21 de mayo de 1879”,


#loop ----
#resultados_liras <- map_df(lista_liras$id_lira |> sample(2), ~{
resultados_liras <- map_df(lista_liras$id_lira, ~{
  x_lira <- .x
  #x_lira <- 345

  x_meta <- lista_liras |> 
    filter(id_lira == x_lira)
  
  glue::glue("id_lira {x_lira}: “{x_meta |> pull(titulo) |> str_to_sentence()}”, de {x_meta |> pull(autor)}") |> message()
  
#unir liras ----
  message("uniendo...")
liras_l_join <- liras_linea_2 |> 
  filter(id_lira == x_lira) |> #lira a comparar
  select(-autor) |> 
  mutate(join = 1) |> 
  #unión
  left_join(liras_linea_2 |> 
              #excluir la misma lira y las del mismo autor
              filter(id_lira != x_lira) |> 
                     #autor != x_autor) |> 
              select(-autor) |> 
              mutate(join = 1),
            by = "join") |> 
  select(-join)

#remove(x_autor)

#medir distancia ----
message("midiendo distancia...")

tryCatch({
tictoc::tic()
liras_l_join_c <- liras_l_join |> 
  #comparar
  mutate(distancia = stringdist::stringsim(txt_linea.x, txt_linea.y)) |> 
  select(-starts_with("txt")) |> 
  #definir distancia
  filter(distancia >= 0.6)
tictoc::toc()
}, error = function(err) { 
  message("Error en ", x_lira, err) 
  return(NULL) 
  }
)

remove(liras_l_join) #limpiar

# #comparación explícita
liras_l_join_c_2 <- liras_l_join_c |>
  arrange(desc(distancia)) |>
  #poner línea de origen
  left_join(liras_linea |> select(id_lira, n_linea,
                                  txt_linea.x = txt_linea,
                                  autor.x = autor),
            by = c("id_lira.x" = "id_lira", "n_linea.x" = "n_linea")) |>
  left_join(liras_linea |> select(id_lira, n_linea,
                                  txt_linea.y = txt_linea,
                                  autor.y = autor),
            by = c("id_lira.y" = "id_lira", "n_linea.y" = "n_linea"))

remove(liras_l_join_c)

return(liras_l_join_c_2)
})

#guardar----
resultados_liras |> arrow::write_parquet(here("liras_comparacion_lineas_12.parquet"))

#resultados_liras