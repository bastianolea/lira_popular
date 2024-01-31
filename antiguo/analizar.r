library(dplyr)
library(stringr)

#liras_palabra <- arrow::read_feather("liras_palabra_2.feather")

liras_linea <- liras_palabra |> 
  select(-txt_palabra, -n_palabra) |> 
  distinct()

#palabras más frecuentes
liras_palabra_2 |> 
  count(txt_palabra) |> 
  arrange(desc(n)) |> 
  print(n=30)


#autores ----
autores <- liras_palabra_2$autor |> unique() |> sort()

autores

#tipo ----
tipo <- liras_palabra_2$tipo |> unique() |> sort()


#distancia entre líneas ----

#buscar liras de José Arroyo
liras_linea |> 
  filter(autor == "Arroyo José") |> 
  select(autor, titulo, id_lira) |> 
  distinct()

liras_linea_2 <- liras_linea |> 
  select(id_lira, n_linea, txt_linea) |> 
  #limpiar lineas para comparación
  mutate(txt_linea = txt_linea |> 
           tolower() |> 
           textclean::replace_non_ascii() |> 
           str_remove_all("\\.|\\,|\\¿|\\?|\\!|\\¡|\\*|\\+|\\-|\\;")
         )

#iniciar comparación entre cada línea de una lira y todas las líneas de todas las liras
liras_l_join <- liras_linea_2 |> 
  filter(id_lira == 938) |> #lira a comparar
  mutate(join = 1) |> 
  #unión
  left_join(liras_linea_2 |> 
              filter(id_lira != 938) |> #todas menos la a comparar
              mutate(join = 1),
            by = "join")

#medir distancia
tictoc::tic()
liras_l_join_c <- liras_l_join |> 
  mutate(distancia = stringdist::stringsim(txt_linea.x, txt_linea.y)) |> 
  select(-starts_with("txt"), -join) |> 
  #definir distancia
  filter(distancia >= 0.7)
tictoc::toc()

#comparación explícita
liras_l_join_c |> 
  arrange(desc(distancia)) |> 
  #poner línea de origen
  left_join(liras_linea |> select(id_lira, n_linea, txt_linea.x = txt_linea, autor.x = autor),
            by = c("id_lira.x" = "id_lira", "n_linea.x" = "n_linea")) |> 
  left_join(liras_linea |> select(id_lira, n_linea, txt_linea.y = txt_linea, autor.y = autor),
            by = c("id_lira.y" = "id_lira", "n_linea.y" = "n_linea")) |> 
  filter(autor.y != "Arroyo José") |> 
  print(n=Inf)
#txt_linea.x es la línea a comparar, "y" son las demás liras

liras_linea |> 
  filter(id_lira == 3626,
         n_linea == 2) |> 
  pull(txt_linea)

liras_linea |> 
  filter(id_lira == 100,
         n_linea == 25) |> 
  pull(txt_linea)



