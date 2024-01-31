library(dplyr)
library(here)

#cargar todas las comparaciones y unirlas
comparadas_0 <- bind_rows(
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_1.parquet") |> 
    mutate(archivo = 1),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_2.parquet") |> 
    mutate(archivo = 2),
  # arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_3.parquet") |> 
  #   mutate(archivo = 3),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_4.parquet") |> 
    mutate(archivo = 4),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_5.parquet") |> 
    mutate(archivo = 5),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_6.parquet") |> 
    mutate(archivo = 6),
  # arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_7.parquet") |> 
  #   mutate(archivo = 7),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_8.parquet") |> 
    mutate(archivo = 8),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_9.parquet") |> 
    mutate(archivo = 9),
  # arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_10.parquet") |> 
  #   mutate(archivo = 10),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_11.parquet") |> 
    mutate(archivo = 11),
  arrow::read_parquet("~/Otros/lira_popular/liras_comparacion_lineas_12.parquet") |> 
    mutate(archivo = 12)
)

# comparadas_0 |>
#   distinct(id_lira.y) |> 
#   ggplot2::ggplot(ggplot2::aes(id_lira.y)) +
#   ggplot2::geom_histogram(binwidth = 100)

comparadas_0 |> 
  distinct(id_lira.x, n_linea.x, id_lira.y, n_linea.y, 
           .keep_all = T)

#guardar
arrow::write_feather(comparadas_0,
                     "lira_popular_comparaciones/liras_comparadas_1.feather")

#ver 
comparadas_0 |> 
  group_by(archivo) |> 
  summarize(min(id_lira.x),
            max(id_lira.x)) |> 
  print(n=Inf)

#7 10 sobran
#faltan de 1501:2000 y de 4001 en adelante

#liras por palabra ----
liras_palabra <- arrow::read_feather(here("liras_palabra_2.feather"))

#arrow::write_feather(liras_palabra, "lira_popular/liras_palabra.feather")

#liras por línea ----
liras_linea <- liras_palabra |> 
  select(-txt_palabra, -n_palabra) |> 
  distinct()

#arrow::write_feather(liras_linea, "lira_popular/liras_linea.feather")

#lista de liras ----
lista_liras <- arrow::read_feather(here("liras_palabra_2.feather")) |> 
  select(-txt_palabra, -n_palabra) |> 
  select(autor, titulo, id_lira) |> 
  distinct() |> 
  arrange(id_lira) |> 
  na.omit()
#agregar a analizar.r para poder cargarlo preprocesado

#arrow::write_feather(lista_liras, "lira_popular_comparaciones/lista_liras.feather")



#—----

lista_liras

lira_elegida = 3751
sensibilidad = 0.65

#filtrar ----
lira_info <- liras_linea |> 
  filter(id_lira == lira_elegida) |> 
  select(autor, fecha, tipo, url) |> 
  distinct()

lira_origen <- liras_linea |> 
  filter(id_lira == lira_elegida) |> 
  select(id_lira, n_linea, txt_linea)

lira_analisis <- comparadas_0 |> 
  #elegir lira por su id
  filter(id_lira.x == lira_elegida) |> 
  #filtrar liras del mismo autor
  filter(!(autor.x == autor.y)) |> 
  #filtrar sensibilidad
  #filter(distancia >= sensibilidad) |> 
  #eliminar duplicados
  distinct(txt_linea.x, autor.x, txt_linea.y, autor.y,
           .keep_all = T) |>
  print(n=20)


#coincidencias por línea ----
#la linea 1 aparece 4 veces, la 2 aparece 8 veces, la 40 x veces
c_por_lineas <- lira_analisis |> 
  #filtrar sensibilidad
  filter(distancia >= sensibilidad) |> 
  arrange(n_linea.x) |> 
  group_by(id_lira.x, n_linea.x, txt_linea.x) |> 
  count() |> 
  ungroup() |> 
  print()

#graficar texto con popularidad al lado, destacar las líneas
lira_origen |> 
  left_join(c_por_lineas |> select(n_linea = n_linea.x, n)) |> 
  select(-id_lira) |> 
  mutate(n = tidyr::replace_na(n, 0))

#la línea más popular es: linea 42
c_linea_popular <- c_por_lineas |> 
  arrange(desc(n))

#c_linea_popular$n_linea.x

# #mostrar lineas más populares 
# liras_linea |> 
#   filter(id_lira == lira_elegida,
#          n_linea %in% c_linea_popular$n_linea.x[1:5]) |> 
#   select(txt_linea)
# #graficar texto entero y destacar las líneas


#mayores copiones ----
c_copiones <- lira_analisis |> 
  #filtrar sensibilidad
  filter(distancia >= sensibilidad) |> 
  count(autor.y) |> 
  arrange(desc(n)) |> 
  print()



#elegir línea y ver coincidencias ----
#para el selector de lineas
lira_origen
# lira_analisis |> 
#   select(n_linea.x, txt_linea.x) |> 
#   distinct() |> 
#   arrange(n_linea.x)


#desde el selector de lineas
lira_analisis |> 
  filter(n_linea.x == 8) |> 
  #filtrar sensibilidad
  filter(distancia >= sensibilidad) |> 
  select(txt_linea.y, autor.y, distancia)
