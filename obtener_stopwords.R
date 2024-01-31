# stopwords
# http://snowball.tartarus.org/algorithms/spanish/stop.txt
library(dplyr)
library(stringr)

#fuente A
stop_a <- readr::read_lines("http://snowball.tartarus.org/algorithms/spanish/stop.txt", 
                            locale = readr::locale(encoding = "latin1"))

stop_a_2 <- stop_a |> 
  str_remove("\\|.*$") |> #eliminar comentarios
  str_squish() |> #borrar espacios extra
  str_subset(".") #eliminar palabras vacías

#fuente B
stop_b <- readr::read_lines("https://github.com/stopwords-iso/stopwords-es/raw/master/stopwords-es.txt")

#fuente C
stop_c <- readr::read_lines("https://github.com/Alir3z4/stop-words/raw/master/spanish.txt")

#unir ----
stop_unidas <- c(stop_a_2,
                 stop_b,
                 stop_c) |> 
  unique() |> 
  sort()

stop_unidas_sin_tilde <- stop_unidas |> 
  str_replace("á", "a") |> 
  str_replace("é", "e") |> 
  str_replace("í", "i") |> 
  str_replace("ó", "o") |> 
  str_replace("ú", "u")

stop_unidas_2 <- c(stop_unidas, stop_unidas_sin_tilde) |> unique() |> sort()


#guardar ----
readr::write_lines(stop_unidas_2, "datos/stopwords_español.txt")
