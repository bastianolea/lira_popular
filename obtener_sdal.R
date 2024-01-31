# SDAL Spanish DAL: Diccionario de afectos en español
# https://liaa.dc.uba.ar/es/software-datos/ #enlace roto
# https://github.com/abcsds/sdal

# idea de https://rpubs.com/HAVB/tangos

library(dplyr)
library(stringr)

sdal <- readr::read_csv2("https://github.com/abcsds/sdal/raw/master/data/sdal.csv")

# obj: weather word is noun, verb, or adjective
# pleasure: mean pleasantness
# activation: mean activation
# imagination: mean imagery
# p_sdev: pleasantness standard deviation
# a_sdev: activation standard deviation
# i_sdev: imagery standard deviation

sdal_2 <- sdal |> 
  janitor::clean_names() |> 
  rename(word = word_o) |> 
  mutate(type = str_extract(word, "_\\w{1}$"),
         type = str_remove(type, "_")) |> 
  mutate(word = str_remove(word, "_\\w{1}$"))

#guardar ----
readr::write_rds(sdal_2, "datos/sdal_diccionario_afectos_español.rds")

#mejorar ----
#aumentar conjugaciones de palabras
sdal_2 |> filter(type == "V") |> arrange(word)
# https://preply.com/en/blog/spanish-conjugations-guide/



