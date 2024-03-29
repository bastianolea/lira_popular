# SDAL Spanish DAL: Diccionario de afectos en español
# https://liaa.dc.uba.ar/es/software-datos/ #enlace roto
# https://github.com/abcsds/sdal

# idea de https://rpubs.com/HAVB/tangos

library(dplyr)
library(stringr)
library(purrr)
library(jsonlite)
library(glue)
library(tidyr)

source("funciones.R")

# descargar sdal ----
# https://github.com/abcsds/sdal
# download.file("https://github.com/abcsds/sdal/raw/master/data/sdal.json",
#               "datos/sdal.json")

# cargar datos descargados
sdal <- jsonlite::fromJSON("datos/sdal.json", 
                           simplifyDataFrame = TRUE, flatten = TRUE)

# convertir json a dataframe
sdal_1 <- map_df(1:length(sdal), ~{
  word <- names(sdal)[[.x]]
  datos <- sdal[[.x]] |> as_tibble()
  datos$word <- word
  
  return(datos)
})


# obj: weather word is noun, verb, or adjective
# pleasure: mean pleasantness
# activation: mean activation
# imagination: mean imagery
# p_sdev: pleasantness standard deviation
# a_sdev: activation standard deviation
# i_sdev: imagery standard deviation

sdal_2 <- sdal_1 |> 
  select(word, obj, 
         s_activation = activation, s_imagination = imagination, s_pleasantness = pleasantness, 
         everything()) |> 
  mutate(across(3:8, as.numeric)) |> 
  arrange(word)

#guardar ----
readr::write_rds(sdal_2, "datos/sdal_diccionario_afectos_español.rds")



# expandir ----
# aumentar conjugaciones de palabras, dado que verbos de sdal vienen solo en infinitivo

sdal_verbos <- sdal_2 |> 
  rename(palabra = word, tipo = obj) |> 
  filter(tipo == "V") |> 
  select(verbo = palabra) |> 
  mutate(verbo_ar = str_detect(verbo, "ar$"),
         verbo_er = str_detect(verbo, "er$"),
         verbo_ir = str_detect(verbo, "ir$")) |> 
  mutate(terminacion = str_extract(verbo, "ar$|er$|ir$"),
         raiz = str_remove(verbo, terminacion)) |> 
  mutate(irregular = ifelse(verbo %in% irregulares, TRUE, FALSE))

#https://www.rae.es/dpd/ayuda/modelos-de-conjugacion-verbal

sdal_verbos |> 
  filter(irregular)

sdal_verbos_expandidos <- sdal_verbos |> 
  filter(!irregular) |> 
  select(verbo, starts_with("verbo"), raiz) |> 
  #modo indicativo, tiempos simples
  mutate(
    var_p1_indicativo_simp_presente = case_when(verbo_ar ~ glue("{raiz}o"), #amar
                                                verbo_er ~ glue("{raiz}o"), #temer
                                                verbo_ir ~ glue("{raiz}o")), #partir
    var_p1_indicativo_simp_pretimpe = case_when(verbo_ar ~ glue("{raiz}aba"),
                                                verbo_er ~ glue("{raiz}ía"),
                                                verbo_ir ~ glue("{raiz}ía")),
    var_p1_indicativo_simp_pretperf = case_when(verbo_ar ~ glue("{raiz}é"),
                                                verbo_er ~ glue("{raiz}í"),
                                                verbo_ir ~ glue("{raiz}í")),
    var_p1_indicativo_simp_futuro = case_when(verbo_ar ~ glue("{raiz}aré"),
                                              verbo_er ~ glue("{raiz}eré"),
                                              verbo_ir ~ glue("{raiz}iré")),
    var_p1_indicativo_simp_condic = case_when(verbo_ar ~ glue("{raiz}aría"),
                                              verbo_er ~ glue("{raiz}ería"),
                                              verbo_ir ~ glue("{raiz}iría")),
    #segunda persona
    var_p2_indicativo_simp_presente = case_when(verbo_ar ~ glue("{raiz}as"),
                                                verbo_er ~ glue("{raiz}es"),
                                                verbo_ir ~ glue("{raiz}es")),
    var_p2_indicativo_simp_pretimpe = case_when(verbo_ar ~ glue("{raiz}abas"),
                                                verbo_er ~ glue("{raiz}ías"),
                                                verbo_ir ~ glue("{raiz}ías")),
    var_p2_indicativo_simp_pretperf = case_when(verbo_ar ~ glue("{raiz}aste"),
                                                verbo_er ~ glue("{raiz}iste"),
                                                verbo_ir ~ glue("{raiz}iste")),
    var_p2_indicativo_simp_futuro = case_when(verbo_ar ~ glue("{raiz}arás"),
                                              verbo_er ~ glue("{raiz}erás"),
                                              verbo_ir ~ glue("{raiz}irás")),
    var_p2_indicativo_simp_condic = case_when(verbo_ar ~ glue("{raiz}arías"),
                                              verbo_er ~ glue("{raiz}erías"),
                                              verbo_ir ~ glue("{raiz}irías")),
    #tercera persona
    var_p3_indicativo_simp_presente = case_when(verbo_ar ~ glue("{raiz}a"),
                                                verbo_er ~ glue("{raiz}e"),
                                                verbo_ir ~ glue("{raiz}e")),
    var_p3_indicativo_simp_pretimpe = case_when(verbo_ar ~ glue("{raiz}aba"),
                                                verbo_er ~ glue("{raiz}ía"),
                                                verbo_ir ~ glue("{raiz}ía")),
    var_p3_indicativo_simp_pretperf = case_when(verbo_ar ~ glue("{raiz}ó"),
                                                verbo_er ~ glue("{raiz}ió"),
                                                verbo_ir ~ glue("{raiz}ió")),
    var_p3_indicativo_simp_futuro = case_when(verbo_ar ~ glue("{raiz}ará"),
                                              verbo_er ~ glue("{raiz}erá"),
                                              verbo_ir ~ glue("{raiz}irá")),
    var_p3_indicativo_simp_condic = case_when(verbo_ar ~ glue("{raiz}aría"),
                                              verbo_er ~ glue("{raiz}ería"),
                                              verbo_ir ~ glue("{raiz}iría")),
  ) |> 
  #modo indicativo, tiempos compuestos
  mutate(
    var_p1_indicativo_compuesto = case_when(verbo_ar ~ glue("{raiz}ado"),
                                            verbo_er ~ glue("{raiz}ido"),
                                            verbo_ir ~ glue("{raiz}ido")),
  ) |> 
  #modo subjuntivo, tiempos simples
  mutate(
    var_p1_subjuntivo_simp_presente = case_when(verbo_ar ~ glue("{raiz}e"),
                                                verbo_er ~ glue("{raiz}a"),
                                                verbo_ir ~ glue("{raiz}a")),
    var_p1_subjuntivo_simp_pretimpe = case_when(verbo_ar ~ glue("{raiz}ara"),
                                                verbo_er ~ glue("{raiz}iese"),
                                                verbo_ir ~ glue("{raiz}iese")),
    var_p1_subjuntivo_simp_futuro = case_when(verbo_ar ~ glue("{raiz}are"),
                                              verbo_er ~ glue("{raiz}iere"),
                                              verbo_ir ~ glue("{raiz}iere")),
  ) |> 
  glimpse()

sdal_2_expandida <- sdal_verbos_expandidos |> 
  pivot_longer(cols = starts_with("var"), values_to = "verbo_conjugado") |> 
  select(verbo, verbo_conjugado) |> 
  left_join(sdal_2, by = c("verbo" = "word"), relationship = "many-to-many") |> 
  select(-verbo) |> 
  rename(word = verbo_conjugado)

sdal_3 <- bind_rows(sdal_2, sdal_2_expandida) |> 
  arrange(word) |> 
  distinct(word, .keep_all = T) |> 
  mutate(word = as.character(word))


#guardar ----
readr::write_rds(sdal_3, "datos/sdal_diccionario_afectos_español_expandido.rds")
